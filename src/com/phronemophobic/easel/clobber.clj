(ns com.phronemophobic.easel.clobber
  (:require
   [membrane.component :refer
    [defui
     defeffect]]
   [membrane.ui :as ui]
   [membrane.alpha.component.drag-and-drop :as dnd]
   [com.phronemophobic.easel.model :as model]
   [membrane.basic-components :as basic]
   [membrane.skia.paragraph :as para]
   [clojure.core.async :as async]
   [clojure.set :as set]
   [clojure.string :as str]
   [com.phronemophobic.easel.splitpane :as splitpane]
   [com.phronemophobic.viscous :as viscous]
   [com.phronemophobic.clobber.modes.clojure.ui :as cui]
   [com.phronemophobic.clobber.editor :as clobber-editor]  
   [nextjournal.beholder :as beholder]
   [clojure.zip :as z]

   [com.phronemophobic.clobber.modes.clojure :as clojure-mode]
   [com.phronemophobic.clobber.modes.text :as text-mode]
   [com.phronemophobic.clobber.util.ui.key-binding :as key-binding]

   [com.phronemophobic.membrandt :as ant]))


(defui buffer-selector [{:keys [base-style
                                buffer-select-state
                                focused?
                                width]
                         :as this}]
  (let [
        base-style (or base-style 
                       #:text-style
                       {:font-families ["Menlo"]
                        :font-size 12
                        :height 1.2
                        :height-override true})

        applets (:applets buffer-select-state)
        offset (get buffer-select-state :offset 0)
        search-str (get buffer-select-state :search-str "")
        search-str-lower (str/lower-case search-str)

        matching-applets
        (into 
         []
         (comp (filter (fn [applet]
                         (str/includes? (str/lower-case (or (:label applet) ""))
                                        search-str-lower)))
               (drop offset))
         applets)

        ps (into 
            ["Applet: "
             search-str
             " | "]
            (comp (map-indexed 
                   (fn [i applet]
                     (let [style (if (zero? i)
                                   (assoc base-style 
                                          :text-style/font-style
                                          {:font-style/weight :bold})
                                   base-style)]
                       {:style style
                        :text (:label applet)})))
                  (interpose " | "))
            matching-applets)

        p (para/paragraph
           ps
           width
           {:paragraph-style/text-style base-style})
        
        p (if focused?
            (ui/on
             :key-event
             (fn [key scancode action mods]
               (when (#{:press :repeat} action)
                 (let [ctrl? (not (zero? (bit-and ui/CONTROL-MASK mods)))]
                   (cond
                     (and ctrl?
                          (= (char key) \G))
                     [[:set $buffer-select-state nil]]

                     
                     (and ctrl?
                          (= (char key) \S))
                     [[:update $offset 
                       (fn [offset]
                         (if (>= offset (count matching-applets))
                           0
                           (inc offset)))]]))))
             
             :key-press
             (fn [s]
               (cond
                 
                 (= s :enter)
                 (let [applet (first matching-applets)
                       applet-id (:id applet)
                       focus (:focus context)]
                   [[:com.phronemophobic.easel/set-pane-applet-id {:applet-id applet-id}]
                    [:set $buffer-select-state nil]
                    [:set $focus applet-id]])
                 
                 (= s :backspace)
                 [[:update $search-str
                   (fn [s]
                     (subs s 0 (max 0 (- (count s) 1))))]
                  [:set $offset 0]]
                 
                 (string? s)
                 [[:update $search-str str s]
                  [:set $offset 0]]))
             p)
            ;; else
            p)]
    p))

(declare clobber-applet)
(defui clobber-ui* [{:keys [this]}]
  (let [focus (:focus context)
        state (:state this)
        editor (:editor state)
        focused? (= (:id this) focus)]
    (when editor
      (let [buffer-select-state (::buffer-select-state this)
            body (ui/on
                  ::show-select-buffer
                  (fn [m]
                    [[::show-select-buffer (assoc this
                                                  :$focus $focus)]])
                  ::hide-pane
                  (fn [m]
                    [[::focus-next {:this this
                                    :$focus $focus}]
                     [:com.phronemophobic.easel/hide-pane {}]])
                  ::delete-pane
                  (fn [m]
                    [[::focus-next {:this this
                                    :$focus $focus}]
                     [:com.phronemophobic.easel/delete-pane {}]])
                  ::close-other-panes
                  (fn [m]
                    [[:com.phronemophobic.easel/close-other-panes {}]])
                  ::split-pane
                  (fn [{:keys [editor]}]
                    (let [forked-editor (-> editor
                                            (dissoc ::cui/auto-reload-unwatch)
                                            (update :tree
                                                    (fn [^org.treesitter.TSTree tree]
                                                      (when tree
                                                        (.copy tree)))))]
                      [[:com.phronemophobic.easel/add-applet
                        {:make-applet
                         #(clobber-applet % {:editor forked-editor
                                             :label (str (:label editor) "*")
                                             :ui (:ui this)})}]]))
                  ::focus-next
                  (fn [m]
                    [[::focus-next {:this this
                                    :$focus $focus}]])
                  ::cui/request-focus
                  (fn []
                    [[:set $focus (:id this)]])
                  (let [ui (:ui this)
                        extra (:extra state)]
                    (ui {:editor editor
                         :$editor $editor
                         :focused? focused?
                         :extra extra
                         :$extra $extra})))
            body (if buffer-select-state
                   (ui/on
                    :mouse-down
                    (fn [_]
                      [[:set $focus (:id this)]])
                    (ui/vertical-layout
                     (ui/no-events body)
                     (let [e (:extra state)]
                       (buffer-selector {:buffer-select-state buffer-select-state
                                         :extra (get e ::buffer-selector)
                                         :focused? focused?}))))
                   body)]
        body))))

(defn clobber-ui [this $context context]
  (clobber-ui* {:this this
                :$this [(:$ref this)]
                :context context
                :$context $context}))


(defn load-editor [dispatch! $ref editor-info size]
  (let [
        height (nth size 1)
        
        {:keys [editor ui]} (if (:editor editor-info)
                              editor-info
                              (clobber-editor/guess-mode editor-info))

        editor (if-let [line (:line editor-info)]
                 (text-mode/editor-goto-line editor line)
                 editor)

        editor (assoc editor
                      :label (:label editor-info)
                      :key-bindings
                      (assoc (:key-bindings editor)
                             "C-x 3" ::split-pane
                             "C-x k" ::delete-pane
                             "C-x b" ::show-select-buffer
                             "C-x o" ::focus-next
                             "C-x 1" ::close-other-panes
                             "C-x 0" ::hide-pane))

        editor (-> editor
                   (cui/editor-set-height height)
                   (text-mode/editor-update-viewport))
        $editor [$ref '(keypath :state) '(keypath :editor)]]
    (dispatch!
     :update
     $ref
     (fn [applet]
       (-> applet
           (assoc :ui ui)
           (assoc-in [:state :editor] editor)
           (assoc-in [:state :$editor] $editor))))
    (dispatch! ::cui/auto-reload-file
               {:editor editor
                :$editor $editor})))

(defrecord ClobberApplet [dispatch! editor-info]
  model/IApplet
  (-start [this $ref size _content-scale]
    (let [
          ]
      (assoc this
             :extra {}
             :tap-vals []
             :$ref $ref
             :size size
             ::model/queue
             [(fn []
                (load-editor dispatch! $ref editor-info size)
                (dispatch! :repaint!))])))
  (-stop [this]
    (dispatch! ::cui/auto-reload-file-unwatch
               (:state this))
    nil)
  model/IUI
  (-ui [this $context context]
    (clobber-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (let [height (nth size 1)]
      (-> this
          (assoc :size size)
          (update-in [:state :editor] cui/editor-set-height height)))))

(defn ^:private truncate-string-end [s n]
  (if (> (count s) n)
    (subs s 0 n)
    s))

(defn ^:private truncate-string-begin [s n]
  (if (> (count s) n)
    (subs s (- (count s) n))
    s))



(defn clobber-applet [handler {:keys [file ns string label] :as m}]
  (let [name (cond
               label label

               file
               (-> (.getCanonicalPath file)
                   (truncate-string-begin 16))
               
               ns
               (-> (str ns)
                   (truncate-string-begin 16))
               
               :else "Clobber")
        m (assoc m :label name)]
    (-> (->ClobberApplet handler m)
      (assoc :label (str name)))))


(defeffect ::show-select-buffer [{:keys [$ref $focus id]}]
  (let [applets (dispatch! :com.phronemophobic.easel/get-applets)
        clobber-applets (into []
                              (keep (fn [[id applet]]
                                      (when (instance? ClobberApplet applet)
                                        applet)))
                              applets)]
    (dispatch! :update $ref assoc
               ::buffer-select-state {:applets clobber-applets})
    (dispatch! :set $focus id)))


(defn ^:private zfind
  "Finds first loc that matches pred. Returns nil if no match found."
  [loc pred]
  (loop [loc loc]
    (if (z/end? loc)
      nil
      (if (pred (z/node loc))
        loc
        (recur (z/next loc))))))

(defeffect ::focus-next [{:keys [this $focus]}]
  (let [applets (dispatch! :com.phronemophobic.easel/get-applets)
        root-pane (dispatch! :com.phronemophobic.easel/get-root-pane)
        applet-id (:id this)
        
        zpane (zfind (splitpane/pane-zip root-pane)
               #(= applet-id (:applet-id %)))
        clobber-pane? (fn [pane]
                        (let [applet-id (:applet-id pane)
                              applet (get applets applet-id)]
                          (instance? ClobberApplet applet)))
        next-clobber-pane (loop [loc (z/next zpane)]
                            (cond 
                              (= zpane loc) nil

                              (z/end? loc)
                              ;; start search from beginning
                              (recur (splitpane/pane-zip (z/root loc)))
                              
                              (clobber-pane? (z/node loc)) (z/node loc)
                                    
                              :else (recur (z/next loc))))]
    (when next-clobber-pane
      (dispatch! :set $focus (:applet-id next-clobber-pane)))))

