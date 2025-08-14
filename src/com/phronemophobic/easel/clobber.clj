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
   [com.phronemophobic.viscous :as viscous]
   [com.phronemophobic.clobber.modes.clojure.ui :as cui]
   [nextjournal.beholder :as beholder]

   [com.phronemophobic.clobber.modes.clojure :as clojure-mode]
   [com.phronemophobic.clobber.modes.text :as text-mode]
   [com.phronemophobic.clobber.util.ui.key-binding :as key-binding]

   [com.phronemophobic.membrandt :as ant]))


;; :context
;; {:editors {id {:name name
;;                :as editor-state}}}

;; store state in applet?
;; C-x b let's you select between existing applets

;; need to be able to override key bindings
;; split panes
;; switch to other pane , ie. "C-x o"
;; text search, undo
;; line numbers



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

        matching-applets
        (into 
         []
         (comp (filter (fn [applet]
                         (str/includes? (or (:label applet) "")
                                        search-str)))
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
                    [[:com.phronemophobic.easel/hide-pane {}]])
                  ::cui/request-focus
                  (fn []
                    [[:set $focus (:id this)]])
                  (cui/code-editor
                   {:editor editor
                    :focused? focused?
                    :extra (:extra state)}))
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
        editor (cond
                 (:ns editor-info) (cui/make-editor-from-ns (:ns editor-info))
                 (:file editor-info) (cui/make-editor-from-file (:file editor-info))
                 (:string editor-info) (-> (cui/make-editor)
                                           (text-mode/editor-set-string (:string editor-info)))
                 :else (cui/make-editor))

        editor (if-let [line (:line editor-info)]
                 (text-mode/editor-goto-line editor line)
                 editor)

        editor (assoc editor
                      :key-tree
                      (key-binding/key-bindings->key-tree
                       (assoc cui/clojure-key-bindings
                              "C-x b" ::show-select-buffer
                              "C-x 0" ::hide-pane)))

        editor (-> editor
                   (cui/editor-set-height height)
                   (text-mode/editor-update-viewport))
        $editor [$ref '(keypath :state) '(keypath :editor)]]
    (dispatch!
     :update
     $ref
     (fn [applet]
       (-> applet
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
                (load-editor dispatch! $ref editor-info size))])))
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



(defn clobber-applet [handler {:keys [file ns string] :as m}]
  (let [name (cond
               
               file
               (-> (.getCanonicalPath file)
                   (truncate-string-begin 16))
               
               ns
               (-> (str ns)
                   (truncate-string-begin 16))
               
               :else "Clobber")]
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


