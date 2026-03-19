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
   [com.phronemophobic.clobber.util.ui :as clobber.util.ui]
   [com.phronemophobic.clobber.util.ui.key-binding :as key-binding]
   [com.phronemophobic.easel :as-alias easel]

   [com.phronemophobic.membrandt :as ant]))

(defeffect ::show-tap-watcher [{}]
  (dispatch!
   :com.phronemophobic.easel/add-applet
   {:id @(requiring-resolve 'com.phronemophobic.easel.tap-watcher/id)
    :make-applet
    (fn [handler]
      ((requiring-resolve 'com.phronemophobic.easel.tap-watcher/tap-watcher-applet)
       handler))}))


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
                         (if (> (count matching-applets) 1)
                           (inc offset)
                           0))]]))))
             
             :key-press
             (fn [s]
               (cond
                 
                 (= s :enter)
                 (let [applet (first matching-applets)
                       applet-id (:id applet)
                       focus (:focus context)]
                   [[:com.phronemophobic.easel/set-pane-applet-id {:applet-id applet-id}]
                    [:set $buffer-select-state nil]
                    [::make-editor-active {:$editor (:$editor applet)
                                           :id applet-id}]])
                 
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

(def ^:private inactive-keys
  #{:width
    :height
    :viewport
    :cursor})
(defn ^:private make-active [editor id]
  (let [last-active-id (::active-id editor)]
    (if (= last-active-id id)
      editor
      (let [last-editor editor
            editor (assoc editor
                          ::active-id id)
            editor (if-let [inactive-info (-> editor
                                              :inactive-view
                                              (get id))]
                     (let [editor (-> editor
                                      (update :inactive-view dissoc id))
                           
                           old-cursor (:cursor inactive-info)
                           editor (-> editor
                                      (text-mode/editor-goto-row-col (:row old-cursor)
                                                                     (:column-byte old-cursor)))
                           
                           old-viewport (:viewport inactive-info)
                           editor (-> editor
                                      (assoc-in [:viewport :start-line]
                                                (-> old-viewport :start-line))
                                      (text-mode/editor-update-viewport))]
                       editor)
                     
                     editor)
            editor (if last-active-id
                     (update editor :inactive-view
                             assoc last-active-id (select-keys last-editor inactive-keys))
                     editor)]
        editor))))

(defeffect ::make-editor-active [{:keys [$editor id] :as m}]
  (dispatch! :update $editor make-active id)
  (dispatch! ::easel/request-focus id))

(declare clobber-applet)
(defui clobber-ui* [{:keys [this shared]}]
  (let [focus (:focus context)
        state (:state this)
        editors (::editors shared)
        editor (get editors (::editor-id this))
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
                    [[::make-editor-active {:$editor $editor
                                            :id (:id this)}]])
                  (let [ui (:ui this)
                        extra (:extra state)
                        $editor $editor
                        editor (if (not= (::active-id editor)
                                         (:id this))
                                 (if-let [m (-> editor
                                                :inactive-view
                                                (get (:id this)))]
                                   (merge editor m)
                                   editor)
                                 editor)]
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

(defn clobber-ui [this ui-info]
  (clobber-ui* {:this this
                :$this [(:$ref this)]
                :shared (:shared ui-info)
                :$shared (:$shared ui-info)
                :context (:context ui-info)
                :$context (:$context ui-info)}))


(defn load-editor [{:keys [dispatch! id $ref editor-info size shared $shared $focus]}]
  (let [height (nth size 1)
        {:keys [editor ui]} (or
                             (when (:editor editor-info)
                               (update-in editor-info [:editor ::id]
                                          (fn [id]
                                            (or id (random-uuid)))))
                             
                             (when-let [editor-id (or (when-let [f (:file editor-info)]
                                                        (.getCanonicalFile f))
                                                      (:ns editor-info))]
                               (when-let [editor (-> shared
                                                     ::editors
                                                     (get editor-id))]
                                 {:editor editor
                                  :ui (::ui editor)}))
                             
                             ;; else
                             (let [editor-id (or (when-let [f (:file editor-info)]
                                                   (.getCanonicalFile f))
                                                 (:ns editor-info)
                                                 (random-uuid))
                                   mode (clobber-editor/guess-mode editor-info)
                                   editor (clobber-editor/make-editor (assoc editor-info :mode mode))
                                   ui (clobber-editor/editor-ui mode)]
                               {:editor (assoc editor ::id editor-id)
                                :ui ui}))

        editor-id (::id editor)
        editor (assoc editor ::ui ui)

        editor (if-let [line (:line editor-info)]
                 (text-mode/editor-goto-line editor line)
                 editor)

        editor (assoc editor
                      :label (:label editor-info)
                      :mx-commands (into (get editor :mx-commands [])
                                         [::show-tap-watcher])
                      :key-bindings
                      (assoc (:key-bindings editor)
                             "C-x 3" ::split-pane
                             "C-x k" ::delete-pane
                             "C-x b" ::show-select-buffer
                             "C-x C-b" ::show-buffer-viewer
                             "C-x o" ::focus-next
                             "C-x 1" ::close-other-panes
                             "C-x 0" ::hide-pane))

        [width height] size
        editor (-> editor
                   (clobber.util.ui/editor-set-height height)
                   (assoc :width width
                          :height height)
                   (text-mode/editor-update-viewport)
                   (make-active id))
        $editor [$shared (list 'keypath ::editors) (list 'keypath editor-id)]]
    (dispatch!
     :update $shared
     (fn [shared]
       (assoc-in shared [::editors editor-id] editor)))
    (dispatch!
     :update
     $ref
     (fn [applet]
       (-> applet
           (assoc :ui ui)
           (assoc ::editor-id editor-id)
           (assoc :$editor [$shared
                            '(keypath ::editors)
                            (list 'keypath editor-id)]))))
    (dispatch! ::cui/auto-reload-file
               {:editor editor
                :$editor $editor})
    (dispatch!
     ::make-editor-active {:$editor $editor
                           :id id})))


(defrecord ClobberApplet [dispatch! editor-info]
  model/IApplet
  (-start [this {:keys [$ref size $shared] :as info}]
    (assoc this
           :extra {}
           :tap-vals []
           :$ref $ref
           :$shared $shared
           ::easel/shared-keys [::editors]
           :size size
           ::model/queue
           [(fn []
              (load-editor 
               (assoc info
                      :id (:id this)
                      :editor-info editor-info
                      :dispatch! dispatch!))
              (dispatch! :repaint!))]))
  (-stop [this]
    nil)
  model/IUI
  (-ui [this ui-info]
    (clobber-ui this ui-info))
  model/IResizable
  (-resize [this size _content-scale]
    (let [[width height] size]
      (-> this
          (assoc :size size)
          (update ::model/queue
                  (fn [q]
                    (let [q (or q [])]
                      [(fn []
                         (when (and (get this ::editor-id)
                                    (:$shared this))
                           (let [$shared (:$shared this)
                                 $editor [$shared
                                          '(keypath ::editors)
                                          (list 'keypath (get this ::editor-id))]]
                             (dispatch! :update $editor
                                        (fn [editor]
                                          (-> editor
                                              (clobber.util.ui/editor-set-height height)
                                              (assoc :width width)
                                              (assoc :height height)))))))])))))))

(defn ^:private truncate-string-end [s n]
  (if (> (count s) n)
    (subs s 0 n)
    s))

(defn ^:private truncate-string-begin [s n]
  (if (> (count s) n)
    (subs s (- (count s) n))
    s))



(defn clobber-applet [handler {:keys [file url ns string label] :as m}]
  (let [name (cond
               label label

               file (.getCanonicalPath file)
                   
               url (java.net.URL/.getPath url)
               
               ns (str ns)
               
               :else "Clobber")
        name (truncate-string-begin name 16)
        m (assoc m :label name)]
    (-> (->ClobberApplet handler m)
      (assoc :label (str name)))))


(defeffect ::show-select-buffer [{:keys [$ref $focus id $editor]}]
  (let [applets (dispatch! :com.phronemophobic.easel/get-applets)
        clobber-applets (into []
                              (keep (fn [[id applet]]
                                      (when (instance? ClobberApplet applet)
                                        applet)))
                              applets)]
    (dispatch! :update $ref assoc
               ::buffer-select-state {:applets clobber-applets})
    (dispatch!
     ::make-editor-active {:$editor $editor
                           :id id})))


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
      (let [applet (get applets (:applet-id next-clobber-pane))
            $shared (:$shared applet)

            $editor [$shared
                     '(keypath ::editors)
                     (list 'keypath (get applet ::editor-id))]]

        (dispatch!
         ::make-editor-active {:$editor $editor
                               :id (:applet-id next-clobber-pane)})))))

(defn editor-saved?
  "Returns true if the editor has an associated file and the last save is after the last change."
  [editor] 
  (or (not (:file editor))
      (let [^java.time.Instant last-file-load (:last-file-load editor)
            ^java.time.Instant last-change (:last-change editor)]
        (or (not last-change)
            (= last-file-load last-change)
            (.isAfter last-file-load last-change)))))


(defeffect ::open-editor [{:keys [editor]}]
  (let [forked-editor (-> editor
                          (update :tree
                                  (fn [^org.treesitter.TSTree tree]
                                    (when tree
                                      (.copy tree)))))]
    (dispatch! 
     :com.phronemophobic.easel/add-applet
      {:make-applet
       #(clobber-applet % {:editor forked-editor
                           :label (str (:label editor) "*")
                           :ui (::ui editor)})})))


(defn clean-up-shared-editors [easel]
  (let [applets (:applets easel)
        clobber-applets (into []
                              (keep (fn [[id applet]]
                                      (when (instance? ClobberApplet applet)
                                        applet)))
                              applets)
        in-use-editor-ids (into #{}
                                (map ::editor-id)
                                clobber-applets)
        
        keep-editor-pred (fn [[eid editor]]
                           (or 
                            (contains? in-use-editor-ids eid)
                            (not (editor-saved? editor))))

        ;; enqueue stopping auto reload
        unwatches (into []
                        (comp (remove keep-editor-pred)
                              (map second)
                              (map :com.phronemophobic.clobber.modes.clojure.ui/auto-reload-unwatch))
                        (-> easel 
                            :shared-applet-state
                            ::editors))
        easel (update easel ::easel/queue
                      (fn [q]
                        (into (or q [])
                              unwatches)))

        easel (update-in easel
                         [:shared-applet-state ::editors]
                         (fn [m]
                           (into {}
                                 (filter keep-editor-pred)
                                 m)))]
    easel))

(defn purge-shared-editors [easel]
  (let [applets (:applets easel)
        clobber-applets (into []
                              (keep (fn [[id applet]]
                                      (when (instance? ClobberApplet applet)
                                        applet)))
                              applets)

        ;; enqueue stopping auto reload
        unwatches (into []
                        (comp (map second)
                              (keep :com.phronemophobic.clobber.modes.clojure.ui/auto-reload-unwatch))
                        (-> easel 
                            :shared-applet-state
                            ::editors))
        easel (update easel ::easel/queue
                      (fn [q]
                        (into (or q [])
                              unwatches)))

        easel (assoc-in easel
                        [:shared-applet-state ::editors]
                        {})
        
        easel (transduce
               (map :id)
               (completing
                (fn [easel id]
                 (model/-remove-applet easel id)))
               easel
               clobber-applets)]
    easel))

(comment
  (tap>
   (-> (clean-up-shared-editors (-> @com.phronemophobic.easel/app-state :easel))
       :shared-applet-state
       ::editors))
  
  (tap>
   (-> ( (-> @com.phronemophobic.easel/app-state :easel))
       :shared-applet-state
       ::editors))
  (tap> @com.phronemophobic.easel/app-state)

 
  
  ,)

(defeffect ::cleanup-editors [{}]
  (dispatch! ::easel/update-easel clean-up-shared-editors))

(defeffect ::purge-editors [{}]
  (dispatch! ::easel/update-easel purge-shared-editors))

(defui buffer-viewer [{:keys [editors]}]
  (ui/vertical-layout
   (ui/horizontal-layout
    (ant/button {:text "cleanup"
                :on-click (fn []
                            [[::cleanup-editors {}]])})
    (ant/button {:text "purge"
                :on-click (fn []
                            [[::purge-editors {}]])}))
   (ui/table-layout
    (into []
          (map (fn [[editor-id editor]]
                 [(ui/checkbox (editor-saved? editor))
                  (ui/on
                   :mouse-down
                   (fn [_]
                     [[::open-editor {:editor editor}]])
                   (ui/label editor-id))
                  ]
                 ))
          editors))))

(defrecord BufferViewerApplet [dispatch!]
  model/IApplet
  (-start [this {:keys [$ref size $shared] :as info}]
    (assoc this
           :label "Buffers"
           :extra {}
           :$extra [$ref '(keypath :extra)]
           :$ref $ref
           :$shared $shared
           ::easel/shared-keys [::editors]
           :size size))
  (-stop [this]
    nil)
  model/IUI
  (-ui [this ui-info]
    (buffer-viewer 
     (assoc this
            :editors (-> ui-info :shared ::editors)
            :context (:context ui-info)
            :$context (:$context ui-info))))
  model/IResizable
  (-resize [this size _content-scale]
    (let [[width height] size]
      (assoc this :size size))))

(defn show-buffer-viewer []
  ((requiring-resolve 'com.phronemophobic.easel/add-applet)
   {:make-applet
    #(->BufferViewerApplet %)}))

(defeffect ::show-buffer-viewer [{}]
  (show-buffer-viewer))

(comment
  
  (show-buffer-viewer)
  ,)

