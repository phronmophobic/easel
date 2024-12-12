(ns com.phronemophobic.easel
  (:require [membrane.skia :as skia]
            [membrane.skia.paragraph :as para]
            [membrane.ui :as ui]
            [membrane.alpha.stretch :as stretch]
            [tiara.data :as tiara]
            [com.rpl.specter :as specter]
            [clojure.java.io :as io]
            [com.phronemophobic.easel.list-applets :as
             list-applets]
            [com.phronemophobic.easel.splitpane :as splitpane]
            [com.phronemophobic.easel.term
             :as term]
            [com.phronemophobic.easel.model :as model]
            [com.phronemophobic.easel.browser :as browser]
            [membrane.alpha.component.drag-and-drop :as dnd]
            [com.phronemophobic.schematic.view.util
             :as schematic-util]
            [membrane.component
             :refer
             [defui defeffect]])
  (:import (java.util.concurrent
            ExecutorService
            Executors)))


(def repaint! @#'skia/glfw-post-empty-event)

(defonce app-state
  (atom nil))

(def handler (membrane.component/default-handler app-state))
(def ^java.util.Queue main-queue
  (java.util.concurrent.ConcurrentLinkedQueue.))
(defeffect :repaint! []
  ;; needs debounce
  (repaint!)

  )

(defeffect :watch [key $ref f]
  (let [path (membrane.component/path->spec $ref)]
    (add-watch app-state key
               (fn [key ref old new]
                 (let [new-val (specter/select-one path new)
                       old-val (specter/select-one path old)]
                   (when (not= new-val old-val)
                     (f key ref old-val new-val)))))))



(defonce app-starter-executor
  (delay
    (Executors/newSingleThreadExecutor)))

(def ^:private QUEUES-PATH
  (specter/path
   [:easel
    (specter/multi-path
     [:applets specter/MAP-VALS (specter/must ::model/queue)]
     (specter/must ::queue))]))

(defn purge-queue [state]
  (specter/setval
   QUEUES-PATH
   specter/NONE
   state))

(defn ^:private run-queue []
  (let [[old new] (swap-vals! app-state purge-queue)
        queue (specter/select [QUEUES-PATH specter/ALL] old)]
    (println "woo!" (count queue))
    (run! (fn [work]
            (try
              (work)
              (catch Throwable e
                (println e))))
          queue)))

(add-watch app-state ::applet-queue
           (fn [key ref old new]
             (let [has-queue? (not= specter/NONE (specter/select-any QUEUES-PATH new))]
               (when has-queue?
                 (.submit ^ExecutorService @app-starter-executor
                          run-queue)))))

(defeffect ::add-applet [m]
  (dispatch!
   :update :easel
   (fn [easel]
     (model/-add-applet easel m)))
  nil)

(defn add-component! [key f]
  (handler :update
           '[(keypath :membrane.component/context)
             (keypath :com.phronemophobic.easel.schematic2/component-picker-components)]
           (fn [components]
             (assoc components key f))))

(defeffect ::add-component! [key f]
  (add-component! key f))

(defn on-main-callback []
  (loop []
    (when-let [workf (.poll main-queue)]
      (try
        (workf)
        (catch Throwable e
          (prn e)))
      (recur))))

(defeffect :dispatch-main [f]
  (.add main-queue f)
  (repaint!))


(defn col-width [easel visible-count]
  (let [[w h] (:size easel)
        col-width (if (pos? visible-count)
                    (quot w visible-count)
                    w)]
    col-width))


(defui top-bar [{:keys [pane]}]
  (let [drop-object (:drop-object context)
        pane-id (:id pane)
        bar
        (ui/horizontal-layout
         (ui/on-click
          (fn []
            [[::toggle-pane-direction {:pane-id pane-id}]])
          (para/paragraph
           (if (= (:direction pane) :column)
             "↕️"
             "↔️")))
         (ui/on-click
          (fn []
            [[::splitpane {:pane-id pane-id}]])
          (para/paragraph
           "+"))
         (when (not= ::root-pane pane-id)
           (ui/on-click
            (fn []
              [[::delete-pane {:pane-id pane-id}]])
            (para/paragraph
             "X")))
         (ui/on-click
            (fn []
              [[::clear-pane {:pane-id pane-id}]])
            (para/paragraph
             "O")))
        height (ui/height bar)

        drag-object (get extra ::drag-object)]
    [(ui/on
      :mouse-down
      (fn [_]
        [[::dnd/drag-start {::dnd/obj {::pane pane}}]])
      (schematic-util/on-drag-hover
       {:$body nil
        :extra (get extra ::drag-extra)
        :body
        [(when (and (::pane drag-object)
                    drop-object)
           (dnd/on-drop
            (fn [_pos obj]
              (when-let [drop-pane (::pane obj)]
                [[::swap-panes {:from-pane-id (:id drop-pane)
                                :to-pane-id (:id pane)}]]))
            (ui/filled-rectangle
             [0.8 0.8 0.8]
             (:width pane) height)))
         (ui/with-style ::ui/style-stroke
           (ui/rectangle (:width pane) height))]
        :object drag-object}))
     bar]))

(def top-bar-height (ui/height (top-bar {:pane {:width 0}})))

(defn relayout* [easel]
  (let [root-pane (:root-pane easel)]
    (model/-resize easel [(:width root-pane) (:height root-pane)] (:content-scale easel))))

(defeffect ::toggle-pane-direction [{:keys [$easel pane-id]}]
  (dispatch!
   :update :easel
   (fn [easel]
     (->> easel
          (specter/transform [:root-pane splitpane/WALK-PANE #(= pane-id (:id %))]
                             #(splitpane/toggle-direction %))
          relayout*))))

(defeffect ::splitpane [{:keys [$easel pane-id]}]
  (dispatch!
   :update $easel
   (fn [easel]
     (->> easel
          (specter/transform [:root-pane splitpane/WALK-PANE #(= pane-id (:id %))]
                             (fn [pane]
                               (if (empty? (:panes pane))
                                 (-> pane
                                     (assoc :applet-id nil)
                                     (splitpane/add-child {:id (random-uuid)
                                                           :applet-id (:applet-id pane)})
                                     (splitpane/add-child {:id (random-uuid)}))
                                 ;; pane already has children
                                 (splitpane/add-child pane {:id (random-uuid)}))))
          relayout*))))

(defeffect ::swap-panes [{:keys [$easel from-pane-id to-pane-id]}]
  (dispatch!
   :update $easel
   (fn [easel]
     (-> easel
         (update :root-pane
                 (fn [root-pane]
                   (let [from-pane (splitpane/find-pane-by-id root-pane from-pane-id)
                         to-pane (splitpane/find-pane-by-id root-pane to-pane-id)]
                     (if (and from-pane to-pane)
                       (-> root-pane
                           (splitpane/edit-pane-by-id from-pane-id
                                                      (fn [pane]
                                                        (assoc pane :applet-id (:applet-id to-pane))))
                           (splitpane/edit-pane-by-id to-pane-id
                                                      (fn [pane]
                                                        (assoc pane :applet-id (:applet-id from-pane)))))
                       ;; else
                       root-pane))))
          relayout*))))

(defeffect ::delete-pane [{:keys [$easel pane-id]}]
  (dispatch!
   :update $easel
   (fn [easel]
     (let [pane (->> easel
                     ::cached-layout
                     :all-panes
                     (some #(= pane-id (:id %))))]
       (if-let [applet-id (:applet-id pane)]
         (model/-remove-applet easel applet-id)
         (-> easel
             (update :root-pane
                     #(splitpane/delete-pane-by-id % pane-id))
             relayout*))))))

(defeffect ::clear-pane [{:keys [$easel pane-id]}]
  (dispatch!
   :update $easel
   (fn [easel]
     (-> easel
         (update :root-pane
                 (fn [root-pane]
                   (-> root-pane
                       (splitpane/edit-pane-by-id pane-id
                                                  (fn [pane]
                                                    (dissoc pane :applet-id))))))
         relayout*))))


(defn easel-ui* [{:keys [root-pane applets] :as easel} $extra extra $context context]
  (into []
        (keep (fn [pane]
                (let [bar-extra (get extra [::bar-extra (:id pane)])
                      $bar-extra [$extra
                                  (list 'keypath [::bar-extra (:id pane)])]
                      bar (top-bar {:pane pane
                                    :extra bar-extra
                                    :$extra $bar-extra
                                    :context context
                                    :$context $context})]
                  (if (seq (:panes pane))
                    ;; non-leaf
                    (ui/translate (:x pane)
                                  (:y pane)
                                  bar)
                    ;; else, leaf node
                    (ui/translate (long (:x pane))
                                  (long (:y pane))
                                  (ui/vertical-layout
                                   bar
                                   (if-let [applet (get applets (:applet-id pane))]
                                     (ui/scissor-view
                                      [0 0]
                                      [(:width pane)
                                       (:height pane)]
                                      (model/-ui applet $context context))
                                     ;; no applet found
                                     (let [pane-id (:id pane)
                                           list-applets-extra (get extra [::list-applets-extra pane-id])
                                           $list-applets-extra [$extra
                                                                (list 'keypath [::list-applets-extra pane-id])]
                                           shared-state (get extra ::list-applets-shared-state)
                                           $shared-state [$extra (list 'keypath ::list-applets-shared-state)]]
                                       (ui/on
                                        ::add-applet
                                        (fn [m]
                                          [[::add-applet (assoc m :pane-id pane-id)]])
                                        (list-applets/list-applets2 {:shared-state shared-state
                                                                     :$shared-state $shared-state
                                                                     :extra list-applets-extra
                                                                     :$extra $list-applets-extra
                                                                     :context context
                                                                     :$context $context}))))))))))
        (-> easel ::cached-layout :all-panes)))

(defrecord AEasel [applets
                   last-id $ref
                   root-pane]
  model/IEasel
  (-add-applet [this info]
    (let [{:keys [make-applet pane-id pop-out?]} info
          applet (make-applet handler)
          id (inc last-id)
          applet (assoc applet :id id)
          root-pane (if (and (not pop-out?)
                             pane-id)
                      (splitpane/edit-pane-by-id root-pane pane-id
                                                 #(assoc % :applet-id id))
                      (splitpane/add-child root-pane {:id (random-uuid)
                                                      :applet-id id}))

          this-pane (get-in this [::cached-layout :by-applet-id id])
          new-size [(:width this-pane) (:height this-pane)]]

      (-> this
          (assoc :last-id id)
          (assoc-in [:applets id] applet)
          (assoc :root-pane root-pane)
          relayout*)))
  (-remove-applet [this id]
    (let [applet (get applets id)
          root-pane (-> root-pane
                        (splitpane/delete-pane-by-pred #(= id (:applet-id %))))]
      (-> this
          (update :applets dissoc id)
          (assoc :root-pane root-pane)
          (update ::queue
                  (fnil conj [])
                  (fn []
                    (when applet
                      (try
                        (model/-stop applet)
                        (catch Exception e
                          (println "Error when closing applet:")
                          (prn e))))))
          relayout*)))
  (-show-applet [this id]
    (-> this
        (update :root-pane
                #(splitpane/add-child % {:id (random-uuid)
                                         :applet-id id}))
        relayout*))
  (-hide-applet [this id]
    (-> this
        (update :root-pane
                (fn [pane]
                  (splitpane/delete-pane-by-pred pane #(= id (:applet-id %)))))
        relayout*))
  (-visible-applets [this]
    (let [by-applet-id (-> this
                           ::cached-layout
                           :by-applet-id)]
      (into []
            (filter (fn [applet]
                      (contains? by-applet-id (:applet-id applet))))
            applets)))

  (-applets [this]
    applets)
  model/IResizable
  (-resize [this [w h] content-scale]
    (let [root-pane (-> root-pane
                        (assoc :width w
                               :height h))
          root-pane (if (empty? (:panes root-pane))
                      ;; ensure that root pane can't be edited.
                      (splitpane/add-child root-pane {:id (random-uuid)})
                      ;; pane already has children
                      root-pane)
          root-pane (splitpane/layout-pane-nested root-pane top-bar-height)
          all-panes (splitpane/flatten-pane-nested root-pane)
          cached-layout {:all-panes all-panes
                         :by-applet-id
                         (into {}
                               (keep (fn [pane]
                                       (when-let [applet-id (:applet-id pane)]
                                         [applet-id pane])))
                               all-panes)}

          this (assoc this
                      ;; :size [w h]
                      :root-pane root-pane
                      ::cached-layout cached-layout
                      :content-scale content-scale)
          this (let [start-or-resize
                     (fn [pane applet]
                       (let [new-size [(:width pane)
                                       (max 0
                                            (- (:height pane) top-bar-height))]]
                        (if (::applet-started? applet)
                          (model/-resize applet
                                         new-size
                                         content-scale)
                          (-> applet
                              (model/-start (specter/path
                                              (membrane.component/path->spec $ref)
                                              (specter/keypath :applets (:id applet)))
                                            new-size
                                            content-scale)
                              (assoc ::applet-started? true))
                          )))
                     applets
                     (transduce
                      (filter (fn [pane]
                                (get applets (:applet-id pane))))
                      (completing
                       (fn [applets pane]
                         (update applets (:applet-id pane)
                                 #(start-or-resize pane %))))
                      applets
                      all-panes)]
                 (assoc this :applets applets))]
      this))
  model/IUI
  (-ui [this $context context]
    (let [extra (get this ::extra)
          $extra [$ref (list 'keypath ::extra)]]
     (easel-ui* this $extra extra $context context))))

(defn make-easel []
  (-> (map->AEasel
       {:applets (tiara/ordered-map)
        :last-id 0
        ::extra {}
        :root-pane {:id ::root-pane
                    :width 1
                    :height 1
                    :panes [{:id (random-uuid)}]}})
      relayout*))

(defn delete-X []
  (ui/with-style :membrane.ui/style-stroke
    (ui/with-color
      [1 0 0]
      (ui/with-stroke-width
        3
        [(ui/path [0 0]
                  [10 10])
         (ui/path [10 0]
                  [0 10])]))))

(def tab-height 30)
(defui tab-view [{:keys [tabs selected width]}]
  (stretch/vlayout
   (map (fn [tab]
          (let [background (ui/rectangle width tab-height)
                background (if (selected (:id tab))
                             (->> background
                                  (ui/with-style ::ui/style-stroke))
                             (->> background
                                  (ui/with-color [0.8 0.8 0.8])
                                  (ui/with-style ::ui/style-fill)))
                lbl (ui/center (ui/label (:label tab))
                               (ui/bounds background))
                close (ui/on
                       :mouse-down
                       (fn [_]
                         [[:stop (:id tab)]])
                       (delete-X))]
            [(ui/on
              :mouse-down
              (fn [_]
                [[:toggle (:id tab)]])
              [background
               lbl])
             (ui/translate
              (- width 20)
              (- (/ tab-height 2)
                 4)
              close)])))
   tabs))

(def tab-width 150)
(defui easel-view [{:keys [easel]}]
  (let [[cw ch :as size] (:membrane.stretch/container-size context)]
    (ui/horizontal-layout
     (ui/on
      :toggle
      (fn [id]
        [[:update $easel
          (fn [easel]
            (let [visible (-> easel ::cached-layout :by-applet-id)
                  easel (if (contains? visible id)
                          (model/-hide-applet easel id)
                          (model/-show-applet easel id))]
              easel))]])
      :stop
      (fn [id]
        [[:update $easel
          (fn [easel]
            (-> easel
                (model/-remove-applet id)))]])
      (ui/vertical-layout
       (tab-view {:tabs (vals (model/-applets easel))
                  :selected (-> easel ::cached-layout :by-applet-id)
                  :width tab-width})))
     (ui/on-bubble
      (fn [intents]
        (specter/transform
         [specter/ALL
          (fn [intent]
            (#{::toggle-pane-direction
               ::add-pane-child
               ::delete-pane
               ::clear-pane
               ::splitpane
               ::swap-panes}
             (first intent)))]
         (fn [intent]
           [(first intent) (assoc (second intent) :$easel $easel)])
         intents))
      (dnd/drag-and-drop
       {:$body nil
        :body
        (model/-ui easel $context context)})))))



(def pad 20)

(defn run []
  (let [
        _ (swap! app-state
                 (fn [state]
                   (if state
                     state
                     {:easel (-> (assoc (make-easel)
                                        :$ref (specter/keypath :easel))
                                 ;; hack for now.
                                 ;; otherwise, size just starts at zero
                                 ;; need to use `on-present`?
                                 (model/-resize [787 847] [2 2]))})))
        
        app (membrane.component/make-app #'easel-view app-state handler)]
    (skia/run app
      {:include-container-info true
       ::skia/on-main on-main-callback
       :handlers
       {:reshape
        (fn [window window-handle width height]
          (let [[xscale yscale] (skia/get-window-content-scale-size window-handle)
                width (int (/ width xscale))
                height (int (/ height yscale))]
            (swap! app-state update :easel
                   model/-resize
                   [(- width tab-width) height]
                   [xscale yscale]))
          (skia/-reshape window window-handle width height))}})))


(defn add-term []
  (swap! app-state
         update :easel
         model/-add-applet {:make-applet (term/termlet handler)})
  nil)

(defn add-browser [url]
  (swap! app-state
         update :easel
         model/-add-applet {:make-applet (browser/browslet handler url)})
  nil)

(defn remove-all-widgets [easel]
  (let [easel (reduce
               model/-remove-applet
               easel
               (keys (:applets easel)))
        easel (model/-resize easel
                             (:size easel)
                             (:content-scale easel))]
    easel))

(defn remove-all-widgets! []
  (swap! app-state
         (fn [state]
           (update state :easel remove-all-widgets)))
  nil)

(defn remove-widget! [id]
  (swap! app-state
         (fn [state]
           (update state :easel
                   (fn [easel]
                     (model/-remove-applet easel id)))))
  nil)

(defn reset-easel! []
  (remove-all-widgets!)
  
  ;; (require '[com.phronemophobic.llama :as llama])
  ;; (require '[com.phronemophobic.llama.util :as llutil])
  ;; (def model-path "../llama.clj/models/llama-2-7b-chat.Q4_0.gguf")
  ;; (def ctx (llama/create-context model-path {:n-ctx 2048
  ;;                                      :n-gpu-layers 100
  ;;                                      }))
           
  ,)

(defn reset-run []
  (try
    (remove-all-widgets!)
    (catch Exception e
      (prn e)
      nil))
  (reset! app-state nil)
  (run))


(comment
  (run)
  (skia/run #(/ 1 0))

  (reset-run)

  ,)

