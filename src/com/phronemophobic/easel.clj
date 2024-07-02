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
            [membrane.component
             :refer
             [defui defeffect]]))


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

(defeffect ::add-applet [make-applet]
  (dispatch!
   :update :easel
   (fn [easel]
     (model/-add-applet easel
                        (make-applet dispatch!))))
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

(defn top-bar [pane]
  (let [pane-id (:id pane)
        bar (ui/horizontal-layout
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
                 "X"))))
        height (ui/height bar)]
    [(ui/with-style ::ui/style-stroke
       (ui/rectangle (:width pane) height))
     bar]))

(def top-bar-height (ui/height (top-bar {:width 0})))

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
   :update :easel
   (fn [easel]
     (->> easel
          (specter/transform [:root-pane splitpane/WALK-PANE #(= pane-id (:id %))]
                             #(-> %
                                  (assoc :applet-id nil)
                                  (splitpane/add-child {:id (random-uuid)
                                                        :applet-id (:applet-id %)})
                                  (splitpane/add-child {:id (random-uuid)})))
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


(defn easel-ui* [{:keys [root-pane applets] :as easel} $context context]
  (into []
        (keep (fn [pane]
                (if (seq (:panes pane))
                  ;; non-leaf
                  (ui/translate (:x pane)
                                (:y pane)
                                (top-bar pane))
                  ;; else, leaf node
                  (ui/translate (:x pane)
                                (:y pane)
                                (ui/vertical-layout
                                 (top-bar pane)
                                 (if-let [applet (get applets (:applet-id pane))]
                                   (ui/fixed-bounds
                                    [(:width pane)
                                     (:height pane)]
                                    (model/-ui applet $context context))
                                   ;; no applet found
                                   ;; drag target?
                                   nil))))))
        (-> easel ::cached-layout :all-panes)))

(defrecord AEasel [applets
                   last-id $ref
                   root-pane]
  model/IEasel
  (-add-applet [this applet]
    (let [
          id (inc last-id)
          applet (assoc applet :id id)
          root-pane (-> root-pane
                        (splitpane/add-child {:id (random-uuid)
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
          applet (when applet
                   (try
                     (model/-stop applet)
                     (catch Exception e
                       (println "Error when closing applet:")
                       (prn e))))

          root-pane (-> root-pane
                        (splitpane/delete-pane-by-pred #(= id (:applet-id %))))]
      (-> this
          (update :applets dissoc id)
          (assoc :root-pane root-pane)
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
    (easel-ui* this $context context)))

(defn make-easel []
  (-> (map->AEasel
       {:applets (tiara/ordered-map)
        :last-id 0
        :root-pane {:id ::root-pane
                    :width 1
                    :height 1
                    :panes []}})
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
      (tab-view {:tabs (vals (model/-applets easel))
                 :selected (-> easel ::cached-layout :by-applet-id)
                 :width tab-width}))
     (ui/on
      ::toggle-pane-direction
      (fn [m]
        [[::toggle-pane-direction (assoc m :$easel $easel)]])
      ::add-pane-child
      (fn [m]
        [[::add-pane-child (assoc m :$easel $easel)]])
      ::delete-pane
      (fn [m]
        [[::delete-pane (assoc m :$easel $easel)]])
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
                     {:easel (assoc (make-easel)
                                    :$ref (specter/keypath :easel))})))
        
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
          (when (not (seq (model/-applets (:easel @app-state))))
            (handler ::add-applet
                     list-applets/list-applets))
          (skia/-reshape window window-handle width height))}})))


(defn add-term []
  (swap! app-state
         update :easel
         model/-add-applet (term/termlet handler))
  nil)

(defn add-browser [url]
  (swap! app-state
         update :easel
         model/-add-applet (browser/browslet handler url))
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

(defn reset-easel! []
  (remove-all-widgets!)
  (handler ::add-applet
           list-applets/list-applets)


           
  ,)

(defn reset-run []
  (try
    (remove-all-widgets!)
    (catch Exception e
      (prn e)
      nil))
  (reset! app-state nil)
  (run))

