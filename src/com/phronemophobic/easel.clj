(ns com.phronemophobic.easel
  (:require [membrane.skia :as skia]
            [membrane.skia.paragraph :as para]
            [membrane.ui :as ui]
            [membrane.alpha.stretch :as stretch]
            [tiara.data :as tiara]
            [com.rpl.specter :as specter]
            [clojure.java.io :as io]
            [clojure.zip :as z]
            [com.phronemophobic.easel.list-applets :as
             list-applets]
            [com.phronemophobic.easel.splitpane :as splitpane]
            [com.phronemophobic.easel.term
             :as term]
            [com.phronemophobic.membrandt :as ant]
            [com.phronemophobic.membrandt.icon.ui :as icon.ui]
            [com.phronemophobic.easel.model :as model]
            [com.phronemophobic.easel.browser :as browser]
            [membrane.alpha.component.drag-and-drop :as dnd]
            [com.phronemophobic.schematic.view.util
             :as schematic-util]
            [membrane.basic-components :as basic]
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
             (assoc components key f)))
  nil)

(defeffect ::add-component! [key f]
  (add-component! key f))

(defrecord ComponentApplet [label component-var initial-state]
  model/IApplet
  (-start [this $ref size _content-scale]
    (let [component-meta (meta component-var)
          arglists (:arglists component-meta)
          first-arglist (first arglists)
          arg-map (first first-arglist)
          args (:keys arg-map)
          $args (into {}
                      (comp
                       (remove '#{extra context})
                       (map (fn [arg]
                              (let [kw (keyword (name arg))
                                    $kw (keyword (str "$" (name arg)))]
                                [$kw [$ref '(keypath :state) (list 'keypath kw)]]))))
                      args)
          state (into (assoc initial-state
                             :extra {}
                             :$extra [$ref '(keypath :state) '(keypath :extra)])
                      $args)

          $scroll-state [$ref '(keypath :scroll-state)]]
      (assoc this
             ;; :dispatch! dispatch!
             :$ref $ref
             :state state
             :scroll-state {:$extra [$ref '(keypath :scroll-state :extra)]
                            :extra {}
                            :offset [0 0]
                            :$offset [$scroll-state '(keypath :offset)]}
             :size size)))
  (-stop [this])
  model/IUI
  (-ui [this $context context]
    (let [context (assoc context
                         :membrane.stretch/container-size
                         (:size this))
          ui (component-var
              (assoc (:state this)
                     :context context
                     :$context $context))

          scroll-state (:scroll-state this)
          $ref (:$ref this)

          [cw ch] (:size this)
          scroll-bounds [(max 0 (- cw 7 4))
                         (max 0 (- ch 7 4))]
          ui (basic/scrollview
              (assoc scroll-state
                     :body ui
                     :context context
                     :$context $context
                     :scroll-bounds scroll-bounds))]
      (ui/translate
       4 4
       (ui/try-draw
        (try
          ui
          (catch Exception e
            (tap> e)
            (prn e)
            (ui/label "Error!")))
        (fn [draw e]
          (tap> e)
          (prn e)
          (draw (ui/label "Error!")))))))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn add-component-as-applet [component-var initial-state]
  (handler :com.phronemophobic.easel/add-applet
           {:make-applet
            (fn [_]
              (map->ComponentApplet
               {:label (or (-> component-var
                               meta
                               :name)
                           "Component")
                :component-var component-var
                :initial-state initial-state}))}))
(defeffect ::add-component-as-applet [component-var initial-state]
  (add-component-as-applet component-var initial-state))

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
          (if (= (:direction pane) :column)
            (icon.ui/icon {:name "column-height"})
            (icon.ui/icon {:name "column-width"})))
         (ui/on-click
          (fn []
            [[::splitpane {:pane-id pane-id}]])
          (icon.ui/icon {:name "plus"}))
         (when (not= ::root-pane pane-id)
           (ui/on-click
            (fn []
              [[::delete-pane {:pane-id pane-id}]])
            (icon.ui/icon {:name "close"})))
         (ui/on-click
            (fn []
              [[::clear-pane {:pane-id pane-id}]])
            (icon.ui/icon {:name "minus-square"}))
         (when (= ::root-pane pane-id)
           (ui/on-click
            (fn []
              [[::toggle-pane-resize {:pane-id pane-id}]])
            (icon.ui/icon {:name "edit"}))))
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
           (ui/with-color [0.33 0.33 0.33]
            (ui/rectangle (:width pane) height)))]
        :object drag-object}))
     bar]))

(def top-bar-height (ui/height (top-bar {:pane {:width 0}})))

(defn relayout* [easel]
  (let [root-pane (:root-pane easel)]
    (model/-resize easel [(:width root-pane) (:height root-pane)] (:content-scale easel))))

(defeffect ::toggle-pane-direction [{:keys [$easel pane-id]}]
  (dispatch!
   :update $easel
   (fn [easel]
     (->> easel
          (specter/transform [:root-pane splitpane/WALK-PANE #(= pane-id (:id %))]
                             #(splitpane/toggle-direction %))
          relayout*))))

(defeffect ::toggle-pane-resize [{:keys [$easel pane-id]}]
  (dispatch!
   :update $easel
   (fn [easel]
     (->> easel
          (specter/transform [:root-pane splitpane/WALK-PANE #(= pane-id (:id %))]
                             #(update % ::pane-resize not))))))

(defn ^:private zfind
  "Finds first loc that matches pred. Returns nil if no match found."
  [loc pred]
  (loop [loc loc]
    (if (z/end? loc)
      nil
      (if (pred (z/node loc))
        loc
        (recur (z/next loc))))))


;; TODO: move more pane code into splitpane namespace.
(defeffect ::begin-resize-drag [{:keys [$easel from-pane to-pane mpos] :as m}]
  ;; resizing a pane may convert relative stretch size to
  ;; absolute size, which may overconstrain a pane
  ;; TODO: check invariants for over constraining.
  (assert (or from-pane to-pane))
  ;; check root-pane-layout in cached layout
  ;; make sure :width or :height is set on from-pane and to-pane
  ;; call relayout*
  (let [setup-drag
        (fn [easel]
          (let [root-pane-layout (-> easel ::cached-layout :root-pane-layout)
                _ (assert root-pane-layout)

                pane-ids (into #{} (remove nil?) [from-pane to-pane])
                pane-layouts-by-id
                (into {}
                      (map (fn [pane]
                             [(:id pane) pane]))
                      (specter/select [splitpane/WALK-PANE
                                       (fn [pane]
                                         (contains? pane-ids (:id pane)))]
                                      root-pane-layout))

                ;; determine direction
                zpane (zfind (splitpane/pane-zip root-pane-layout)
                             #(contains? pane-ids (:id %)))
                _ (assert (and zpane (z/up zpane)))
                direction (or (-> zpane z/up z/node :direction)
                              :row)

                ;; set explicit sizes on from, to panes
                easel (specter/transform
                       [:root-pane
                        splitpane/WALK-PANE
                        #(contains? pane-ids (:id %))]
                       (fn [pane]

                         (let [size (splitpane/get-size (get pane-layouts-by-id (:id pane))
                                                        direction)]
                           (splitpane/set-size pane direction size)))
                       easel)

                ;; store drag state
                coord (if (= direction :column)
                        (nth mpos 1)
                        (nth mpos 0))
                easel (update easel :root-pane
                              (fn [pane]
                                (assoc pane :drag-state
                                       (merge
                                        (select-keys m [:from-pane :to-pane])
                                        {:direction direction
                                         :start-coord coord}
                                        (when to-pane
                                          {:to-size (splitpane/get-size (get pane-layouts-by-id to-pane)
                                                                        direction)})
                                        (when from-pane
                                          {:from-size (splitpane/get-size (get pane-layouts-by-id from-pane)
                                                                          direction)})))))]
            easel))]
    (dispatch!
     :update $easel
     (fn [easel]
       (-> easel
           setup-drag
           relayout*)))))

(defeffect ::resize-drag [{:keys [$easel mpos]}]

  ;; set width and height on panes as appropriate
  ;; call relayout*
  (dispatch!
   :update $easel
   (fn [easel]
     (let [root-pane (:root-pane easel)
           {:keys [to-size from-size start-mpos to-pane from-pane direction start-coord]
            :as drag-state} (:drag-state root-pane)

           coord (if (= direction :column)
                   (nth mpos 1)
                   (nth mpos 0))
           delta (- coord
                    start-coord)

           pane-ids (into #{} (remove nil?) [from-pane to-pane])
           easel
           (specter/transform
            [:root-pane
             splitpane/WALK-PANE
             #(contains? pane-ids (:id %))]
            (fn [pane]

              (cond
                (= from-pane (:id pane))
                (splitpane/set-size pane direction
                                    (- from-size
                                       delta))

                (= to-pane (:id pane))
                (splitpane/set-size pane direction
                                    (+ to-size
                                       delta))))
            easel)]
       (-> easel
           relayout*)))))

(defeffect ::end-resize-drag [{:keys [$easel from-pane to-pane]}]
  ;; resizing a pane may convert relative stretch size to
  ;; absolute size, which may overconstrain a pane
  ;; TODO: check invariants for over constraining.
  (dispatch!
   :update $easel
   (fn [easel]
     (-> easel
         (update :root-pane
                 (fn [pane]
                   (-> pane
                       (dissoc :drag-state))))
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



(defui pane-resizer-node [{:keys [pane]}]
  (let [subpanes (:panes pane)
        direction (get pane :direction :row)]
    (when (seq subpanes)
      (ui/translate
       (:x pane 0)
       (:y pane 0)
       [(into []
              (map (fn [subpane]
                     (pane-resizer-node {:pane subpane})))
              subpanes)
        (into []
              (comp
               (map-indexed
                (fn [idx {:keys [x y width height] :as subpane}]
                  (let [hover? (get extra [::hover (:id subpane)])
                        fill-color [1 0 0 (if hover?
                                            0.4
                                            0.2)]
                        [x y w h] (case direction
                                    :row [(+ (:x subpane 0) (:width subpane)) (:y subpane 0)
                                          10 (:height subpane)]
                                    :column [(:x subpane 0 ) (+ (:y subpane 0)
                                                                (:height subpane))
                                             (:width subpane) 10])]
                    (ui/translate x y
                                  (basic/on-hover
                                   {:hover? hover?
                                    :$body nil
                                    :body
                                    (ui/on
                                     :mouse-down
                                     (fn [_]
                                       ;; The basic idea is that when resizing a row of panes
                                       ;; we'll break it down into a few cases.
                                       ;; If it's the first item in layout, then we
                                       ;; treat the resizing as if it's setting the absolute size
                                       ;; of the first item.
                                       ;; If it's the last item, we treat it as if it's resizing
                                       ;; the absolute size of the last item.
                                       ;; Finally, if it's in the middle and doesn't border the
                                       ;; edge items, we treat it as if both panes on either side of the
                                       ;; border are being resized.
                                       (cond

                                         (zero? idx)
                                         [[::begin-resize-drag {:to-pane (:id subpane)}]]

                                         (= idx (- (count subpanes) 2))
                                         [[::begin-resize-drag {:from-pane (-> subpanes last :id)}]]

                                         :else
                                         [[::begin-resize-drag {:to-pane (-> subpanes (nth idx) :id)
                                                                :from-pane (-> subpanes (nth (inc idx)) :id)}]]))
                                     (ui/filled-rectangle fill-color
                                                          w h))}))))))
              (drop-last subpanes))]))))


(defui pane-resizer [{:keys [root-pane]}]
  (let [main-view (ui/fixed-bounds
                   [(:width root-pane) (:height root-pane)]
                   (pane-resizer-node {:pane root-pane}))
        main-view (if (:drag-state root-pane)
                    (ui/on
                     :mouse-move
                     (fn [mpos]
                       [[::resize-drag {:mpos mpos}]])
                     :mouse-up
                     (fn [_]
                       [[::end-resize-drag {:pane-id (:id root-pane)}]]
                       )
                     main-view)
                    ;; else, not resizing
                    (ui/wrap-on
                     :mouse-down
                     (fn [handler mpos]
                       (let [intents (handler mpos)]
                         (if (seq intents)
                           (eduction
                            (map (fn [intent]
                                   (if (= ::begin-resize-drag (first intent))
                                     [::begin-resize-drag (assoc (second intent)
                                                                 :mpos mpos)]
                                     intent)))
                            intents)
                           [[::toggle-pane-resize {:pane-id ::root-pane}]])))
                     main-view))]
    main-view))

(defn ^:private first-empty-pane-id [easel]
  (first
   (eduction
    (filter (fn [pane]
              (and (not (:applet-id pane))
                   (empty? (:panes pane)))))
    (map :id)
    (->> easel
         ::cached-layout
         :all-panes
         reverse))))

(defn easel-ui* [{:keys [root-pane applets] :as easel} $extra extra $context context]
  (let [main-view
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
                                              (ui/scissor-view
                                               [0 0]
                                               [(:width pane)
                                                (max 1
                                                     (- (:height pane)
                                                        top-bar-height
                                                        1))]
                                               (list-applets/list-applets2 {:shared-state shared-state
                                                                            :$shared-state $shared-state
                                                                            :extra list-applets-extra
                                                                            :$extra $list-applets-extra
                                                                            :context context
                                                                            :$context $context})))))))))))
              (-> easel ::cached-layout :all-panes))
        main-view (if (::pane-resize root-pane)
                    (ui/no-events main-view)
                    main-view)]
    [main-view
     (when (::pane-resize root-pane)
       (when-let [root-pane-layout (-> easel ::cached-layout :root-pane-layout)]
         (pane-resizer {:root-pane root-pane-layout
                        :extra (get extra ::pane-resizer)
                        :$extra [$extra (list 'keypath ::pane-resizer)]})))]))



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
    (let [;; check if there is an empty pane first
          ;; if so, add it to the empty pane
          ;; otherwise, add a new pane
          empty-pane-id (first-empty-pane-id this)

          this (if empty-pane-id
                 (update this :root-pane
                         (fn [root-pane]
                           (splitpane/edit-pane-by-id root-pane empty-pane-id #(assoc % :applet-id id))))
                 (update this :root-pane
                         #(splitpane/add-child % {:id (random-uuid)
                                                  :applet-id id})))]
      (relayout* this)))
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
    (let [root-pane (if (empty? (:panes root-pane))
                      ;; ensure that root pane can't be edited.
                      (splitpane/add-child root-pane {:id (random-uuid)})
                      ;; pane already has children
                      root-pane)
          root-pane (-> root-pane
                        (assoc :width w
                               :height h))
          root-pane-layout (splitpane/layout-pane-nested root-pane top-bar-height)
          all-panes (splitpane/flatten-pane-nested root-pane-layout)
          cached-layout {:all-panes all-panes
                         :root-pane-layout root-pane-layout
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
                                               (specter/keypath :applets)
                                               (specter/must (:id applet)))
                                             new-size
                                             content-scale)
                               (assoc ::applet-started? true)))))
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
        :workspaces {:by-id {}
                     :last-id 0}
        :root-pane {:id ::root-pane
                    :width 1
                    :height 1
                    :panes [{:id (random-uuid)}]}})
      relayout*))

(def tab-height 30)
(defui tab-view [{:keys [tabs selected width]}]
  [(ui/spacer width 0)
   (stretch/vlayout
    (map (fn [tab]
           (let [background (ui/rectangle width tab-height)
                 background (if (selected (:id tab))
                              (->> background
                                   (ui/with-style ::ui/style-stroke)
                                   (ui/with-color [0.33 0.33 0.33]))
                              (->> background
                                   (ui/with-color [0.8 0.8 0.8])
                                   (ui/with-style ::ui/style-fill)))
                 lbl (ui/center (ui/label (:label tab))
                                (ui/bounds background))
                 close (ui/on
                        :mouse-down
                        (fn [_]
                          [[:stop (:id tab)]])
                        (icon.ui/icon {:name "delete"
                                       :hover? (get extra [:delete-hover? (:id tab)])}))
                 [close-width close-height] (ui/bounds close)]
             [(ui/on
               :mouse-down
               (fn [_]
                 [[:toggle (:id tab)]])
               [background
                lbl])
              (ui/translate
               (- width 20)
               (- (/ tab-height 2)
                  (/ close-height 2))
               close)])))
    tabs)])

(defeffect ::save-workspace [{:keys [$easel]}]
  (dispatch! :update
             $easel
             (fn [easel]
               (update easel :workspaces
                       (fn [{:keys [last-id by-id] :as workspaces}]
                         (let [next-id (inc last-id)

                               root-pane (:root-pane easel)
                               new-workspace {:root-pane (dissoc root-pane :width :height)
                                              :id next-id}]
                           (-> workspaces
                               (update :by-id assoc next-id new-workspace)
                               (update :last-id inc))))))))

(defeffect ::clear-workspace [{:keys [$easel]}]
  (dispatch! :update
             $easel
             (fn [easel]
               (-> easel
                   (update :root-pane dissoc :panes)
                   relayout*))))

(defeffect ::select-workspace [{:keys [$easel workspace-id]}]
  (dispatch! :update $easel
             (fn [easel]
               (let [

                     workspace (-> easel :workspaces :by-id (get workspace-id))
                     workspace-root-pane (:root-pane workspace)

                     {:keys [width height]} (:root-pane easel)
                     workspace-root-pane (assoc workspace-root-pane
                                                :width width
                                                :height height)]
                 (assert workspace-root-pane)
                 (-> easel
                     (assoc :root-pane workspace-root-pane)
                     relayout*)))))

(defeffect ::delete-workspace [{:keys [$easel workspace-id]}]
  (dispatch! :update
             $easel
             (fn [easel]
               (update-in easel [:workspaces :by-id]
                          dissoc workspace-id))))

(defui string-editor [{:keys [string edit-string
                              last-click
                              ^:membrane.component/contextual
                              focus] :as m}]
  (let [focus? (= focus $string) ]
    (if focus?
      (ui/wrap-on
       :key-press
       (fn [handler s]
         (if (= s :enter)
           [[:set $focus nil]
            [:set $string edit-string]]
           (handler s)))
       (ui/horizontal-layout
        (ui/on
         ::basic/request-focus
         (constantly nil)
         (basic/textarea-view {:text edit-string
                               :focus? focus?
                               :cursor (get extra :cursor 0)
                               :border? true}))))
      (ui/on
       :mouse-down
       (fn [_]
         ;; TODO: correctly implement double click
         ;;       and select workspace after 500ms timeout.
         (let [t (.getTime ^java.util.Date (java.util.Date.))]
           (if (and last-click
                    (< (- t last-click)
                       500))
             [[:set $focus $string]
              [:set $edit-string string]]
             [[:set $last-click t]])))
       (ui/label string)))))

(defui workspace-view [{:keys [workspaces width]}]
  (ui/vertical-layout
   (ui/horizontal-layout
    (ui/on-click
     (fn []
       [[::save-workspace {}]])
     (icon.ui/icon {:name "save"}))
    (ui/on-click
     (fn []
       [[::clear-workspace {}]])
     (icon.ui/icon {:name "minus-circle"})))
   (let [by-id (:by-id workspaces)]
     (stretch/vlayout
      (map (fn [workspace-id]
             (let [workspace (get by-id workspace-id)
                   background (ui/rectangle width tab-height)
                   background (->> background
                                   (ui/with-style ::ui/style-stroke)
                                   (ui/with-color [0.33 0.33 0.33]))

                   editing-name? (get extra [::editing? (:id workspace)])

                   lbl (string-editor {:string (get workspace :name (str workspace-id))
                                       :editing editing-name?})
                   lbl (ui/center lbl
                                  (ui/bounds background))

                   close (ui/on
                          :mouse-down
                          (fn [_]
                            [[::delete-workspace {:workspace-id (:id workspace)}]])
                          (icon.ui/icon {:name "delete"
                                         :hover? (get extra [:delete-hover? (:id workspace)])}))
                   [close-width close-height] (ui/bounds close)]
               [(ui/on
                 :mouse-down
                 (fn [_]
                   [[::select-workspace {:workspace-id (:id workspace)}]])
                 background)
                lbl
                (ui/translate
                 (- width 20)
                 (- (/ tab-height 2)
                    (/ close-height 2))
                 close)])))
      (->> by-id
           (map first)
           sort)))))

(defn add-$easel
  "Adds the :$easel key to the set of intents."
  [$easel intents body]
  (ui/on-bubble
   (fn [intents']
     (specter/transform
      [specter/ALL
       (fn [intent]
         (intents
          (first intent)))]
      (fn [intent]
        [(first intent) (assoc (second intent) :$easel $easel)])
      intents'))
   body))

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
                  :width tab-width})
       (add-$easel
        $easel
        #{::save-workspace
          ::clear-workspace
          ::select-workspace
          ::delete-workspace}
        (workspace-view {:workspaces (:workspaces easel)
                         :width tab-width}))))
     (add-$easel
      $easel
      #{::toggle-pane-direction
        ::add-pane-child
        ::delete-pane
        ::clear-pane
        ::splitpane
        ::swap-panes
        ::toggle-pane-resize
        ::begin-resize-drag
        ::resize-drag
        ::end-resize-drag}
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
       :window-title "Easel"
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

  (tap> (easel-view @app-state))

  ,)

