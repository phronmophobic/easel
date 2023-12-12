(ns com.phronemophobic.easel
  (:require [membrane.skia :as skia]
            [membrane.ui :as ui]
            [membrane.alpha.stretch :as stretch]
            [tiara.data :as tiara]
            [com.rpl.specter :as specter]
            [clojure.java.io :as io]
            [com.phronemophobic.easel.list-applets :as
             list-applets]
            [com.phronemophobic.easel.term
             :as term]
            [com.phronemophobic.easel.model :as model]
            [com.phronemophobic.easel.browser :as browser]
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

(defeffect ::add-applet [make-applet]
  (dispatch!
   :update :easel
             (fn [easel]
               (model/-add-applet easel
                                  (make-applet dispatch!))))
  nil)

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

(defrecord AEasel [applets visible last-id $ref layout-direction size]
  model/IEasel
  (-add-applet [this applet]
    (let [cw (col-width this (inc (count visible)))
          ch (second size)
          new-size [cw ch]
          id (inc last-id)
          applet (assoc applet :id id)
          applet (model/-start applet (specter/path
                                        (membrane.component/path->spec $ref)
                                        (specter/keypath :applets id))
                               new-size)]

      (-> this
          (assoc :last-id id)
          (assoc-in [:applets id] applet)
          (update :visible conj id)
          (model/-resize size (:content-scale this)))))
  (-remove-applet [this id]
    (if-let [applet (get applets id)]
      (let [applet (try
                     (model/-stop applet)
                     (catch Exception e
                       (println "Error when closing applet:")
                       (prn e)))]
        (-> this
            (update :applets dissoc id)
            (update :visible disj id)))
      this))
  (-layout-direction [this]
    layout-direction)
  (-layout-direction [this layout-direction]
    (assoc this :layout-direction layout-direction))
  (-visible-applets [this]
    (into []
          (keep (fn [id]
                  (get applets id)))
          visible))
  (-show-applet [this id]
    (-> this
        (update :visible conj id)))
  (-hide-applet [this id]
    (-> this
        (update :visible disj id)))
  (-applets [this]
    applets)
  model/IResizable
  (-resize [this [w h] content-scale]
    (let [this (assoc this
                      :size [w h]
                      :content-scale content-scale)
          this (if (seq visible)
                 (let [col-width (int (/ w (count visible)))
                       applets (reduce (fn [applets k]
                                         (update applets k #(model/-resize % [col-width h] content-scale)))
                                       applets
                                       visible)]
                   (assoc this :applets applets))
                 this)]
      this)))

(defn make-easel []
  (map->AEasel
   {:applets (tiara/ordered-map)
    :visible (tiara/ordered-set)
    :last-id 0
    :layout-direction :horizontal}))



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
            (let [visible (:visible easel)
                  easel (if (contains? visible id)
                          (model/-hide-applet easel id)
                          (model/-show-applet easel id))]
              (model/-resize easel (:size easel) (:content-scale easel))))]])
      :stop
      (fn [id]
        [[:update $easel
          (fn [easel]
            (-> easel
                (model/-remove-applet id)
                (model/-resize (:size easel) (:content-scale easel))))]])
      (tab-view {:tabs (vals (model/-applets easel))
                 :selected (:visible easel)
                 :width tab-width}))
     (stretch/hlayout
      (map #(model/-ui % $context context))
      (model/-visible-applets easel)))))



(def pad 20)

(require 'com.phronemophobic.membrane.schematic3)
(def eval-ns (the-ns 'com.phronemophobic.membrane.schematic3))
(defn run []
  (let [
        #_#__ (swap! app-state
                     assoc :vt
                     (-start (->Termlet handler)
                             (specter/keypath :vt)))
        _ (swap! app-state
                 (fn [state]
                   (if state
                     state
                     {:easel (assoc (make-easel)
                                    :$ref (specter/keypath :easel))
                      :membrane.component/context {:eval-ns eval-ns}})))
        
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
         model/-add-applet (term/termlet handler))
  nil)

(defn add-browser [url]
  (swap! app-state
         update :easel
         model/-add-applet (browser/browslet handler url))
  nil)
(comment
  (do
    (run)
    (Thread/sleep 1000)
    (handler ::add-applet
             list-applets/list-applets))
  (reset! app-state nil)

  (add-term)
  (add-browser "https://phoboslab.org/xtype/")
  (add-browser "https://phoboslab.org/ztype/")
  (add-browser "https://www.youtube.com/")
  (add-browser "https://github.com/")


  (do (run) (add-browser "https://github.com/") (add-browser "https://github.com/"))
  ,)


(defn -main [& args]
  (do
    (run)
    (handler ::add-applet
             list-applets/list-applets))
  (Thread/sleep 5000)
  (add-browser "https://github.com/")

  (Thread/sleep 5000))

(comment

  (def pty

    (-> @app-state
        :easel
        :applets
        seq
        (nth 2)
        val
        :pty))
  (com.pty4j.unix.Pty/raise
   (int (.pid pty))
   (int 28))

           
  ,)

