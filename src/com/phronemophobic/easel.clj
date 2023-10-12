(ns com.phronemophobic.easel
  (:require [membrane.skia :as skia]
            [membrane.ui :as ui]
            [asciinema.vt :as vt]
            [com.phronemophobic.membrane.term :as term]
            [membrane.alpha.stretch :as stretch]
            [tiara.data :as tiara]
            [com.rpl.specter :as specter]
            [clojure.java.io :as io]
            [clojure.core.async :as async]
            [com.phronemophobic.easel.model :as model]
            [membrane.component
             :refer
             [defui defeffect]])
  (:import [com.pty4j PtyProcess WinSize]))


(def repaint! @#'skia/glfw-post-empty-event)

(defonce app-state
  (atom nil))

(def handler (membrane.component/default-handler app-state))

(defeffect ::repaint! []
  repaint!)



(defrecord AEasel [applets visible last-id $ref layout-direction size]
  model/IEasel
  (-add-applet [this applet]
    (let [id (inc last-id)
          applet (assoc applet :id id)
          applet (model/-start applet (specter/path
                                       (membrane.component/path->spec $ref)
                                       (specter/keypath :applets id :state))) ]

      (-> this
          (assoc :last-id id)
          (assoc-in [:applets id] applet)
          (update :visible conj id))))
  (-remove-applet [this id]
    (if-let [applet (get applets id)]
      (let [applet (model/-stop applet)]
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
  (-resize [this w h]
    (let [this (assoc this :size [w h])
          this (if (seq visible)
                 (let [col-width (int (/ w (count visible)))
                       applets (reduce (fn [applets k]
                                         (update applets k #(model/-resize % col-width h)))
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

(def term-events #'term/term-events)
(def term-view @#'term/term-view)

(def menlo
  (#'term/load-terminal-font skia/toolkit
                             "Menlo"
                             11))

(defn term-ui [this $context context]
  (let [focus (:focus context)
        focus? (= focus (:id this))
        view
        (ui/on
         :mouse-down
                (fn [_]
                  [[:set [$context (list 'keypath :focus)]
                    (:id this)]])
         (term-view term/default-color-scheme
                    menlo
                    (:state this)))
        view (if focus?
               (ui/wrap-on
                :key-event
                (fn [handler key scancode action mods]
                  (when (not= 39 key)
                    (handler key scancode action mods)))
                (term-events (:pty this)
                             view))
               view)
        view (ui/fill-bordered
              (if focus?
                [0.8 0.8 0.8]
                [0.95 0.95 0.95])
              10
              view)]
    view))

(defrecord Termlet [dispatch!]
  model/IApplet
  (-start [this $ref]
    (let [cmd (into-array String ["/bin/bash" "-l"])
          pty (PtyProcess/exec ^"[Ljava.lang.String;" cmd
                               ^java.util.Map (merge (into {} (System/getenv))
                                                     {"TERM" "xterm-256color"}))
          width 90
          height 30
          pty (doto pty
                (.setWinSize (WinSize. width height)))
          is (io/reader (.getInputStream pty))
          os (.getOutputStream pty)]
      (async/thread
        (try
          (with-open [is is]
            (loop []
              (let [input (.read is)]
                (when (not= -1 input)
                  (dispatch! :update $ref
                             (fn [vt]
                               (vt/feed-one vt input)))
                  (dispatch! ::repaint!)
                  (recur)))))
          (catch Exception e
            (prn e))))
      (assoc this
             :state (vt/make-vt width height)
             :pty pty
             :os os
             :is is)))
  
  
  (-stop [this]
    (.destroy (:pty this))
    (.close (:os this)))
  (-ui [this $context context]
    (term-ui this $context context))
  model/IResizable
  (-resize [this w h]
    (let [w (- w 20)
          h (- h 20)
          cols (int (/ w (:membrane.term/cell-width menlo)))
          rows (int (/ h (:membrane.term/cell-height menlo)))]
      (.setWinSize (:pty this)
                   (WinSize. cols rows))
      (assoc this :state (vt/make-vt cols rows)))))

(defn termlet []
  (-> (->Termlet handler)
      (assoc :label "term")))

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
                                  (ui/with-style ::ui/style-fill))
                             )
                lbl (ui/center (ui/label (:label tab))
                               (ui/bounds background))]
            (ui/on
             :mouse-down
             (fn [_]
               [[:toggle (:id tab)]])
             [background
              lbl]))))
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
                          (model/-show-applet easel id)
                          )
                  [w h] (:size easel)]
              (model/-resize easel w h)))]])
      (tab-view {:tabs (vals (model/-applets easel))
                 :selected (:visible easel)
                 :width tab-width}))
     (stretch/hlayout
      (map #(model/-ui % $context context))
      (model/-visible-applets easel)))))



(def pad 20)
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
                                    :$ref (specter/keypath :easel))})))

        app (membrane.component/make-app #'easel-view app-state handler)]
    (skia/run app
      {:include-container-info true
       :handlers
       {:reshape
        (fn [window window-handle width height]
          (let [[xscale yscale] (skia/get-window-content-scale-size window-handle)
                width (int (/ width xscale))
                height (int (/ height yscale))]
            (swap! app-state update :easel
                   model/-resize
                   (- width tab-width)
                   height))
          (skia/-reshape window window-handle width height))}})))


(defn add-term []
  (swap! app-state
         update :easel
         model/-add-applet (termlet))
  nil)
