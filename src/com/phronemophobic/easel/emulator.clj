(ns com.phronemophobic.easel.emulator
  (:require
   [membrane.component :refer
    [defui
     defeffect]]
   [membrane.ui :as ui]
   [clojure.java.io :as io]
   [membrane.alpha.component.drag-and-drop :as dnd]
   [com.phronemophobic.easel.model :as model]
   [membrane.basic-components :as basic]
   [clojure.core.async :as async]
   [clojure.set :as set]
   [com.phronemophobic.viscous :as viscous]
   [com.phronemophobic.clj-libretro.skia :as retro-skia]
   [com.phronemophobic.clj-libretro.ui :as retro-ui]
   [com.phronemophobic.clj-libretro.api :as retro]
   [com.phronemophobic.membrandt :as ant]))


(defui emulator-view [{:keys [size view id]}]
  (let [focus (:focus context)
        focus? (= focus id)]
    (ui/on
     :mouse-down
     (fn [_]
       [[:set $focus id]])
     (if focus?
       view
       (ui/no-events view)))))

(defn emulator-ui [this $context context]
  (let [state (-> this
                  (assoc :context context
                         :$context $context
                         :extra (:extra this)
                         :$extra [(:$ref this) '(keypath :extra)]))]
    
    (emulator-view state)))


(defn hud [iretro pm]
  (ui/vertical-layout
   (ui/horizontal-layout
    #_(ui/button "save"
                 (fn []
                   (save-game iretro "ui.save")
                   nil))
    #_(ui/button "load"
                 (fn []
                   (load-save iretro "ui.save")
                   nil)))
   pm))

(defrecord EmulatorWidget [dispatch!]
  model/IApplet
  (-start [this $ref size _content-scale]
    (let [$close-fn [$ref '(keypath :close-fn)]
          view-fn (atom nil)
          $view [$ref '(keypath :view)]]
      (assoc this
             :extra {}
             :$ref $ref
             ::model/queue
             [(fn []
                (future
                  (try
                    (println "loading core.")
                    (let [core (com.phronemophobic.clj-libretro.api/load-core "fceumm")]
                      (retro-ui/play-game
                       core
                       (.getCanonicalPath
                        (io/file ".."
                                 "tom7"
                                 "Officially licensed games"
                                 "Tetris (USA).nes"))
                       {:run-with-close-handler
                        (fn [view opts close-handler]
                          (dispatch! :set $close-fn close-handler)
                          (reset! view-fn view)
                          nil)
                        :render-frame retro-skia/render-frame
                        :hud #(hud core %)
                        :->repaint!
                        (fn [_]
                          (fn []
                            (when-let [view-fn @view-fn]
                              (dispatch! :set $view (view-fn))
                              (dispatch! :repaint!))))}))
                    (catch Exception e
                      (prn e)))))]
             :size size)))
  (-stop [this]
    (when-let [close-fn (:close-fn this)]
      (close-fn))
    nil)
  model/IUI
  (-ui [this $context context]
    (emulator-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn emulator-applet [handler]
  (-> (->EmulatorWidget handler)
      (assoc :label (str "Tetris") )))


