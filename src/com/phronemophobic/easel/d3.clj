(ns com.phronemophobic.easel.d3
  (:require
   [membrane.component :refer
    [defui
     defeffect]]
   [membrane.ui :as ui]
   [com.phronemophobic.easel.model :as model]
   [com.phronemophobic.clj-webgpu.d3.skia :as d3]
   [com.phronemophobic.clj-libretro.skia :as retro-skia]
   [com.phronemophobic.clj-libretro.ui :as retro-ui]
   [com.phronemophobic.clj-libretro.api :as retro]
   [clj-manifold3d.core :as mf]
   [com.phronemophobic.membrandt :as ant]))


(defui d3 [{:keys [size state id]}]
  (let [focus (:focus context)
        focus? (= focus id)
        view (d3/view3d {:state state})]
    (ui/on
     :mouse-down
     (fn [_]
       [[:set $focus id]])
     (if focus?
       view
       (ui/no-events view)))))

(defn d3-ui [this $context context]
  (let [state (-> this
                  (assoc :context context
                         :$context $context
                         :extra (:extra this)
                         :$extra [(:$ref this) '(keypath :extra)]))]
    
    (d3/view3d state)))

(defrecord D3Widget [dispatch!]
  model/IApplet
  (-start [this $ref size _content-scale]
    (let [$state [$ref '(keypath :state)]
          mesh (mf/import-mesh "/Users/adrian/Downloads/Klein_Smoother_no_base.STL")]
      
      (assoc this
             :extra {}
             :$ref $ref
             :$state $state
             :size size
             ::model/queue
             [(fn []
                (dispatch! ::d3/animate-mesh {:$state $state
                                              :mesh mesh}))])))
  (-stop [this]
    (dispatch! ::d3/stop (:state this))
    nil)
  model/IUI
  (-ui [this $context context]
    (d3-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn d3-applet [handler]
  (-> (->D3Widget handler)
      (assoc :label (str "Klein Bottle") )))


