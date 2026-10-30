(ns com.phronemophobic.easel.scad
  (:require [clojure.core.async :as async]
            [com.phronemophobic.clj-webgpu.d3 :as d3]
            [com.phronemophobic.clj-webgpu.compute :as wgpu]
            [com.phronemophobic.clj-webgpu.camera.orbit :as orbit]
            [clj-manifold3d.core :as mf]
            [clojure.core.matrix :as mat]
            [com.phronemophobic.clj-webgpu.d3.matrix-util :as mutil]
            [membrane.component :refer [defeffect]]
            [membrane.ui :as ui]
            [membrane.skia.paragraph :as para]
            [membrane.component :refer [defui
                                        defeffect]]
            [tech.v3.datatype.ffi :as dt-ffi]
            [tech.v3.datatype :as dtype]
            [com.phronemophobic.membrandt.impl.grid :as grid]
            [com.phronemophobic.easel :as easel]
            [com.phronemophobic.easel.model :as easel.model]
            [com.phronemophobic.membrandt :as ant]
            [com.phronemophobic.membrandt.icon.ui :as icon.ui])
  (:import java.util.concurrent.Executors
           java.util.concurrent.ExecutorService))

(defonce ^:private executor3d 
  (delay 
    (Executors/newSingleThreadExecutor)))

;; This allows us to treat the pixmap with identity semantics
;; otherwise, we have to pay the cost of hashing all of the pixel
;; data each frame.
(deftype WrapPixmap [pm]
  ui/IOrigin
  (-origin [_]
    [0 0])
  
  ui/IBounds
  (-bounds [_]
    [(:width pm)
     (:height pm)])
  
  clojure.lang.IDeref
  (deref [_] pm)
  
  ui/IChildren
  (-children [this]
    [pm])

  clojure.lang.IHashEq
  (hasheq [_] (System/identityHashCode pm))
  (hashCode [_] (System/identityHashCode pm))
  (equals [this that]
    (if (instance? WrapPixmap that)
      (identical? pm (deref that))
      false)))


(def ^:private ^:dynamic *in-queue* false)
(defmacro ^:private enqueue [& body]
  `(if *in-queue*
     (do ~@body)
     (^[java.util.concurrent.Callable]
      ExecutorService/.submit @executor3d
      (fn []
        (try
          (binding [*in-queue* true]
            ~@body)
          (catch Throwable t#
            (tap> t#)))))))

(defn grid-draw []
  (let [camera-inverse-buf (dtype/make-container :native-heap
                                                 :float32 
                                                 16)
        grid-size-buf (dtype/make-container :native-heap
                                            :float32 
                                            [10])]
    {:id ::grid-draw
     :type :render-shader
     :bindings {:camera-inverse {:type :buffer
                                 :opts {:usage #{:Uniform
                                                 :CopyDst}
                                        :type :f32
                                        :length 16}}
                :grid-size {:type :buffer
                            :opts {:usage #{:Uniform
                                            :CopyDst}
                                   :type :f32
                                   :length 1}
                            :data grid-size-buf}}
     
     :drawf (fn [ctx bindings]
              {:vertex-count 6
               :data {(:camera-inverse bindings)
                      (dtype/copy! 
                       (into []
                             cat
                             (mat/transpose
                              (mat/inverse
                               (let [camera-mat (mutil/->matrix (:camera ctx))
                                     projection-mat (mutil/->matrix (assoc (:projection ctx)
                                                                           :width (:width ctx)
                                                                           :height (:height ctx)))
                                     mat (mat/mmul
                                          projection-mat
                                          (mat/inverse camera-mat))]
                                 mat))))
                       camera-inverse-buf)}
               :bindings [(-> ctx :uniform :view+projection)
                          (:camera-inverse bindings)
                          (:grid-size bindings)]})
     :shader {:resource "../clj-webgpu/shaders/grid.wgsl"}}))

;; should only be run inside executor3d
(defn ^:private calc-pm [{:keys [pm ctx3d mesh mesh-id camera size] :as m}]
  (let [[width height] size
        ctx3d (or ctx3d
                  (-> (d3/init! {:width (long width) :height (long height)})
                      (assoc ::grid-draw (grid-draw))))
        draw (when mesh
               {:id mesh-id
                :type :mesh
                :mesh mesh})
        
        
        ctx3d (d3/draw! ctx3d
                        {:camera camera
                         :projection {:type :projection/perspective}
                         :draw
                         [draw
                          (::grid-draw ctx3d)]})
        pm (if pm
             (do (d3/ctx->pixmap ctx3d @pm)
                 pm)
             (->WrapPixmap (d3/ctx->pixmap ctx3d nil)))]
    {:pm pm
     :old-pm nil
     :mesh mesh
     :mesh-id mesh-id
     :ctx3d ctx3d}))

;; should only be run inside executor3d
(defeffect ::redraw [{:keys [$ref]}]
  (enqueue
   (let [applet (dispatch! :get $ref)
         m (calc-pm applet)]
     ;; updating the pixmap takes a while, so we don't want to
     ;; do it inside of the update function
     ;; this should only be run on the app queue
     ;; to ensure consistency
     (dispatch! :update $ref
                (fn [applet]
                  (merge applet m))))
   (dispatch! :repaint!)))

(defeffect ::reset-camera [{:keys [mesh $camera $ref]}]
  (let [{:keys [minx miny minz
                maxx maxy maxz]
         :as exs}
        (d3/extents2 mesh)
        
        radius (* 3
                  (max
                   (- maxx minx)
                   (- maxy miny)
                   (- maxz minz)))
        target [(+ minx (/ (- maxx minx) 2))
                (+ miny (/ (- maxy miny) 2))
                (+ minz (/ (- maxz minz) 2))]
        camera (orbit/make {:radius radius
                            :target target
                            :tilt (/ Math/PI -4)})]
    (dispatch! :set $camera camera)
    (dispatch! ::redraw {:$ref $ref})))

(defeffect ::update-mesh [{:keys [$ref mesh]}]
  (let [{:keys [minx miny minz
                maxx maxy maxz]
         :as exs}
        (d3/extents2 mesh)
        
        radius (* 3
                  (max
                   (- maxx minx)
                   (- maxy miny)
                   (- maxz minz)))
        target [(+ minx (/ (- maxx minx) 2))
                (+ miny (/ (- maxy miny) 2))
                (+ minz (/ (- maxz minz) 2))]
        camera (orbit/make {:radius radius
                            :target target
                            :tilt (/ Math/PI -4)})]
    (dispatch! :update $ref
                     (fn [applet]
                       (-> applet
                           (assoc :camera camera
                                  :mesh mesh)
                           (update :mesh-id inc))))
    
    (dispatch! ::redraw {:$ref $ref})
    (enqueue
     (wgpu/run-clean-queue!))))




(defn debounce-chan
  "Reads from channel `in`. Will wait `ms` milliseconds
  and write the latest value read from `in` to `out`."
  [in out ms]
  (async/go
    (loop []
      (let [x (async/<! in)]
        (when x
          (loop [to (async/timeout ms)
                 x x]
            (async/alt!
              to ([_] (async/>! out x))
              in ([x] (recur to x))))
          (recur))))))

(defn resize-chan [dispatch! $ref]
  (let [debounced (async/chan (async/sliding-buffer 1))
        in (async/chan)]
    (debounce-chan in debounced 200)
    (async/go
      (loop []
        (when-let [[width height] (async/<!! debounced)]
          (enqueue
           (dispatch! :update $ref 
                      (fn [applet]
                        (-> applet
                            (dissoc :ctx3d)
                            (assoc :old-pm (:pm applet))
                            (dissoc :pm))))
             (dispatch! ::redraw {:$ref $ref}))
          (recur))))
    in))


(defeffect ::mouse-down [{:keys [mpos ui-state $ui-state camera]}]
  (dispatch! :update $ui-state
             assoc
             :mdown mpos
             :camera camera))


(defeffect ::mouse-move [{:keys [$ref mesh mpos ui-state $ui-state camera $camera]}]
  (let [[x1 y1] (:mdown ui-state)
        [x2 y2] mpos
        
        factor 0.001
        radius  (:radius camera)
        pdelta (* 0.01  (- x2 x1))
        ptilt (* 0.01  (- y1 y2))
        
        ptrack (mapv #(* factor radius %) [(- x1 x2 )
                                  (- y2 y1 )])]
      (dispatch! :set $camera 
                 (if (:shift? ui-state)
                   (-> (:camera ui-state) 
                       (orbit/track ptrack))
                   (-> (:camera ui-state) 
                       (orbit/pan pdelta)
                       (orbit/tilt ptilt))))
    (dispatch! ::redraw {:$ref $ref})))

(defeffect ::mouse-up [{:keys [$ui-state]}]
  (dispatch! :update $ui-state dissoc :mdown))

(defeffect ::scroll [{:keys [$ref mesh camera $camera scroll]}]
  (let [[sx sy] scroll
        radius (:radius camera)
        ddolly (* radius 0.008 (- sy))]
    (dispatch! :update $camera 
               (fn [camera]
                 (-> camera
                     (orbit/dolly ddolly ))))
    
    (dispatch! ::redraw {:$ref $ref})))

(defui model-viewer [{:keys [model pm old-pm ui-state camera $ref mesh]}]
  [(ui/on
   :key-event
   (fn [key scancode action mods]
     (when (= 340 key)
       (cond
         (= action :press)
         [[:update $ui-state assoc :shift? true]]
         
         (= action :release)
         [[:update $ui-state assoc :shift? false]]
         
         :else nil)))
   
   :scroll
   (fn [scroll _]
     [[::scroll {:scroll scroll
                 :camera camera
                 :$ref $ref
                 :mesh mesh
                 :$camera $camera}]])

   :mouse-down
   (fn [mpos]
     [[::mouse-down {:mpos mpos
                     :camera camera
                     :$camera $camera
                     :ui-state ui-state
                     :$ui-state $ui-state}]])
   :mouse-up 
   (fn [mpos]
     [[::mouse-up {:mpos mpos
                    :camera camera
                     :$camera $camera
                     :ui-state ui-state
                     :$ui-state $ui-state}]])
   :mouse-move
   (fn [mpos]
     (when (:mdown ui-state)
       [[::mouse-move {:mpos mpos
                       :camera camera
                       :$ref $ref
                       :mesh mesh
                       :$camera $camera
                       :ui-state ui-state
                       :$ui-state $ui-state}]]))
   
   
    (or pm
       old-pm))
   (ant/button {:text "reset camera"
                :on-click
                (fn []
                  [[::reset-camera {:$camera $camera
                                    :$ref $ref
                                    :mesh mesh}]])})])



(defrecord ModelViewerApplet [dispatch! mesh-id mesh]
  easel.model/IApplet
  (-start [this {:keys [$ref size $shared] :as info}]
    
    (let [[width height] size
          camera (orbit/make {:radius 5
                              :tilt (/ Math/PI -4)})]
      (assoc this
             ::easel.model/queue
             [(fn []
                (enqueue
                 (dispatch! ::update-mesh {:$ref $ref
                                           :mesh mesh})))]
             :label "scad"
             :camera camera
             :$camera [$ref '(keypath :camera)]
             :$pm [$ref '(keypath :pm)]
             :$model [$ref '(keypath :model)]
             :$ctx3d [$ref '(keypath :ctx3d)]
             :ui-state {}
             :$ui-state [$ref '(keypath :ui-state)]
             :extra {}
             :$extra [$ref '(keypath :extra)]
             :$ref $ref
             :resize-chan (resize-chan dispatch! $ref)
             :$shared $shared
             :size size)))
  (-stop [this]
    (enqueue
     (wgpu/run-clean-queue!))
    nil)
  easel.model/IUI
  (-ui [this ui-info]
    (model-viewer 
     (assoc this
            :context (:context ui-info)
            :$context (:$context ui-info))))
  easel.model/IResizable
  (-resize [this size _content-scale]
    (let [[width height] size
          this (assoc this :size size)]
      (async/put! (:resize-chan this) size)
      this)))

(defn show-model-viewer [model]
  
  (let [mesh (if (instance? manifold3d.Manifold model)
               (mf/get-mesh model)
               model)]
    ((requiring-resolve 'com.phronemophobic.easel/add-applet)
     {:id ::id
      :update-applet
      (fn [applet]
        (let [{:keys [$ref dispatch!]} applet]
          (assoc applet
                 ::easel.model/queue
                 [(fn []
                    (enqueue
                     (dispatch! ::update-mesh {:$ref $ref
                                               :mesh mesh})))])))
      :make-applet
      #(->ModelViewerApplet % 1 mesh)}))
  nil)

(defeffect ::show-model-viewer [{:keys [model]}]
  (show-model-viewer model))


(comment
  (show-model-viewer
   (mf/scale (mf/union
              (mf/cube 1 2 3  )
              (mf/sphere 1 100))
             [10 10 10]))
  
  (show-model-viewer
   (-> (mf/slice 
        (mf/union
         (mf/sphere 5 100 )
         (mf/cube [5 6 7  ])))
       (clj-manifold3d.impl/to-polygons)
       (mf/translate [10 0])
       (mf/revolve 100 180)))
  
  
  (show-model-viewer
   (mf/import-mesh "models/PolyGear.stl"))
  
  (show-model-viewer
   (mf/import-mesh "models/EiffelTower_fixed.stl"))
  
  (show-model-viewer
   (mf/import-mesh 
    "models/Cummins_5.9.stl"))
  
  (show-model-viewer
   (mf/union
    (mf/translate
     (mf/manifold
      (mf/import-mesh 
       "models/Allosaurus_Head_For_Wall.stl"))
     [340 -200 0])
    (mf/manifold
     (mf/import-mesh 
      "models/david bust sliced - main part.stl"))))
  
  
  (show-model-viewer
   (let [cyl (mf/cylinder 10 1 1 100 true)]
     (mf/union
      cyl
      (mf/rotate cyl
                 [0 90 0])
      (mf/rotate cyl
                 [90 0 0]))))
  
  (show-model-viewer
   (-> (let [c (mf/difference (mf/square 10 10 true) (mf/square 8 8 true))]
         (mf/loft [c (mf/scale c [1.5 1.5]) c]
                  [(mf/frame 1)
                   (mf/translate (mf/frame) [0 0 15])
                   (mf/translate (mf/frame) [0 0 30])]))))
  
  (show-model-viewer
   (-> (mf/text "/System/Library/Fonts/Supplemental/Zapfino.ttf"
                "Clojure! " 10 20 :non-zero
                )
       (mf/scale-to-height 10)
       (mf/extrude 1)))
  
  
  (defn sinewave-heatmap
    "Generates a 3D sinewave heatmap.
    The output is a vector of vectors representing a square matrix.
    Each cell represents the height at that x/y coordinate based on a sinewave.
    
    Args:
    - size: The size of the matrix (width and height).
    - frequency: Frequency of the sinewave (controls the number of wave oscillations).
    - amplitude: Amplitude of the sinewave (controls the height of the wave).
    - phase: Phase shift of the sinewave.
    
    Returns a matrix where each value is the sinewave height at that coordinate."
    [size step frequency amplitude phase]
    (vec
     (for [y (range 0 size step)]
       (vec
        (for [x (range 0 size step)]
          (+ (+ 0.1 amplitude)
             (* amplitude
                (Math/sin
                 (+ (* frequency (/ x size))
                    (* frequency (/ y size))
                    phase)))))))))
  
  (show-model-viewer
   (mf/rotate
    (-> (sinewave-heatmap 10 0.05 50 5 0)
        (mf/surface 1.0))
    [180 90 -90]))
  
  
  (show-model-viewer
   (mf/loft
    (reductions
     (fn [m i]
       (if (odd? i)
         (assoc m :frame (mf/translate (mf/frame 1) [10 0 (* i 10)]))
         (assoc m :frame (mf/translate (mf/frame 1) [-10 0 (* i 10)]))))
     {:cross-section (mf/square 20 20 true)
      :frame (mf/frame 1)}
     (range 10))))
  
  ,)



