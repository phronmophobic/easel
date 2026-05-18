(ns com.phronemophobic.easel.flow
  (:require [com.phronemophobic.clj-graphviz :as graphviz]
            [membrane.component :refer [defui
                                        defeffect]]
            [membrane.basic-components :as basic]
            [com.phronemophobic.membrandt :as ant]
            [com.phronemophobic.membrandt.icon.ui :as icon.ui]
            [com.phronemophobic.easel.model :as model]
            [euclidean.math.vector :as v]
            [euclidean.math.quaternion :as q]
            [membrane.alpha.component.drag-and-drop :as dnd]
            [membrane.ui :as ui]
            [clojure.string :as str]
            [clojure.math :as math]
            [com.phronemophobic.viscous :as viscous]
            [clojure.core.async :as async]
            [clojure.core.async.flow :as flow]
            [clojure.core.async.flow.spi :as flow.spi]))

(defn monitoring [{:keys [report-chan error-chan]}]
  (prn "========= monitoring start")
  (async/thread
    (loop []
      (let [[val port] (async/alts!! [report-chan error-chan])]
        (if (nil? val)
          (tap> "========= monitoring shutdown")
          (do
            (tap> [:flow-reporting
                   {:port (if (= port error-chan) :error-chan :report-chan)
                    :val val}])
            (recur))))))
  nil)

(def zaxis (v/vector 0 0 1))
(defn rotate-pi [v pis]
  (q/rotate (q/from-angle-axis (* Math/PI pis)
                               zaxis)
            (v/vector (v/get-x v)
                      (v/get-y v)
                      0)))

(defn arrow
  ([[hx hy :as head] [tx ty :as tail]]
   (let [p (v/->Vector2D (- hx tx) (- hy ty))
         x (v/get-x p)
         y (v/get-y p)

         arrow1 (-> p
                    v/normalize
                    (rotate-pi 3/4)
                    (v/scale 10))

         arrow2 (-> p
                    v/normalize
                    (rotate-pi -3/4)
                    (v/scale 10))]
     (ui/with-style :membrane.ui/style-stroke
                    (ui/translate
                     tx ty
                     [(ui/path [0 0]
                               [x y])
                      (ui/translate x y
                                    [(ui/path [0 0]
                                              [(v/get-x arrow1)
                                               (v/get-y arrow1)])
                                     (ui/path [0 0]
                                              [(v/get-x arrow2)
                                               (v/get-y arrow2)])])])))))


(defn wrap-producer
  "given a process map. Return a process that allows process to recur when state contains a true ::produce key."
  [{:keys [describe init transition transform]}]
  {:describe describe
   :init
   (fn [m]
     (let [ch (async/chan (async/sliding-buffer 1))]
       (-> (if init (init m) m)
           (update ::flow/in-ports
                   assoc
                   ::kickstart ch)
           (update ::flow/out-ports
                   assoc ::recur ch))))
   :transition
   (fn [state status]
     (let [state (if transition
                   (transition state status)
                   state)]
       (when (and (::produce state)
                  (= status ::flow/resume))
         (let [kickstart-ch (-> state
                                ::flow/in-ports
                                ::kickstart)]
           (async/>!! kickstart-ch true)))
       (when (= state ::flow/stop)
         (let [kickstart-ch (-> state
                                ::flow/in-ports
                                ::kickstart)]
           (async/close! kickstart-ch true)))
       state))
   :transform
   (fn [state in msg]
     (let [[state outs] (transform state in msg)
           
           outs (if (::produce state)
                  (conj (into [] outs)
                        [::recur [true]])
                  outs)]
       [state outs]))})


(defn port-id->str [[pid port :as o]]
  (str/join
   "::"
   (eduction
    (map name)
    [pid port])))

(defn str->coord [s]
  (if (str/includes? s "::")
    (into []
          (map keyword)
          (str/split s #"::"))
    (keyword s)))

(defn flow->graph [g]
  (let [subgraphs
        (into []
              (comp
               (map (fn [[pid proc-map]]
                      (let [proc (clojure.datafy/datafy (:proc proc-map))
                            proc (if-let [desc (:desc proc)]
                                   desc
                                   proc)
                            {:keys [ins outs]} proc]
                        {:graph {:color "black"}
                         :id (str "cluster-" (name pid))
                         :edges 
                         (into []
                               (concat
                                (eduction
                                 (map (fn [port]
                                        [(port-id->str [pid port])
                                         (name pid)]))
                                 (keys ins))
                                (eduction
                                 (map (fn [port]
                                        [(name pid)
                                         (port-id->str [pid port])]))
                                 (keys outs))))}))))
              (:procs g))
        external-edges (into []
                             (map
                              (fn [[[pid1 port1 :as coord1] [pid2 port2 :as coord2]]]
                                [(port-id->str coord1)
                                 (port-id->str coord2)]))
                             (:conns g))
        nodes (into #{}
                    (comp
                     (mapcat (fn [[pid proc-map]]
                               (let [proc (clojure.datafy/datafy (:proc proc-map))
                                     proc (if-let [desc (:desc proc)]
                                            desc
                                            proc)
                                     {:keys [ins outs]} proc]
                                 
                                 (cons 
                                  (name pid)
                                  (eduction
                                   
                                   (mapcat keys)
                                   (map (fn [port]
                                          (port-id->str [pid port])))
                                   [ins outs]))))))
                    (:procs g))]
    {:nodes nodes
     :edges external-edges
     :subgraphs subgraphs}))



(defui helper-viewer [{:keys [view]}]
  view)

(defn show! [view]
  (let [applet (->> @com.phronemophobic.easel/app-state
                    :easel
                    :applets
                    vals
                    (keep (fn [applet]
                            (when (= #'helper-viewer 
                                     (:component-var applet))
                              applet)))
                    first)
        $ref (:$ref applet)]
    (com.phronemophobic.easel/handler 
     :update $ref
     assoc-in [:state :view]
     view)))



(comment

  (dev/add-component-as-applet #'helper-viewer {:view nil})

  (graphviz/render-graph 
   flow-graph)
  
  (graphviz/layout flow-graph)
  
  (graphviz/layout {:edges [["a" "b"]]})
  
  (defn rg 
    ([g]
     (rg g {}))
    ([g opts]
     (graphviz/render-graph g opts)
     (easel/handler :set $view
                    (ui/image 
                     (#'skia/slurp-bytes
                      "graph.png")))))
  
  
  
  ,)

(defn render-layout [{:keys [nodes edges subgraphs colors status ping]} ]
  (let [dpi 72
        nodes (into []
                    (map (fn [{:keys [width height id x y]}]
                           (let [[w h] (mapv #(* dpi %) [width height])
                                 ;; _ (dev/dtap ping id)
                                 
                                 color (if (and 
                                            (not (str/includes? id "::"))
                                            (not= :running
                                                 (-> ping (get (keyword id)) ::flow/status)))
                                         [0.9 0.9 0.9]
                                         [1 1 1])]
                             (ui/translate (- x (quot w 2)) (- y (quot h 2))
                                           (dnd/on-drop
                                            (fn [pos obj]
                                              [[::drop-val {:drop-object obj
                                                            :id id}]])
                                            (ui/on
                                             :mouse-down
                                             (fn [_]
                                               [[::select-node {:id id}]
                                                [::dnd/drag-start {:id id}]])
                                             [(ui/with-style
                                               ::ui/style-fill
                                               (ui/with-color
                                                color
                                                (ui/rounded-rectangle w h (quot (min w h) 4))))
                                              (ui/center
                                               (ui/with-style
                                                ::ui/style-fill
                                                (ui/label
                                                 (if-let [port-status (get status id)]
                                                   (str id "\n"
                                                        (:count port-status)
                                                        "/"
                                                        (:capacity port-status))
                                                   id)))
                                               [w h])
                                              (ui/rounded-rectangle w h (quot (min w h) 4))
                                              ]))))))
                    nodes)
        edges (into []
                    (map (fn [{:keys [beziers from to]}]
                           (let [points (:points (first beziers))
                                 {x1 :x y1 :y} (first points)
                                 {x2 :x y2 :y} (last points)]
                             (arrow [x2 y2] [x1 y1])
                             #_(ui/path [x1 y1] [x2 y2]))))
                    edges)
        subgraphs (into []
                        (map (fn [{:keys [bounding-box]}]
                               (let [{:keys [lower-left upper-right]} bounding-box
                                     {llx :x
                                      lly :y} lower-left
                                     {uux :x
                                      uuy :y} upper-right]
                                 (ui/translate 
                                  llx lly
                                  (ui/with-style
                                   ::ui/style-stroke
                                   (ui/rectangle (- uux llx) (- uuy lly)))))))
                        subgraphs)]
    (ui/with-style 
     ::ui/style-stroke
     (ui/with-color [0 0 0]
                    [subgraphs edges nodes]))))

(comment
  
  (with-meta
   (ui/->Cached
    (render-layout (graphviz/layout flow-graph)))
   {:view true})
  
  (-> x-149459 
      flow->graph
      ;; graphviz/layout
      ;; render-layout
      )

  ,)



(defeffect ::cleanup [{:keys [$ref flow]}]
  (when flow
    (flow/stop flow))
  (dispatch! :update $ref
             (fn [applet]
               (dissoc applet
                       :layout-size
                       :flow-config
                       :flow-layout
                       ::ping
                       ::status))))

(defn watch-flow [flow]
  (let [ch (async/chan (async/sliding-buffer 1))]
    (async/thread
     (try
       (loop []
         (when-let [status (try
                             (flow/ping flow)
                             (catch Exception e
                               (when-not (= "flow not running" (ex-message e))
                                 (throw e))
                               nil))]
           (when (async/>!! ch status)
             (println "status")
             (Thread/sleep 1000)
             (recur))))
       (catch Exception e
         (prn e))))
    ch))

(defn flow? [o]
  (instance? clojure.core.async.flow.impl.graph.Graph
             o))

(defeffect ::update-flow-config [{:keys [$ref drop-object] :as this}]
  (when-let [drop-val* (:x drop-object)]
    (dispatch! ::cleanup this)
    (let [drop-val @drop-val*
          
          flow (when (flow? drop-val)
                 drop-val)
          flow-config (if flow
                        (clojure.datafy/datafy flow)
                        drop-val)

          flow-layout (-> flow-config
                          flow->graph
                          graphviz/layout)
          
          layout-size (transduce
                       (comp
                        (map :bounding-box)
                        (map :upper-right))
                       (completing
                        (fn [[w h] {:keys [x y] :as m
                                    }]
                          [(max w x) (max h y)]))
                       [0 0]
                       (:subgraphs flow-layout))
          
          ]
      (when flow
        (dispatch! ::watch-flow {:flow flow
                                 :$ref $ref}))
      
      (dispatch! :update $ref
                 (fn [applet]
                   (let [applet (assoc applet
                                       :layout-size layout-size
                                       :flow-config flow-config
                                       :flow-layout flow-layout)
                         applet (if flow
                                  (assoc applet :flow flow)
                                  applet)]
                     applet))))))

(defeffect ::start-flow [{:keys [flow-config $ref]}]
  (let [flow (flow/create-flow flow-config)]
    (dispatch! :update $ref
               assoc
               :flow flow
               ::ping {}
               ::status {})
    (-> (flow/start flow)
        monitoring)))

(defeffect ::pause-flow [{:keys [flow]}]
  (flow/pause flow))

(defeffect ::clear-flow [{:as this}]
  (dispatch! ::cleanup this))





(defn in-channel-buffers [flow-ping]
  (into {}
        (comp
         (map val)
         (mapcat
          (fn [{::flow/keys [pid ins]}]
            (eduction
             (map (fn [[port {:keys [buffer]}]]
                    [(port-id->str [pid port]) buffer]))
             ins))))
        flow-ping))

(defeffect ::watch-flow [{:keys [flow $ref]}]
  (let [ch (watch-flow flow)]
    (async/go
     (loop []
       (when-let [status (async/<! ch)]
         (dispatch! :update $ref
                    (fn [state]
                      (-> state
                          (update ::status merge (in-channel-buffers status))
                          (update ::ping 
                                  (fn [last-ping]
                                    (merge-with merge last-ping status))))))
         (recur))))))

(defeffect ::resume-flow [{:keys [flow $ref] :as m}]
  (dispatch! ::watch-flow m)
  (flow/resume flow))

(defeffect ::stop-flow [{:keys [flow]}]
  (flow/stop flow))

(defeffect ::inject-flow [{:keys [flow id drop-object] :as m}]
  (when-let [val (:x drop-object)]
    (flow/inject flow (str->coord id) [@val])))

(defeffect ::toggle-proc [{:keys [flow id ping $ref]}]
  (let [pid (keyword id)
        running? (= :running
                    (-> ping (get pid) ::flow/status))]
    (dev/dtap ping)
    (if running?
      (do
        (dispatch! :update $ref
                   assoc-in
                   [::ping pid ::flow/status] :paused)
        (flow/pause-proc flow (keyword id)))
      (do 
        (dispatch! :update $ref
                   assoc-in
                   [::ping pid ::flow/status] :running)
        (flow/resume-proc flow (keyword id))))))

(defui flow-view [{:keys [size flow-config
                          flow
                          flow-layout
                          layout-size] :as this}]
  
  (let [[cw ch] size
        colors (get extra :colors)
        $ref (:$ref this)
        header 
        (ui/vertical-layout
         (ui/horizontal-layout
           (ant/button {:text "start"
                        :on-click
                        (fn []
                          [[::start-flow this]])})
           (ant/button {:text "pause"
                        :on-click
                        (fn []
                          [[::pause-flow this]])})
           (ant/button {:text "resume"
                        :on-click
                        (fn []
                          [[::resume-flow this]])})
          (ant/button {:text "stop"
                        :on-click
                        (fn []
                          [[::stop-flow this]])})
          (ant/button {:text "clear"
                        :on-click
                        (fn []
                          [[::clear-flow this]])}))
         (ant/button {:text "tap"
                      :on-click
                      (fn []
                        (tap> flow)
                        nil)})
          (viscous/inspector {:obj (viscous/wrap flow-config)}))
        
        scroll-bounds [(max 0 cw)
                       (max 0 (- ch
                                 (ui/height header)))]
        flow-body (when (and flow-config layout-size)
                    (basic/scrollview
                     {:scroll-bounds scroll-bounds
                      :$body nil
                      :body
                      (ui/wrap-on
                       :mouse-down
                       (fn [handler mpos]
                         (let [intents (handler mpos)]
                           (if (seq intents)
                             intents
                             [[:set $colors {}]])))
                       (ui/on
                        ::drop-val
                        (fn [m]
                          [[::inject-flow (assoc m
                                                 :flow-layout flow-layout
                                                 :flow flow
                                                 :flow-config flow-config)]])
                        ::dnd/drag-start
                        (fn [m]
                          #_[[::dnd/drag-start (assoc m 
                                                      ::dnd/obj {:x (delay
                                                                      (flow/ping-proc ))})]])
                        ::select-node
                        (fn [{:keys [id]}]
                          [[::toggle-proc {:flow flow
                                           :ping (::ping this)
                                           :$ref $ref
                                           :id id}]
                           [:update $colors
                            (fn [colors]
                              (if (get colors id)
                                {}
                                {id [0.9 0.9 0.9]}))]])
                        (render-layout (assoc flow-layout
                                              :status (::status this)
                                              :ping (::ping this)
                                              :colors colors))))})
                    )]
    (dnd/wrap-on-drop
     (fn [handler pos obj]
       (let [intents (handler pos obj)]
         (if (seq intents)
           intents
           [[::update-flow-config (assoc this :drop-object obj)]])))
     (ui/vertical-layout
      header
      flow-body))))

(defrecord FlowWidget [dispatch!]
  model/IApplet
  (-start [this {:keys [$ref size]}]
    (let []
      (assoc this
             :extra {}
             :$extra [$ref '(keypath :extra)]
             :$ref $ref
             :size size)))
  (-stop [this]
    (dispatch! ::cleanup this)
    nil)
  model/IUI
  (-ui [this {:keys [$context context]}]
    (flow-view (assoc this
                      :context context
                      :$context $context)))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn flow-applet [handler]
  (-> (->FlowWidget handler)
      (assoc :label (str "Flow"))))

(comment
  
  (com.phronemophobic.easel/add-applet
   {:make-applet (fn [handler]
                   (flow-applet handler))})
  testg
  
  (update testg :conns
          (fn [conns]
            (into [] (drop 1) conns)))
  ,)

(def dice-producer
  (wrap-producer
   {:describe (fn [] {:outs {:out "Outputs"}})
    :init (fn [m]
            ;; tell flow to repeatedly call :transform after outputs have been sent
            (assoc m
                   ::produce true
                   :n 0))
    :transform 
    (fn [{:keys [n] :as state} in val]
      [(update state :n inc) {:out [n]}])}))

(def testg
  {:procs
   {
    
    :random-numbers
    {:proc
     (flow/process
      (flow/map->step
       dice-producer))}
    
    :random-numbers2
    {:proc
     (flow/process
      (flow/map->step
       dice-producer))}
    
    :pacer
    {:proc
     (flow/process
      (flow/map->step
       {:describe
        (fn []
          {:ins {:in ""}
           :outs {:out ""}})
        :transform
        (fn [_ _ v]
          (Thread/sleep 1000)
          [_ {:out [v]}])}))}
    
    :onto-chan
    {:args {:chan (async/chan 10)}
     :proc
     (flow/process
      (flow/map->step
       {:describe (fn [] {:ins {:in "  "}
                          :params {:chan "Channel to put values onto"}})
        :init (fn [m]
                (assoc m ::flow/out-ports {:out (:chan m)}))
        :transform (fn [_ _ v]
                     [_ {:out [v]}])}))}
    
    :tap-sink
    {:proc (flow/process
            (flow/map->step
             {:describe (fn [] {:ins {:in "gimme stuff to print!"}})
              :transform (fn [_ _ v]
                           (tap> v)
                           nil)}))}
    
    :prn-sink
    {:proc (flow/process
            (flow/map->step
             {:describe (fn [] {:ins {:in "gimme stuff to print!"}})
              :transform (fn [_ _ v] (prn :prn v))}))}}
   :conns
   [
    [[:random-numbers :out] [:pacer :in]]
    [[:random-numbers2 :out] [:pacer :in]]
    [[:pacer :out] [:tap-sink :in]]
    
    
    
    ,]
   
   ,})
(comment
  (def my-flow  x-113569)
  (flow/ping my-flow)
  
    (flow/ping my-flow)
  (flow/ping-proc my-flow :random-numbers)

  (def my-flow (flow/create-flow testg))
  (flow/stop my-flow)
  (flow/ping my-flow)
  (ex-data x-150352)

  (flow/ping-proc x-159818  :decoder1
                   {:timeout-ms 1000})
  (flow/inject  [:decoder0 :myping] [{}])
  

  ,)

