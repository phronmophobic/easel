(ns com.phronemophobic.schematic.view.preview
  (:refer-clojure :exclude [compile load-file])
  (:require [membrane.component :refer [defui defeffect]]
            [membrane.basic-components :as basic]
            [membrane.ui :as ui]
            [clojure.string :as str]
            [clojure.set :as set]
            loom.graph
            loom.alg
            [membrane.skia.paragraph :as para]
            [com.phronemophobic.viscous :as viscous]
            [com.phronemophobic.membrandt :as ant]
            [com.phronemophobic.membrandt.icon.ui :as icon.ui]
            [com.rpl.specter :as specter]
            [com.phronemophobic.schematic.model :as sm]
            [clojure.edn :as edn]
            [com.phronemophobic.schematic.view.component-picker
             :refer [component-picker]]
            [membrane.alpha.component.drag-and-drop :as dnd]
            [com.phronemophobic.schematic.view.util
             :refer [uicall
                     drag-elem-target
                     code-editor]]
            [liq.buffer :as buffer]
            [membrane.skia :as skia]
            [com.phronemophobic.replog :as replog]))



(defmulti compile*
  (fn [ctx o]
    (:element/type o)))

(defn compile [ctx o]
  (cond
    (map? o)
    (compile* ctx o)
    
    (or (string? o)
        (nil? o))
    o

    (seq? o)
    o

    (seqable? o)
    (into (empty o)
          (map #(compile ctx %))
          o)

    :else
    o))

(defn eval+* [eval-ns o]
  (binding [*ns* eval-ns]
    (eval o)))

(def eval+ (memoize eval+*))

(defmethod compile* ::sm/paragraph [ctx
                                    {:keys [element/text
                                            element/width
                                            element/paragraph-style]
                                     :as elem}]
  (let [text (compile ctx text)
        width (compile ctx width)
        paragraph-style (compile ctx paragraph-style)]
    (para/paragraph text
                    width
                    paragraph-style)))

(defmethod compile* ::sm/progress-bar [ctx
                                       {:keys [element/value
                                               element/width
                                               element/height]}]
  (let [value (compile ctx value)
        width (compile ctx width)
        height (compile ctx height)]
    (ant/progress-bar {:progress value
                       :width width
                       :height height})))

(defmethod compile* ::sm/number-slider [ctx
                                        {:keys [element/value
                                                element/width
                                                number-slider/max
                                                number-slider/min
                                                number-slider/integer?]}]
  (let [value (compile ctx value)
        width (compile ctx width)
        max (compile ctx max)
        min (compile ctx min)
        integer? (compile ctx integer?)]
    (ant/number-slider {:value value
                        :width width
                        :max max
                        :min min
                        :integer? integer?})))


(defmethod compile* ::sm/radio-bar [ctx
                                    {:keys [element/size
                                            radio-bar/options
                                            radio-bar/selection]}]
  (let [size (compile ctx size)
        options (compile ctx options)
        selection (compile ctx selection)]
    (ant/radio-bar {:size size
                    :options options
                    :selection selection})))


(defmethod compile* ::sm/button [ctx
                                 {:keys [element/text
                                         element/on-click]
                                  :as elem}]
  (let [{:keys [$elem extra $extra context $context]} ctx
        editing? (get extra :editing?)
        $editing? [$extra (list 'keypath :editing?)]
        $buf [$extra (list 'keypath :buf)]]
    (if editing?
      (let [edit-string (get extra :edit-string)
            buf (get extra :buf)
            code (:element/code text)
            $code [$elem (list 'keypath :element/text) (list 'keypath :element/code)]]
        (uicall
         code-editor
         {:code code
          :$code $code
          :editing? editing?
          :$editing? $editing?
          :buf buf
          :$buf $buf}))
      (ui/on
       :mouse-down
       (fn [_]
         (when (and (map? text)
                    (= ::sm/code
                       (:element/type text)))
           [[:set $buf (buffer/buffer (pr-str (:element/code text)) {:mode :insert})]
            [:set $editing? true]]))
       (ui/no-events
        (ant/button
         (let [props
               (into
                {:text (compile ctx text)}
                (keep (fn [kw]
                        (when-let [v (get elem kw)]
                          [(keyword (name kw))
                           (compile ctx v)])))
                [:ant.style/size
                 :ant.style/type
                 :ant.style/danger?
                 :ant.style/disabled?])]
           props)))))))

(defui no-events-text-input [{:as m}]
  (ui/no-events
   (ant/text-input (into {} m))))

(defmethod compile* ::sm/text-input [ctx
                                     {:keys [element/text
                                             flex.grow/width
                                             $elem
                                             extra
                                             context
                                             $context
                                             $extra]
                                      :as elem}]
  (let [props (into
               {:text (compile ctx text)
                :flex.grow/width width}
               (keep (fn [kw]
                       (when-let [v (get elem kw)]
                         [(keyword (name kw))
                          (compile ctx v)])))
               [:ant.style/size
                :ant.style/status
                :ant.style/disabled?])]
   (no-events-text-input
    props)))

(defmethod compile* ::sm/checkbox [ctx
                                   {:keys [element/checked?]}]
  (ui/checkbox (compile ctx checked?)))


(def binding-map## (gensym))
(defmethod compile* ::sm/code [ctx
                               {:keys [element/code]}]
  (let [bindings (:bindings (:context ctx))
        ;; _ (prn bindings)
        let-bindings (into []
                           (mapcat (fn [[sym _]]
                                     [sym `(get ~binding-map## (quote ~sym))]))
                           bindings)
        fcode `(fn [~binding-map##]
                 (let ~let-bindings
                   ~code))
        ;; _ (clojure.pprint/pprint fcode)
        f (eval+ (:eval-ns ctx) fcode)]
    (f bindings)))


(defmethod compile* ::sm/let [ctx
                              {:keys [element/body
                                      element/bindings]}]
  (let [{:keys [$elem extra $extra context $context]} ctx]
    (if body
      (let [bindings (reduce
                      (fn [bindings {:let/keys [binding val]}]
                        (let [compiled-val (compile
                                            (assoc-in ctx [:context :bindings] bindings)
                                            val)]
                          (assoc bindings binding compiled-val)))
                      (-> ctx :context :bindings)
                      bindings)]
        (compile
         (-> ctx
             (assoc-in [:context :bindings] bindings)
             (assoc :$elem [$elem (list 'keypath :element/body)]
                    :extra (get extra [::body])
                    :$extra [$extra (list 'keypath [::binding])]))
         body))
      ;; else
      (uicall drag-elem-target
              {:elem body
               :$elem [$elem (list 'keypath :element/body)]}))))

(defmethod compile* ::sm/component [ctx
                                    {:keys [component/name
                                            component/args
                                            component/body
                                            component/defaults]}]
  (let [{:keys [$elem extra $extra context $context]} ctx]
    (if body
      (compile
       (-> ctx
           (assoc :$elem [$elem (list 'keypath :component/body)]
                  :extra (get extra :component/body)
                  :$extra [$extra (list 'keypath :component/body)])
           (update-in [:context :bindings]
                      (fn [bindings]
                        (into (-> (or bindings {})
                                  ;; add container size, if available
                                  (update
                                   'context
                                   (fn [compile-context]
                                     (let [compile-context (or compile-context {})]
                                       (merge compile-context
                                              (select-keys context [:membrane.stretch/container-size])))))
                                  (assoc 'this defaults
                                         'extra {}))
                              (map (fn [[k v]]
                                     [(symbol k) (eval+ (:eval-ns ctx) v)]))
                              defaults))))
       body)
      (uicall drag-elem-target
              {:elem body
               :$elem [$elem (list 'keypath :component/body)]}))))



(defmethod compile* ::sm/group [ctx
                                {:keys [element/children]}]
  (let [{:keys [$elem extra $extra context $context]} ctx]
    (into []
          (map-indexed
           (fn [i child]
             (compile
              (assoc ctx
                     :$elem [$elem (list 'keypath :element/children) (list 'nth i)]
                     :extra (get extra [::children i])
                     :$extra [$extra (list 'keypath [::children i])]
                     :context context
                     :$context $context)
              child)))
          children)
    ))

(defmethod compile* ::sm/for [ctx
                              {:keys [element/body
                                      element.for/x
                                      element.for/xs]}]
  (if body
    (let [{:keys [$elem extra $extra context $context]} ctx]
      (into []
            (map (fn [item]
                   (compile
                    (assoc ctx
                           :elem body
                           :$elem [$elem (list 'keypath :element/body)]
                           :context (update context
                                            :bindings
                                            (fn [bindings]
                                              (assoc bindings x item)))
                           :$context $context)
                    body)))
            (compile (assoc ctx
                            :elem xs
                            :$elem [$elem (list 'keypath :element/xs)]
                            :context context
                            :$context $context)
                     xs)))
   (let [{:keys [$elem extra $extra context $context]} ctx]
     [(uicall drag-elem-target
              {:elem body
               :$elem [$elem (list 'keypath :element/body)]})])))

(defmethod compile* ::sm/flex-layout [ctx
                                      {:keys [element/children
                                              flex/layout]}]
  (let [{:keys [$elem extra $extra context $context]} ctx]
    (ui/vertical-layout
     ;; (ui/label "flex-layout")
     (if children
       (ui/flex-layout
        (compile
         (assoc ctx
                :$elem [$elem (list 'keypath :element/children)]
                :extra (get extra ::children)
                :$extra [$extra (list 'keypath ::children)]
                :context context
                :$context $context)
         children)
        (into {}
              (map (fn [[k v]]
                     [k (compile
                         (assoc ctx
                                :$elem [$elem (list 'keypath :flex/layout) (list 'keypath k)]
                                :extra (get extra [::flex k])
                                :$extra [$extra (list 'keypath [::flex k])]
                                :context context
                                :$context $context)
                         v)]))
              layout))
       (uicall drag-elem-target
               {:elem children
                :$elem [$elem (list 'keypath :element/children)]})))))

(defn eval-relative-layout [bindings form]
  (cond
    (contains? bindings form)
    (get bindings form)

    (number? form)
    form

    (map? form)
    ;; assume op
    (let [{:keys [op args]} form]
      (let [opf (case op
                  clojure.core/- -
                  clojure.core/+ +
                  membrane.ui/width ui/width
                  membrane.ui/height ui/height
                  membrane.ui/origin-x ui/origin-x
                  membrane.ui/origin-y ui/origin-y

                  ;; else
                  (cond
                    (keyword? op) op

                    :else
                    (throw (ex-info "Unrecognized relative layout op"
                                    {:op op
                                     :form form}))))]
        (apply opf (eduction
                    (map #(eval-relative-layout bindings %))
                    args))))))

(comment
  (let [id1 (random-uuid)]
    (eval-relative-layout
     {`layout-height 20
      id1 (ui/filled-rectangle [1 0 0] 25 25)}
     {:op `+
      :args [{:op `-
              :args [`layout-height
                     {:op `ui/width
                      :args [id1]}]}
             100]}))
  (def default-layout
    {`sm/layout-width 300
     `sm/layout-height 400})

  ,)

(defeffect ::update-relative-layout [{:keys [$elem selection mx my]}]
  (dispatch!
   :update
   $elem
   (fn [elem]
     (let [[control-x control-y] (:control-point selection)
           [snap-x snap-y] (-> selection
                               :start-drag
                               :snap-point)
           deps (-> selection
                    :start-drag
                    :deps)
           [x1 y1] (-> selection
                       :start-drag
                       :pt)
           offset-x (- mx x1)
           offset-y (- my y1)

           elem (-> elem
                    (assoc-in [:relative/layout (:element/id selection) control-x]
                              {:op `+
                               :args [snap-x offset-x]})
                    (assoc-in [:relative/layout (:element/id selection) control-y]
                              {:op `+
                               :args [snap-y offset-y]}))
           elem (if (seq deps)
                  (assoc-in elem [:relative/layout (:element/id selection) :deps] deps)
                  (update-in elem [:relative/layout (:element/id selection)] dissoc :deps))]
       elem))))

(defmethod compile* ::sm/relative-layout [ctx
                                          {:keys [element/children
                                                  relative/layout]
                                           :as self}]
  (let [{:keys [$elem extra $extra context $context]} ctx
        selection (get extra :selection)
        $selection [$extra (list 'keypath :selection)]

        schematic-selection (or (-> ctx
                                    :context
                                    :selection)
                                #{})]
    (when (seq children)
      (let [elems-by-id
            (into {}
                  (map-indexed
                   (fn [i child]
                     [(:element/id child)
                      (compile
                       (assoc ctx
                              :$elem [$elem (list 'keypath :element/children) (list 'nth i)]
                              :extra (get extra [::children i])
                              :$extra [$extra (list 'keypath [::children i])]
                              :context context
                              :$context $context)
                       child)]))
                  children)
            [cw ch :as container-size] (:membrane.stretch/container-size context)

            g (let [
                    adjacencies
                    (into {}
                          (map (fn [[k {:keys [deps]}]]
                                 [k deps]))
                          layout)
                    ;; loom.graph is goofy
                    g (if (seq adjacencies)
                        (loom.graph/digraph adjacencies)
                        (loom.graph/digraph))
                    g (apply
                       loom.graph/add-nodes
                       g
                       (keys elems-by-id))]
                g)

            compile-order (reverse
                           (loom.alg/topsort g))

            initial-bindings {`sm/layout-width (or (:element/width self)
                                                   cw
                                                   300)
                              `sm/layout-height (or (:element/height self)
                                                    ch
                                                    300)}

            pt-icon-size 16
            pt-offset (- (/ pt-icon-size 2))
            pt-icon (icon.ui/icon {:name "plus-circle"
                                   :size [pt-icon-size pt-icon-size]})

            bindings
            (transduce
             identity
             (completing
              (fn [bindings elem-id]
                (let [elem (get elems-by-id elem-id)
                      elem-layout (get layout elem-id)
                      elem (let [{:element/keys [x y]} elem-layout]
                             (ui/translate (eval-relative-layout bindings (or x 0))
                                           (eval-relative-layout bindings (or y 0))
                                           elem))]
                  (assoc bindings elem-id elem))))
             initial-bindings
             compile-order)

            control-points
            (eduction
             (map :element/id)
             (mapcat
              (fn [elem-id]
                (let [elem (get bindings elem-id)]
                  [(ui/translate (+ (ui/origin-x elem) pt-offset)
                                 (+ (ui/origin-y elem) pt-offset)
                                 (ui/on-click
                                  (fn []
                                    [[:set $selection
                                      {:element/id elem-id
                                       :control-point [:element/x :element/y]}]])
                                  pt-icon))])))
             children)
            snap-points
            (eduction
             cat

             [(when container-size
                (eduction
                 (map (fn [{[x y] :pt
                            :keys [snap-point]
                            :as m}]
                        (ui/translate
                         (+ x pt-offset) (+ y pt-offset)
                         (ui/on
                          :mouse-down
                          (fn [_]
                            [[::select-snap-point
                              {:snap-point snap-point}]])
                          pt-icon))))
                 [{:pt [0 0] :snap-point [0 0]}
                  {:pt [0 ch] :snap-point [0 `sm/layout-height]}
                  {:pt [cw ch] :snap-point [`sm/layout-width `sm/layout-height]}
                  {:pt [cw 0]  :snap-point [`sm/layout-width 0]}]))
              
              (eduction
               (map :element/id)
               (remove (fn [elem-id]
                         (= elem-id (:element/id selection))))
               (mapcat
                (fn [elem-id]
                  (let [elem (get bindings elem-id)]
                    (eduction
                     (map (fn [{[x y] :pt
                                :keys [snap-point]
                                :as m}]
                            (ui/translate
                             (+ x pt-offset) (+ y pt-offset)
                             (ui/on
                              :mouse-down
                              (fn [_]
                                [[::select-snap-point
                                  {:snap-point snap-point
                                   :deps #{elem-id}}]])
                              pt-icon))))
                     [{:pt [(ui/origin-x elem) (ui/origin-y elem)]
                       :snap-point [{:op `ui/origin-x
                                     :args [elem-id]}
                                    {:op `ui/origin-y
                                     :args [elem-id]}]}

                      {:pt [(ui/origin-x elem)
                            
                            (+ (ui/origin-y elem)
                               (ui/height elem))]
                       :snap-point [{:op `ui/origin-x
                                     :args [elem-id]}
                                    {:op `+
                                     :args [{:op `ui/origin-y
                                             :args [elem-id]}
                                            {:op `ui/height
                                             :args [elem-id]}]}]}
                      {:pt [(+ (ui/origin-x elem)
                               (ui/width elem))
                            (ui/origin-y elem)
                            ]
                       :snap-point [{:op `+
                                     :args [{:op `ui/origin-x
                                             :args [elem-id]}
                                            {:op `ui/width
                                             :args [elem-id]}]}
                                    {:op `+
                                     :args [{:op `ui/origin-y
                                             :args [elem-id]}]}]}

                      {:pt [(+ (ui/origin-x elem)
                               (ui/width elem))
                            (+ (ui/origin-y elem)
                               (ui/height elem))]
                       :snap-point [{:op `+
                                     :args [{:op `ui/origin-x
                                             :args [elem-id]}
                                            {:op `ui/width
                                             :args [elem-id]}]}
                                    {:op `+
                                     :args [{:op `ui/origin-y
                                             :args [elem-id]}
                                            {:op `ui/height
                                             :args [elem-id]}]}]}]))))
               children)])]
        (into []
              cat
              [(eduction
                (map :element/id)
                (map #(get bindings %))
                ;; :element/children provides draw order
                ;; which may be different than compile order
                children)
               (if selection
                 [

                  (if (:start-drag selection)
                    (when container-size
                      (ui/on
                       :mouse-up
                       (fn [[mx my]]
                         [[:delete $selection]
                          [::update-relative-layout
                           {:$elem $elem
                            :selection selection
                            :mx mx
                            :my my}]])
                       :mouse-move
                       (fn [[mx my]]
                         [[::update-relative-layout
                           {:$elem $elem
                            :selection selection
                            :mx mx
                            :my my}]])
                       (ui/spacer cw ch)))
                    (ui/wrap-on
                     :mouse-down
                     (fn [f [mx my]]
                       (let [intents (f [mx my])]
                         (specter/transform
                          [specter/ALL
                           #(= ::select-snap-point (first %))]
                          (fn [[_ m]]
                            [:update $selection
                             #(assoc % :start-drag
                                     (assoc m :pt [mx my]))])
                          intents)))
                     (vec snap-points)))]

                 ;; else
                 (when (schematic-selection (:element/id self))
                   control-points))
               ])))))

(defmethod compile* ::sm/defui [ctx
                                {:keys [element/name
                                        element/function
                                        element/data]}]

  (let [ctx (assoc-in ctx [:context :bindings 'extra] {})
        data-evaled (into {:context
                           (get-in ctx
                                   [:context :bindings 'context])}
                          (map (fn [[k node]]
                                 [k (compile ctx node)]))
                          data)]
    (function data-evaled)))

(defui debug [{}])

(comment
  (skia/run
    (membrane.component/make-app #'debug {}))

  (compile
   {:element/id (random-uuid)
    :element/type ::sm/flex-layout
    :element/children {:element/id (random-uuid)
                       :element/type ::sm/for
                       :element.for/x 'x
                       :element.for/xs '[1 2 3]
                       :element/body {:element/id (random-uuid)
                                      :element/type ::sm/paragraph
                                      :element/text
                                      {:element/id (random-uuid)
                                       :element/type ::sm/code
                                       :elment/code 'x}}}})

  (compile
   {:element/id (random-uuid)
    :element/type ::sm/for
    :element.for/x 'x
    :element.for/xs '[1 2 3]
    :element/body {:element/id (random-uuid)
                   :element/type ::sm/paragraph
                   :element/text
                   {:element/id (random-uuid)
                    :element/type ::sm/code
                    :element/code '(str x)}}})

  ,)

(defui editor [{:keys [elem
                       eval-ns]}]
  (let [preview-container (:preview-container extra)

        subcontext (case preview-container

                     :mobile
                     (assoc context :membrane.stretch/container-size [375 812])

                     :small-mobile
                     (assoc context :membrane.stretch/container-size [200 400])

                     ;; else
                     context)]
    (ui/vertical-layout
     (ui/label (pr-str preview-container))
     (apply
      ui/horizontal-layout
      (for [container-button-info [{:icon-name "minus-square" :container-type nil}
                                   {:icon-name "mobile" :container-type :mobile}
                                   {:icon-name "mobile" :container-type :small-mobile}
                                   {:icon-name "desktop" :container-type :desktop}]]
        (ui/on-click
         (fn []
           [[:set $preview-container (:container-type container-button-info)]])
         (icon.ui/icon {:name (:icon-name container-button-info)}))))
     (ui/translate
      4 4
      (if (nil? elem)
        (drag-elem-target {:elem elem})
        (try
          [(when preview-container
             (case preview-container
               :mobile
               (ui/with-style :membrane.ui/style-stroke
                 (ui/rectangle 375 812))

               :small-mobile
               (ui/with-style :membrane.ui/style-stroke
                 (ui/rectangle 200 400))

               ;; else
               nil
               ))
           (ui/try-draw
            (compile
             {:$elem $elem
              :extra extra
              :$extra $extra
              :context subcontext
              :$context $context
              :eval-ns eval-ns}
             elem)
            (fn [draw e]
              (draw (ui/label e))))]
          (catch Throwable e
            (clojure.pprint/pprint e)
            (ui/label "Error"))))))))

(defui editor+component-picker [{:keys [elem]}]
  (dnd/drag-and-drop
   {:$body nil
    :body
    (ui/vertical-layout
     (ui/horizontal-layout
      (component-picker {})
      (ui/vertical-layout
       (ant/button {:text "clear"
                      :on-click (fn []
                                  [[:set $elem nil]])})
       (editor {:elem elem})))
     (let [inspector-extra (get extra ::inspector-extra)]
       (viscous/inspector {:obj (viscous/wrap elem)
                           :width (get inspector-extra :width 40)
                           :height (get inspector-extra :height 1)
                           :show-context? (get inspector-extra :show-context?)
                           :extra inspector-extra})))}))


(defonce app-state (atom {}))

(comment
  (clojure.pprint/pprint
   (sm/compile (:elem @app-state) ))
  ,)

(defeffect ::sm/toggle-selection [intent]
  (dispatch! :update
             (:$selection intent)
             (fn [s]
               (let [s (or s #{})
                     id (:element/id intent)]
                 (if (contains? s id)
                   (disj s id)
                   #{id})))))

(defeffect ::sm/delete-selection [{:keys [$elem selection $selection]}]
  (when-let [id (first selection)]
    (dispatch! :update
               $elem
               (fn [elem]
                 (specter/setval (sm/elem-by-id id)
                                 specter/NONE
                                 elem))))
  (dispatch! :set $selection #{}))


(defeffect ::replog-elem [{:keys [eval-ns elem] :as m}]
  (let [ns (ns-name eval-ns)
        requires '[[membrane.component
                    :refer [defui defeffect]]
                   [com.phronemophobic.membrandt :as ant]
                   [membrane.ui :as ui]]
        ops [{::replog/ns ns
              ::replog/requires requires
              ::replog/form `(require ~@(eduction
                                         (map (fn [req]
                                                (list 'quote req)))
                                         requires))}
             {::replog/form (sm/compile elem)
              ::sm/elem elem
              ::replog/ns ns }]]
    (tap> {:effect (ns-name *ns*)})
    (tap> ops)
    (replog/append ops)
    (tap> (replog/load-log ops))


    ))

(defui toolbar [{:keys [elem selection
                        ^:membrane.component/contextual
                        eval-ns]}]
  (ui/vertical-layout
   (ant/button {:text "clear"
                :size :small
                :on-click (fn []
                            [[:set $elem nil]])})
   (ui/horizontal-layout
    (ui/label (str "selection: " (pr-str selection))))
   (ant/button {:text "delete"
                :size :small
                :on-click (fn []
                            [[::sm/delete-selection
                              {:$elem $elem
                               :selection selection
                               :$selection $selection}]])})
   (ant/button {:text "print"
                :size :small
                :on-click (fn []
                            (clojure.pprint/pprint
                             (sm/compile elem))
                            nil)})
   (ant/button {:text "debug load"
                :size :small
                :on-click
                  (fn []
                    #_(let [node (->> (com.phronemophobic.replog/get-main-log)
                                      (last))
                            loaded-elem (::sm/elem node)
                            loaded-ns (:com.phronemophobic.replog/ns node)
                            ]
                      [[:set $eval-ns loaded-ns]
                       [:set $elem loaded-elem]])
)})
   (ant/button {:text "save"
                :size :small
                :on-click (fn []
                            [[::replog-elem {:eval-ns eval-ns
                                             :elem elem}]])})
   (ant/button {:text "eval"
                :size :small
                :on-click (fn []
                            (binding [*ns* (or eval-ns
                                               (the-ns 'clojure.core))]
                              (let [v (eval (sm/compile elem))]
                                [[:com.phronemophobic.easel/add-component!
                                  (keyword (name (ns-name *ns*))
                                           (name (:component/name elem)))
                                  (fn []
                                    {:element/type ::sm/defui
                                     :element/name (name (:component/name elem))
                                     :element/function v
                                     :element/data (update-vals
                                                    (:component/defaults elem)
                                                    (fn [v]
                                                      {:element/type ::sm/code
                                                       :element/id (random-uuid)
                                                       :element/code v}))
                                     :element/id (random-uuid)})]])))})
   (ant/button {:text "show!"
                :size :small
                :on-click (fn []
                            (let [v (binding [*ns* (or eval-ns
                                                       (the-ns 'clojure.core))]
                                      (eval (sm/compile elem)))]
                              [[:com.phronemophobic.easel/add-component-as-applet
                                v
                                (:component/defaults elem)]]
                              #_(skia/run (membrane.component/make-app v
                                                                       (eval+ (:component/defaults elem))))))}))
  )

(comment

  (require '[com.phronemophobic.schematic.view.tree :as view.tree])

  (defui editor++ [{:keys [elem]}]
    (let [selection (get context :selection)]
      (ui/on
       ::sm/toggle-selection
       (fn [{:element/keys [id] :as intent}]
         [[::sm/toggle-selection (assoc intent
                                        :$selection $selection)]])
       (dnd/drag-and-drop
        {:$body nil
         :body
         (ui/vertical-layout
          (ui/horizontal-layout
           (component-picker {})
           (ui/vertical-layout
            (ant/button {:text "clear"
                           :on-click (fn []
                                       [[:set $elem nil]])})
            (try
              (ui/try-draw
               (let [v (editor {:elem elem
                                :extra (get extra ::preview-editor)})]
                 (doall (tree-seq some? ui/children v))
                 v)
               (fn [draw e]
                 (clojure.pprint/pprint e)
                 (draw (ui/label e))))
              (catch Throwable e
                (clojure.pprint/pprint e)
                (ui/label "Error")))))
          (view.tree/editor {:elem elem
                             :extra (get extra ::tree-editor)})
          (let [inspector-extra (get extra ::inspector-extra)]
            (viscous/inspector {:obj (viscous/wrap elem)
                                :width (get inspector-extra :width 40)
                                :height (get inspector-extra :height 1)
                                :show-context? (get inspector-extra :show-context?)
                                :extra inspector-extra}))
          (ui/horizontal-layout
           (ui/label (str "selection: " (pr-str selection))))
          (ant/button {:text "delete"
                         :on-click (fn []
                                     [[::sm/delete-selection
                                       {:$elem $elem
                                        :selection selection
                                        :$selection $selection}]])})
          (ant/button {:text "print"
                         :on-click (fn []
                                     (clojure.pprint/pprint
                                      (sm/compile elem))
                                     nil)})
          (ant/button {:text "eval"
                         :on-click (fn []
                                     (eval (sm/compile elem))
                                     nil)})
          (ant/button {:text "show!"
                         :on-click (fn []
                                     (let [v (eval (sm/compile elem))]
                                       (skia/run (membrane.component/make-app v
                                                                              (eval+ (:component/defaults elem)))))
                                     nil)}))}))))

  (reset! app-state
          {:elem {:element/type ::sm/let
                  :element/id ::root
                  :element/bindings [{:let/binding 'a
                                      :let/val
                                      {:element/id (random-uuid)
                                       :element/type ::sm/code
                                       :element/code '42}}
                                     {:let/binding 'a
                                      :let/val
                                      {:element/id (random-uuid)
                                       :element/type ::sm/code
                                       :element/code '1}}
                                     ]
                  :element/body {:element/type ::sm/group
                                 :element/id ::root-group
                                 :element/children []}}})
  (skia/run
    (membrane.component/make-app #'editor++ app-state))

  ,)

