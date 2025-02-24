(ns com.phronemophobic.schematic.model
  (:refer-clojure :exclude [compile load-file])
  (:require [clojure.spec.alpha :as s]
            [com.phronemophobic.viscous :as viscous]
            [membrane.alpha.stretch :as stretch]
            [com.rpl.specter :as specter]
            [clojure.zip :as z]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [zippo.core :as zippo]
            loom.graph
            loom.alg
            [clojure.test.check.generators :as gen]
            [flatland.ordered.map :refer
             [ordered-map]]
            [membrane.basic-components :as basic]
            [membrane.component :as component
             :refer [defui defeffect]]
            [com.phronemophobic.membrandt :as ant]
            [membrane.ui :as ui]
            [membrane.skia :as skia]
            [membrane.skia.paragraph :as para]
            #_[clojure.spec.gen.alpha :as gen]))

(defn stoggle [s x]
  (if (contains? s x)
    (disj s x)
    (conj s x)))

(defn write-edn [w obj]
  (binding [*print-length* nil
            *print-level* nil
            *print-dup* false
            *print-meta* false
            *print-readably* true

            ;; namespaced maps not part of edn spec
            *print-namespace-maps* false

            *out* w]
    (pr obj)))

(def WALK-ELEM
  (specter/recursive-path [] p
                          (specter/if-path
                           vector? [specter/ALL p]
                           ;; else
                           (specter/continue-then-stay
                            (specter/multi-path
                             [(specter/must :element/children) p]
                             [(specter/must :prototype/component) p]
                             [(specter/must ::value) p]
                             [(specter/must :component/body) p]
                             [(specter/must :element/body) p]
                             [(specter/must :element/bindings) specter/ALL :let/val p])))))

(defmulti compile* :element/type)

(defn compile [o]
  (cond
    (map? o)
    ;;(compile* o)
    (if (= (:element/type o)
           ::code)
      (compile* o)
      (compile* o)
      #_`(when-let [elem# ~(compile* o)]
         (with-meta
           elem#
           (quote ~{::ast (select-keys o [:element/id])}))))

    (or (string? o)
        (nil? o))
    o

    (seq? o)
    o

    (seqable? o)
    (into (empty o)
          (map compile)
          o)

    :else
    o))


(defprotocol IContentOffset
  (content-offset [view]))

(extend-protocol IContentOffset
  membrane.ui.Padding
  (content-offset [view]
    [(:left view)
     (:top view)])
  Object
  (content-offset [view]
    (ui/origin view)))

(defn ui-zip [view]
  (z/zipper (constantly true)
            ui/children
            #(throw (ex-info "Can't unzip" {:view view}))
            view))


(defn global-coord [zview]
  ;; Walk up the parents to find
  ;; the "global" coordinate
  (let [[ox oy] (-> zview
                    z/node
                    ui/origin)]
    (loop [x ox
           y oy
           zview (z/up zview)]
      (if zview
        (let [view (z/node zview)
              [ox oy] (content-offset view)]
          (recur (+ x ox)
                 (+ y oy)
                 (z/up zview)))
        [x y]))))

(defn highlight-selection [view selection]
  (try
    (loop [highlight []
           zview (ui-zip view)]

      (if (z/end? zview)
        highlight
        (let [view (z/node zview)
              ast (-> view
                      meta
                      ::ast)
              eid (:element/id ast)]
          (recur (if (selection eid)
                   (conj highlight
                         (let [[gx gy] (global-coord zview)
                               [w h] (ui/bounds view)]
                           (ui/translate gx gy
                                         (ui/filled-rectangle [0 1 0]
                                                              (max 5 w)
                                                              (max 5 h)))))
                   highlight)
                 (z/next zview)))))
    (catch Exception e
      nil)))

(s/def :element/id uuid?)
#_(s/def :element/type
    #{::rectangle})

(s/def ::form any?)
(s/def ::coord (s/or :double double?
                     :int int?
                     :form ::form))

(s/def :element/children
  (s/or :literal (s/coll-of :element/element)
        :form ::form))

(s/def :for/xs any?)
(s/def :for/x symbol?)
(s/def ::for
  (s/keys
   :req [:for/xs
         :for/x
         :for/body]))

(s/def :element/x ::coord)
(s/def :element/y ::coord)
(s/def :element/width ::coord)
(s/def :element/height ::coord)

(defmulti element-type :element/type)
(defmethod element-type ::rectangle [_]
  (s/keys :req [:element/width
                :element/height]
          :opt [:element/x
                :element/y]))

(def layout-specs
  (s/keys :opt [:element/children]))
(defmethod element-type ::vertical-layout [_]
  layout-specs)

(defmethod element-type ::horizontal-layout [_]
  layout-specs)

(defmethod element-type ::group [_]
  (s/keys :opt [:element/children]))

(defmethod element-type ::let [_]
  (s/keys :opt [:element/body
                :element/bindings]))

(s/def :element/element
  (s/merge
   (s/multi-spec element-type :element/type)
   (s/keys :opt [:element/id])))

(comment
  (swap! @#'s/registry-ref dissoc :element/type)
  ,)


(defmethod compile* ::flex-layout [{:element/keys [children]
                                    :flex/keys [layout]
                                    :as m}]
  ;; currently, all properties are literals and
  ;;    don't use compiled or calculated values
  `(ui/flex-layout ~(compile children)
                   ~(into {}
                          (map (fn [[k v]]
                                 [k (compile v)]))
                          layout)))



(defn compile-relative-layout-form [bindings form]
  (cond

    (contains? bindings form)
    (get bindings form)

    (number? form)
    form

    (symbol? form)
    form

    (map? form)
    ;; assume op
    (let [{:keys [op args]} form]
      `(~op ~@(mapv #(compile-relative-layout-form bindings %) args)))))

(defmethod compile* ::relative-layout [{:element/keys [children]
                                        :keys [relative/layout]
                                        :as m}]
  (let [by-id (into {}
                    (map (fn [m]
                           [(:element/id m) m]))
                    children)

        g (apply
           loom.graph/digraph
           (into {}
                 (map (fn [[k {:keys [deps]}]]
                        [k deps]))
                 layout)
           (keys by-id))
        compile-order (reverse
                       (loom.alg/topsort g))

        width## (gensym "width-")
        height## (gensym "height-")

        bindings (into {`layout-width width##
                        `layout-height height##}
                       (map (fn [id]
                              [id (gensym)]))
                       (keys by-id))
        elem-bindings
        (into []
              (comp
               (filter (fn [elem-id]
                         (get by-id elem-id)))
               (map (fn [elem-id]
                      (let [ast (get by-id elem-id)
                            elem (compile ast)

                            elem-layout (get layout elem-id)
                            elem (if elem-layout
                                   (let [{:element/keys [x y]} elem-layout]
                                     `(ui/translate ~(compile-relative-layout-form bindings (or x 0))
                                                    ~(compile-relative-layout-form bindings (or y 0))
                                                    ~elem))
                                   ;; else
                                   elem)]
                        [(get bindings elem-id) elem])))
                    cat)
              compile-order)]
    `(let [~width## ~(or (:element/width m)
                         '(-> context
                              :membrane.stretch/container-size
                              first))
           ~height## ~(or (:element/height m)
                          '(-> context
                              :membrane.stretch/container-size
                              second))
           ~@elem-bindings]
       ~(into []
              (map (fn [m]
                     (get bindings (:element/id m))))
              children))))

(defmethod compile* ::rectangle [{:element/keys [width height]}]
  `(ui/filled-rectangle [0 0 0] ~width ~height))



(defmethod compile* ::vertical-layout [{:element/keys [children]}]
  `(apply ui/vertical-layout
          ~(compile children)))

(defmethod compile* ::horizontal-layout [{:element/keys [children]}]
  `(apply ui/horizontal-layout
          ~(compile children)))


(defmethod compile* ::group [{:element/keys [children]}]
  (into []
        (map compile children)))

(defmethod compile* ::on [{:element/keys [children events]}]
  `(ui/on
    ~@(eduction
       cat
       events)
    ~(into []
        (map compile children))))

(defmethod compile* ::wrap-on [{:element/keys [children events]}]
  `(ui/wrap-on
    ~@(eduction
       cat
       events)
    ~(into []
        (map compile children))))

(defmethod compile* ::for [{:element/keys [body]
                            :element.for/keys [x xs]}]
  `(vec
    (for [~x ~(compile xs)]
      ~(compile body))))

(defmethod compile* ::let [{:element/keys [body bindings]}]
  `(let ~(into []
               (comp
                (map (fn [{:let/keys [binding val]}]
                       [binding (compile val)]))
                cat)
               bindings)
     ~(compile body)))

(defmethod compile* ::paragraph [{:element/keys [text
                                                 width
                                                 paragraph-style]}]
  `(para/paragraph ~(compile text)
                   ~(compile width)
                   ~(compile paragraph-style)))

(defmethod compile* ::button [{:element/keys [text on-click]
                               :as elem}]
  `(ant/button
    ~(let [args `{:text ~(compile text)}
           args (into args
                      (keep (fn [kw]
                              (when-let [v (get elem kw)]
                                [(keyword (name kw))
                                 (compile v)])))
                      [:ant.style/size
                       :ant.style/type
                       :ant.style/danger?
                       :ant.style/disabled?])]
       (if on-click
         (assoc args :on-click (compile on-click))
         args))))

(defmethod compile* ::text-input [{:element/keys [text]
                                   width :flex.grow/width
                                   :as elem}]
  (let [props `{:text ~(compile text)}
        props (into props
                    (keep (fn [kw]
                            (when-let [v (get elem kw)]
                              [(keyword (name kw))
                               (compile v)])))
                    [:ant.style/size
                     :ant.style/status
                     :ant.style/disabled?])
        props (if width
                (assoc props :flex.grow/width width)
                props)]
   `(ant/text-input ~props)))

(defmethod compile* ::progress-bar [{:keys [element/value
                                            element/width
                                            element/height]}]
  (let [value (compile value)
        width (compile width)
        height (compile height)]
    `(ant/progress-bar {:progress ~value
                        :width ~width
                        :height ~height})))

(defmethod compile* ::number-slider [{:keys [element/value
                                             element/width
                                             number-slider/max
                                             number-slider/min
                                             number-slider/integer?]}]
  (let [value (compile value)
        width (compile width)
        max (compile max)
        min (compile min)
        integer? (compile integer?)]
    `(ant/number-slider {:value ~value
                         :width ~width
                         :max ~max
                         :min ~min
                         :integer? ~integer?})))

(defmethod compile* ::radio-bar [{:keys [element/size
                                         radio-bar/options
                                         radio-bar/selection]}]
  (let [size (compile size)
        options (compile options)
        selection (compile selection)]
    `(ant/radio-bar {:size ~size
                     :options ~options
                     :selection ~selection})))


(defmethod compile* ::defui [{:keys [element/function
                                     element/data]}]
  (let [data-compiled (into {}
                            (map (fn [[k v]]
                                   [k (compile v)]))
                            data)
        fsym (let [m (meta function)]
               (symbol (name (ns-name (:ns m)))
                       (name (:name m))))]
    `(~fsym ~data-compiled)))

(defmethod compile* ::checkbox [{:element/keys [checked?]}]
  `(basic/checkbox {:checked? ~(compile checked?)}))

(defmethod compile* ::translate [{:element/keys [x y body]}]
  `(ui/translate ~(compile (or x 0))
                 ~(compile (or y 0))
                 ~(compile body)))

(defmethod compile* ::spacer [{:element/keys [width height]}]
  `(ui/spacer ~(compile (or width 0))
              ~(compile (or height 0))))



(def compile2
  (memoize compile))

(defn on-draw-error [draw e]
  (draw (ui/label e)))

(def render2
  (memoize
   (fn [form]
     (try
       (eval
        `(fn [~'interns]
           (try
             (ui/try-draw
              ~(compile2 form)
              on-draw-error)
             (catch Exception e#
               (clojure.pprint/pprint e#)
               (ui/label e#)))))
       (catch Exception e
         (clojure.pprint/pprint e)
         (constantly
          (ui/label e)))))))



(defn find-elem-at-point [view pos]
  (let [eid
        ;; use seq to make sure we don't stop for empty sequences
        (some (fn [child]
                (when-let [local-pos (ui/within-bounds? child pos)]
                  (find-elem-at-point child local-pos)))
              (reverse (ui/children view)))]
    (if eid
      eid
      (-> (meta view)
          ::ast
          :element/id))))

(def last-save (atom (System/currentTimeMillis)))
(defn now-str []
  (.format (java.text.SimpleDateFormat. "yyyy-MM-dd_HH-mm-ss")
           (java.util.Date.)))

(declare app-state)
(defn save-root! [fname]
  (let [root (:root @app-state)]
    (with-open [w (io/writer fname)]
      (write-edn w root))))

(defn save!
  ([]
   (save! (:root @app-state)))
  ([root]
   (prn "saving...")
   (try
     (with-open [w (io/writer "saves/latest-save.edn")]
       (write-edn w root))
     (with-open [w (io/writer (str "saves/save-" (now-str) ".edn"))]
       (write-edn w root))
     (catch Exception e
       (clojure.pprint/pprint e)))
   (reset! last-save (System/currentTimeMillis))
   nil))
(defn maybe-save [root]
  (when (> (- (System/currentTimeMillis)
              @last-save)
           ;; 3 min
           (* 1000 60 3))
    (save! root)))
(defonce app-state (atom nil))
(defonce app-history
  (let [app-history (atom [])]
    (add-watch app-state ::history
               (fn [k ref old new]
                 (when (not= old new)
                   (future
                     (maybe-save (:root new)))
                   (swap! app-history conj new))))
    app-history))



(def initial-state
  {:root {:element/type ::let
          :element/id ::root
          :element/bindings []
          :element/body {:element/type ::group
                         :element/id ::root-group
                         :element/children []}}
   :interns {}
   :selection #{}
   :collapsed #{}})

(defn load-file [fname]
  (with-open [rdr (io/reader fname)
              pbr (java.io.PushbackReader. rdr)]
    (edn/read pbr)))

(defn load-latest []
  (with-open [rdr (io/reader "saves/latest-save.edn")
              pbr (java.io.PushbackReader. rdr)]
    (edn/read pbr)))

(defn load!
  ([fname]
   (reset! app-state
           (assoc initial-state
                  :root (load-file fname))))
  ([]
   (reset! app-state
           (assoc initial-state
                  :root (load-latest)))))

(defn init! []
  (reset! app-state initial-state)
  )

(def branch-form-keys
  [:element/body
   :prototype/component
   :component/body
   ::value])
(def branch-children-keys
  [:element/children])

(defn element-branch? [e]
  (or (vector? e)
      (some #(get e %) branch-form-keys)
      (some #(get e %) branch-children-keys)))

(defn element-children [e]
  (if (vector? e)
    e
    (or (some (fn [k]
                (when-let [form (get e k)]
                  [form]))
              branch-form-keys)
        (some #(get e %) branch-children-keys))
    #_(if-let [body (:element/body e)]
        [body]
        (if-let [children (:element/children e)]
          children
          (if-let [component (:prototype/component e)]
            [(-> component
                 ::value
                 :component/body)])))))


(defmethod compile* ::component [{:keys [component/name
                                         component/args
                                         component/body
                                         component/defaults
                                         element/id]}]
  (let [args (if args
               args
               (into []
                     (map symbol)
                     (keys defaults)))]
    `(defui ~name [{:keys ~args :as ~'this}]
       ~(compile body)))
  #_`(let [f#
           (fn ;; ~(symbol
             ;;   (clojure.core/name name))
             [{:keys [~@(eduction
                         (map symbol)
                         args)]
               :as m#}]
             (let [~'extra (get m# :extra)
                   ~'context (get m# :context)]
               ~(component/path-replace
                 (compile body)
                 (into
                  {}
                  (map (fn [arg]
                         [(symbol arg) [{} (delay [nil (list 'quote (list 'keypath arg))])]])
                       (conj args :extra :context))))))
           args# ~(compile defaults)]
       (define ~id f# {:name ~name})
       (f# args#)))

(defmethod compile* ::code [{:element/keys [code]}]
  code)





(def
  my-root
  {:element/type ::let
   :element/id ::root
   :element/bindings []
   :element/body {:element/type ::group
                  :element/id ::root-group
                  :element/children []}})



(let [one-child-keys [:element/children
                      :prototype/component
                      ::value
                      :component/body
                      :element/body]
      all-keys (conj one-child-keys
                     :element/bindings)]
  (defn model-branch? [elem]
    (or (vector? elem)
        (some #(% elem) all-keys)))
  (defn model-children [elem]
    (if (vector? elem)
      (seq elem)
      (if-let [bindings (:element/bindings elem)]
        (map :let/val bindings)
        (when-let [child (some #(% elem) one-child-keys)]
          (list child)))))
  (defn model-make-node [node children]
    (if (vector? node)
      (vec children)
      (if (:element/bindings node)
        (throw (ex-info "make-node not supported for element/bindings" {}))
        (if-let [k (some #(when (% node)
                                %)
                             one-child-keys)]
          (do
            (assert (<= (count children) 1) (str "Unexpected number of children for " k))
            (assoc node k (first children)))
          (throw (ex-info "Unexpected state." {})))))))

(defn ^:private zfind [z pred]
  (loop [z z]
    (if (z/end? z)
      nil
      (if (pred (z/node z))
        z
        (recur (z/next z))))))

(defn zelem-parents [zelem]
  (loop [parents '()
         zelem zelem]
    (if zelem
      (recur (conj parents (z/node zelem))
             (z/up zelem))
      parents)))

(defn model-zip [root]
  (z/zipper model-branch? model-children model-make-node root))

(defn zelem-by-id [root eid]
  (zfind (model-zip root)
         #(= eid
             (:element/id %))))

(defn collect-bindings [zelem]
  (let [parents (zelem-parents zelem)
        bindings (reduce
                  (fn [bindings elem]
                    (case (:element/type elem)
                      ;; todo
                      ;; ::sm/let

                      ::component
                      (into bindings
                            (map (fn [[k v]]
                                   (symbol k)))
                            (:component/defaults elem))

                      ::for
                      (if-let [x (:element.for/x elem)]
                        (conj bindings x)
                        bindings)


                      ;; else
                      bindings))
                  #{}
                  parents)]
    bindings))

(defn elem-by-id
  "Returns a path to elem with the provided id."
  [id]
  (specter/path
    [WALK-ELEM
     (fn [elem]
       (= id
          (:element/id elem)))]))

(defn add-let-binding [elem binding]
  (specter/setval
   [:element/bindings specter/END]
   [binding]
   elem))

(defn add-child [elem child]
  (specter/setval
   [:element/children specter/END]
   [child]
   elem))

(defn let-binding [name val]
  {:let/binding name
   :let/val val})

(defn set-child [elem child]
  (case (:element/type elem)
    (::group ::flex-layout)
    (assoc elem :element/children
           (case (:element/type child)
             (::for ::group)
             child
             ;; else
             {:element/type ::group
              :element/id (random-uuid)
              :element/children [child]}))

    (::component)
    (assoc elem :component/body child)

    (::for)
    (assoc elem :element/body child)))

(defeffect ::delete-by-id [{:keys [$elem id]}]
  (dispatch! :update
             $elem
             (fn [elem]
               (specter/setval (elem-by-id id)
                               specter/NONE
                               elem))))

(def my-new-root
  (->> my-root
       #_(specter/transform
          (elem-by-id ::root)
          #(add-let-binding % (let-binding 'a
                                {:element/type ::group
                                 :element/id (random-uuid)
                                 :element/children []})))
       #_(specter/transform
          (elem-by-id ::root-group)
          #(add-child % 'a))))

;; need list of ops
;; start with 7gui?

(specter/select
 WALK-ELEM
 my-root
 )

(def button
  {:element/code `(basic/button {:text "Count"} )
   :element/id (random-uuid)
   :element/type ::code})
(def textbox
  {:element/code `(basic/textarea {:text "2"})
   :element/id (random-uuid)
   :element/type ::code})
(def layout
  {:element/type ::flex-layout
   :element/id ::layout
   })





(def my-counter
  (->> my-root
       (specter/transform
        (elem-by-id ::root)
        #(-> %
             (add-let-binding (let-binding
                                  'layout layout))))
       (specter/transform
        (elem-by-id ::layout)
        #(-> %
             (add-child textbox)
             (add-child button)))
       (specter/transform
        (elem-by-id ::root-group)
        #(add-child % {:element/code 'layout
                       :element/id (random-uuid)
                       :element/type ::code}))))

(def todo-items
  {:element/body {:element/code `(ui/horizontal-layout
                                  (basic/checkbox {:checked?  (:complete? ~'todo)})
                                  (basic/textarea {:text (:description ~'todo)}))
                  :element/id (random-uuid)
                  :element/type ::code}
   :element/type ::for
   :element/id (random-uuid)
   :element.for/x 'todo
   :element.for/xs {:element/code 'todos
                    :element/id (random-uuid)
                    :element/type ::code}})

(def my-todo
  (->> my-root
       (specter/transform
        (elem-by-id ::root)
        #(-> %
             (add-let-binding (let-binding
                                  'items todo-items))))

       (specter/transform
        (elem-by-id ::root)
        #(-> %
             (add-let-binding (let-binding
                                  'layout layout))))
       (specter/transform
        (elem-by-id ::layout)
        #(assoc %
                :flex/layout {:flex/direction :flex.direction/column} 
                :element/children
                {:element/code 'items
                 :element/id (random-uuid)
                 :element/type ::code}))
       (specter/transform
        (elem-by-id ::root-group)
        #(add-child % {:element/code 'layout
                       :element/id (random-uuid)
                       :element/type ::code})))
  )

(def my-todo-component
  {:element/type ::component
   :component/name 'todo-list
   :component/args '[todos]
   :component/defaults {:todos [{:description "drink coffee"
                                 :complete? true}
                                {:description "write code"}]}
   :component/body my-todo})

(membrane.component/defui
   todo-list
   [{:keys [todos]}]
   (clojure.core/when-let
    [elem__38413__auto__
     (clojure.core/let
      [items
       (clojure.core/when-let
        [elem__38413__auto__
         (clojure.core/vec
          (clojure.core/for
           [todo todos]
           (membrane.ui/horizontal-layout
            (membrane.basic-components/checkbox
             {:checked? (:complete? todo)})
            (membrane.basic-components/textarea
             {:text (:description todo)}))))]
        (clojure.core/with-meta
         elem__38413__auto__
         '#:com.phronemophobic.schematic.model{:ast
                                               #:element{:id
                                                         #uuid "25db0f27-6c01-4711-8b17-b2dee3541d30"}}))
       layout
       (clojure.core/when-let
        [elem__38413__auto__
         (clojure.core/apply membrane.ui/vertical-layout items)]
        (clojure.core/with-meta
         elem__38413__auto__
         '#:com.phronemophobic.schematic.model{:ast
                                               #:element{:id
                                                         :com.phronemophobic.schematic.model/layout}}))]
      (clojure.core/when-let
       [elem__38413__auto__ [layout]]
       (clojure.core/with-meta
        elem__38413__auto__
        '#:com.phronemophobic.schematic.model{:ast
                                              #:element{:id
                                                        :com.phronemophobic.schematic.model/root-group}})))]
    (clojure.core/with-meta
     elem__38413__auto__
     '#:com.phronemophobic.schematic.model{:ast
                                           #:element{:id
                                                     :com.phronemophobic.schematic.model/root}})))

