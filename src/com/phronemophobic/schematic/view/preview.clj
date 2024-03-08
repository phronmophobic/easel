(ns com.phronemophobic.schematic.view.preview
  (:refer-clojure :exclude [compile load-file])
  (:require [membrane.component :refer [defui defeffect]]
            [membrane.basic-components :as basic]
            [membrane.ui :as ui]
            [clojure.string :as str]
            [clojure.set :as set]
            [membrane.skia.paragraph :as para]
            [com.phronemophobic.viscous :as viscous]
            [com.rpl.specter :as specter]
            [com.phronemophobic.schematic.model :as sm]
            [clojure.edn :as edn]
            [membrane.components.code-editor.code-editor :as code-editor]
            [com.phronemophobic.schematic.view.component-picker
             :refer [component-picker]]
            [membrane.alpha.component.drag-and-drop :as dnd]
            [com.phronemophobic.schematic.view.util
             :refer [uicall
                     drag-elem-target]]
            [liq.buffer :as buffer]
            [membrane.skia :as skia])
)



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

(defn eval+* [o]
  (eval o))

(def eval+ (memoize eval+*))

(defmethod compile* ::sm/paragraph [ctx
                                    {:keys [element/text
                                            element/width
                                            element/paragraph-style]}]
  (let [text (compile ctx text)
        width (compile ctx width)
        paragraph-style (compile ctx width)]
    (prn {:text text})
    (when (string? text)
      (para/paragraph text
                      width
                      paragraph-style))))

(defmethod compile* ::sm/button [ctx
                                 {:keys [element/text
                                         element/on-click
                                         $elem
                                         extra
                                         context
                                         $context
                                         $extra]}]
  (ui/button
   (compile ctx text)))

(defmethod compile* ::sm/text-input [ctx
                                     {:keys [element/text
                                             $elem
                                             extra
                                             context
                                             $context
                                             $extra]}]
  (ui/no-events
   (basic/textarea {:text (compile ctx text)}))) 

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
        f (eval+ fcode)]
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
      (compile (update-in ctx
                          [:context :bindings]
                          (fn [bindings]
                            (into (or bindings {})
                                  (map (fn [[k v]]
                                         [(symbol k) v]))
                                  defaults)))
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
                     :$elem [$elem (list 'nth i)]
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
                                              :flex/layout]}]
  (let [{:keys [$elem extra $extra context $context]} ctx]
   (ui/vertical-layout
    ;; (ui/label "flex-layout")
    (if children
      (apply
       ui/vertical-layout
       (compile
        (assoc ctx
               :$elem [$elem (list 'keypath :element/children)]
               :extra (get extra ::children)
               :$extra [$extra (list 'keypath ::children)]
               :context context
               :$context $context)
        children))
      (uicall drag-elem-target
              {:elem children
               :$elem [$elem (list 'keypath :element/children)]})))))



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

(defui editor [{:keys [elem]}]
  (if (nil? elem)
    (drag-elem-target {:elem elem})
    (compile
     {:$elem $elem
      :extra extra
      :$extra $extra
      :context context
      :$context $context}
     elem)))

(defui editor+component-picker [{:keys [elem]}]
  (dnd/drag-and-drop
   {:$body nil
    :body
    (ui/vertical-layout
     (ui/horizontal-layout
      (component-picker {})
      (ui/vertical-layout
       (basic/button {:text "clear"
                      :on-click (fn []
                                  [[:set $elem nil]])})
       (editor {:elem elem})))
     (let [inspector-extra (get extra ::inspector-extra)]
       (viscous/inspector {:obj (viscous/wrap elem)
                           :width (get inspector-extra :width 40)
                           :height (get inspector-extra :height 1)
                           :show-context? (get inspector-extra :show-context?)
                           :extra inspector-extra})))}))


(def app-state (atom {}))

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
            (basic/button {:text "clear"
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
          (basic/button {:text "delete"
                         :on-click (fn []
                                     [[::sm/delete-selection
                                       {:$elem $elem
                                        :selection selection
                                        :$selection $selection}]])}))}))))

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

