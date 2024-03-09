(ns com.phronemophobic.schematic.view.tree
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
            [com.phronemophobic.schematic.view.util
             :refer [uicall
                     on-drag-hover
                     drag-elem-target]]
            [membrane.alpha.component.drag-and-drop :as dnd]
            [liq.buffer :as buffer]
            [membrane.skia :as skia])
)



;; declare
(defui editor [{:keys [elem]}])

(defui drag-elem-wrap-target [{:keys [elem drag-object body]}]
  (dnd/on-drop
   (fn [pos obj]
     (let [intents [[:set $drag-object nil]]]
       (if-let [new-elem (::sm/element obj)]
         (let [new-elem (sm/set-child new-elem elem)]
           (conj intents [:set $elem new-elem]))
         intents)))
   (on-drag-hover
    {:$body nil
     :object drag-object
     :body
     (let [body (if drag-object
                  (ui/fill-bordered
                   [1 0 0 0.2]
                   0
                   body)
                  body)]
       body)})))

(defui component-title [{:keys [text
                                elem
                                id
                                ^:membrane.component/contextual
                                selection]}]
  (drag-elem-wrap-target
   {:elem elem
    :$body nil
    :body
    (ui/on
     :mouse-down
     (fn [_]
       [[::sm/toggle-selection {:element/id id}]])
     (para/paragraph
      {:text text
       :style #:text-style {:font-style #:font-style{:weight :bold}
                            :color
                            (if (contains? selection id)
                              [1 0 0]
                              [0 0 0])}}))}))

(def hr (ui/with-stroke-width 4
          (ui/with-color [0 0 0]
            (ui/with-style :membrane.ui/style-stroke
              (ui/path [0 0]
                       [400 0])))))


(defmulti compile*
  (fn [o]
    (-> o :elem :element/type)))

(defn compile [o]
  (let [elem (:elem o)]
    (cond
      (map? elem)
      (compile* o)


      (or (string? elem)
          (nil? elem))
      elem

      (seq? elem)
      elem

      (seqable? elem)
      (into (empty elem)
            (map compile)
            elem)

      :else
      elem)))


(defui symbol-editor [{:keys [symbol editing? symbol-name] :as m}]
  (if (not editing?)
    (ui/on
     :mouse-down
     (fn [_]
       [[:set $editing? true]
        [:set $symbol-name (str symbol)]])
     (ui/label symbol))
    (ui/wrap-on
     :key-press
     (fn [handler s]
       
       (if (= s :enter)
         [[:set $editing? false]
          [:set $symbol (clojure.core/symbol symbol-name)]]
         (handler s)))
     (ui/horizontal-layout
      (basic/button {:text "O"
                     :on-click
                     (fn []
                       [[:set $editing? false]
                        [:set $symbol (clojure.core/symbol symbol-name)]])})
      (basic/button {:text "X"
                     :on-click
                     (fn []
                       [[:set $editing? false]])})
      (basic/textarea {:text symbol-name})))))

(defui string-editor [{:keys [string editing? edit-string] :as m}]
  (if (not editing?)
    (ui/on
     :mouse-down
     (fn [_]
       [[:set $editing? true]
        [:set $edit-string string]])
     (ui/label string))
    (ui/wrap-on
     :key-press
     (fn [handler s]
       (if (= s :enter)
         [[:set $editing? false]
          [:set $string edit-string]]
         (handler s)))
     (ui/horizontal-layout
      (basic/button {:text "O"
                     :on-click
                     (fn []
                       [[:set $editing? false]
                        [:set $string edit-string]])})
      (basic/button {:text "X"
                     :on-click
                     (fn []
                       [[:set $editing? false]])})
      (basic/textarea {:text edit-string})))))

(defui code-editor [{:keys [code editing? buf] :as m}]
  (if (not editing?)
    (let [inspector-extra (get extra ::inspector-extra)]
      (ui/horizontal-layout
       (basic/button {:text "O"
                      :on-click
                      (fn []
                        [[:set $editing? true]
                         [:set $buf (buffer/buffer (pr-str code) {:mode :insert})]])})
       (viscous/inspector {:obj (viscous/wrap code)
                           :width (get inspector-extra :width 40)
                           :height (get inspector-extra :height 1)
                           :show-context? (get inspector-extra :show-context?)
                           :extra inspector-extra})))
    (ui/horizontal-layout
     (basic/button {:text "O"
                    :on-click
                    (fn []
                      [[:set $editing? false]
                       [:update $code
                        (fn [old-code]
                          (try
                            (edn/read-string (buffer/text buf))
                            (catch Exception e
                              old-code)))]])})
     (basic/button {:text "X"
                    :on-click
                    (fn []
                      [[:set $editing? false]])})
     (code-editor/text-editor {:buf buf}))))

(defui args-editor [{:keys [args]}]
  (apply
   ui/vertical-layout
   (basic/button {:text "+"
                  :on-click (fn []
                              [[:update $args conj 'foo]])})
   (for [arg args]
     (ui/horizontal-layout
      (basic/button {:text "X"
                     :hover? (get extra [:hover $arg])
                     :on-click (fn []
                                 [[:delete $arg]])})
      (symbol-editor {:symbol arg})))))

(comment
  (skia/run
    (membrane.component/make-app #'args-editor {:args '[a b c d]}))
  ,)



(defmethod compile* ::sm/component [{{:keys [component/name
                                             component/args
                                             component/body
                                             component/defaults
                                             element/id]} :elem
                                     :keys [
                                            $elem
                                            extra
                                            context
                                            $context
                                            $extra]}]
  (ui/vertical-layout
   (ui/horizontal-layout
    (ui/label "name: ")
    (uicall symbol-editor
            {:symbol name
             :$symbol [$elem (list 'keypath :component/name)]}))
   (ui/horizontal-layout
    (ui/label "args: ")
    (uicall args-editor
            {:args args
             :$args [$elem (list 'keypath :component/args)]}))
   (ui/horizontal-layout
    (ui/label "defaults: ")
    (uicall code-editor
            {:code defaults
             :$code [$elem (list 'keypath :component/defaults)]}))
   (ui/translate
    20 0
    (if body
      (compile
       {:elem body
        :$elem [$elem (list 'keypath :component/body)]
        :extra (get extra :component/body)
        :$extra [$extra (list 'keypath :component/body)]
        :context context
        :$context $context})
      
      ;; else
      (uicall drag-elem-target
              {:elem body
               :$elem [$elem (list 'keypath :component/body)]})
      ))
   #_(symbol-editor {:symbol name
                     :$symbol [$elem (list 'keypath :component/name)]})
   ))

(defmethod compile* ::sm/paragraph [{{:keys [element/text
                                             element/width
                                             element/paragraph-style
                                             element/id]
                                      :as elem} :elem
                                     :keys [$elem
                                            extra
                                            context
                                            $context
                                            $extra]}]
  (ui/vertical-layout
   (uicall
    component-title
    {:text "(paragraph"
     :selection (:selection context)
     :elem elem
     :$elem $elem
     :id id})
   (ui/translate
    20 0
    (if text
      (compile
       {:elem text
        :$elem [$elem (list 'keypath :element/text)]
        :extra (get extra ::text)
        :$extra [$extra (list 'keypath ::text)]
        :context context
        :$context $context})
      (uicall drag-elem-target
              {:elem text
               :$elem [$elem (list 'keypath :element/text)]})))))

(defmethod compile* ::sm/button [{{:keys [element/text
                                         element/on-click
                                         element/id]
                                   :as elem} :elem
                                  :keys [$elem
                                         extra
                                         context
                                         $context
                                         $extra]}]
  (ui/vertical-layout
   (uicall
    component-title
    {:text "button"
     :elem elem
     :$elem $elem
     :selection (:selection context)
     :id id})
   (ui/translate
    20 3
    (ui/vertical-layout
     (uicall code-editor
             {:code text
              :$code [$elem (list 'keypath :element/text)]})
     (uicall code-editor
             {:code on-click
              :$code [$elem (list 'keypath :element/on-click)]})))))

(defmethod compile* ::sm/text-input [{{:keys [element/text
                                              element/id]
                                       :as elem} :elem
                                      :keys [$elem
                                             extra
                                             context
                                             $context
                                             $extra]}]
  (ui/vertical-layout
   (uicall
    component-title
    {:text "text-input"
     :elem elem
     :$elem $elem
     :selection (:selection context)
     :id id})
   (ui/translate
    20 3
    (ui/vertical-layout
     (uicall code-editor
             {:code text
              :$code [$elem (list 'keypath :element/text)]}))))) 

(defmethod compile* ::sm/checkbox [{{:keys [element/checked?
                                            element/id]
                                     :as elem} :elem
                                    :keys [$elem
                                           extra
                                           context
                                           $context
                                           $extra]}]
  (ui/vertical-layout
   (uicall
    component-title
    {:text "checkbox"
     :elem elem
     :$elem $elem
     :selection (:selection context)
     :id id})
   (ui/translate
    20 3
    (ui/vertical-layout
     (uicall code-editor
             {:code checked?
              :$code [$elem (list 'keypath :element/checked?)]})))))


(defmethod compile* ::sm/code [{{:keys [element/code]} :elem
                                :keys [$elem
                                       extra
                                       context
                                       $context
                                       $extra]}]
  (uicall code-editor
          {:code code
           :$code [$elem (list 'keypath :element/code)]}))


(defmethod compile* ::sm/let [{{:keys [element/body
                                       element/bindings
                                       element/id]
                                :as elem} :elem
                               :keys [$elem
                                      extra
                                      context
                                      $context
                                      $extra]}]
  (let [bindings-editor
        (apply
         ui/vertical-layout
         (eduction
          (map-indexed (fn [i {:let/keys [binding val]}]
                         (ui/vertical-layout
                          (uicall symbol-editor
                                  {:symbol binding
                                   :$symbol [$elem (list 'keypath :element/bindings) (list 'nth i) (list 'keypath :let/binding)]})
                          (compile
                           {:elem val
                            :$elem [$elem (list 'keypath :element/bindings) (list 'nth i) (list 'keypath :let/val)]
                            :extra (get extra [::binding ::val i])
                            :$extra [$extra (list 'keypath [::binding ::val i])]
                            :context context
                            :$context $context}))))
          bindings))
        body-editor
        (when body
          (compile
           {:elem body
            :$elem [$elem (list 'keypath :element/body)]
            :extra (get extra ::body)
            :$extra [$extra (list 'keypath ::body)]
            :context context
            :$context $context}))]
    (ui/translate
     3 3
     (ui/vertical-layout
      (uicall
       component-title
       {:text "(let"
        :elem elem
        :$elem $elem
        :selection (:selection context)
        :id id})
      (ui/translate
       13 3
       (ui/vertical-layout
        bindings-editor
        hr
        (ui/translate 5 10
         body-editor)))))))


(defmethod compile* ::sm/group [{{:keys [element/children
                                         element/id]
                                  :as elem} :elem
                                 :keys [$elem
                                        extra
                                        context
                                        $context
                                        $extra]}]
  (ui/vertical-layout
   (uicall
    component-title
    {:text "(group"
     :selection (:selection context)
     :elem elem
     :$elem $elem
     :id id})
   (uicall drag-elem-target
           {:elem children
            :$elem [$elem (list 'keypath :element/children)
                    specter/AFTER-ELEM]})
   (ui/translate
    20 0
    (apply
     ui/vertical-layout
     (eduction
      (map-indexed (fn [i child]
                     (compile
                      {:elem child
                       :$elem [$elem (list 'keypath :element/children) (list 'nth i)]
                       :extra (get extra [::child i])
                       :$extra [$extra (list 'keypath [::child i])]
                       :context context
                       :$context $context})))
      children)))))

(defmethod compile* ::sm/for [{{:keys [element/body
                                       element.for/x
                                       element.for/xs
                                       element/id]
                                :as elem} :elem
                               :keys [
                                      $elem
                                      extra
                                      context
                                      $context
                                      $extra]}]
  (ui/horizontal-layout
   (uicall
    component-title
    {:text "(for"
     :elem elem
     :$elem $elem
     :selection (:selection context)
     :id id})
   (ui/translate
    5 0
    (ui/vertical-layout             
     (uicall symbol-editor
             {:symbol x
              :$symbol [$elem (list 'keypath :element.for/x)]})
     (compile
      {:elem xs
       :$elem [$elem (list 'keypath :element.for/xs)]
       :extra (get extra ::xs)
       :$extra [$extra (list 'keypath ::xs)]
       :context context
       :$context $context})
     hr
     (if body
       (compile
        {:elem body
         :$elem [$elem (list 'keypath :element/body)]
         :extra (get extra ::body)
         :$extra [$extra (list 'keypath ::body)]
         :context context
         :$context $context})
       ;; else
       (uicall drag-elem-target
               {:elem body
                :$elem [$elem (list 'keypath :element/body)]})
       )))))

(defui drag-elem-wrap-target [{:keys [elem drag-object body]}]
  (dnd/on-drop
   (fn [pos obj]
     (let [intents [[:set $drag-object nil]]]
       (if-let [new-elem (::sm/element obj)]
         (let [new-elem (sm/set-child new-elem elem)]
           ;; (println "new child: ------------")
           ;; (prn $elem)
           ;; (clojure.pprint/pprint new-elem)
           (conj intents [:set $elem new-elem]))
         intents)))
   (on-drag-hover
    {:$body nil
     :object drag-object
     :body
     (let [body (if drag-object
                  (ui/fill-bordered
                   [1 0 0 0.2]
                   0
                   body)
                  body)]
       body)})))

(defmethod compile* ::sm/flex-layout [{{:keys [element/children
                                               element/id
                                               flex/layout]
                                        :as elem} :elem
                                       :keys [$elem
                                              extra
                                              context
                                              $context
                                              $extra]}]
  (ui/vertical-layout
   (uicall
    component-title
    {:text "(flex-layout"
     :selection (:selection context)
     :id id
     :elem elem
     :$elem $elem})
   (ui/translate
    20 0
    (if (vector? children)
      (apply
       ui/vertical-layout
       (eduction
        (map-indexed (fn [i child]
                       (compile
                        {:elem child
                         :$elem [$elem (list 'keypath :element/children) (list 'nth i)]
                         :extra (get extra [::child i])
                         :$extra [$extra (list 'keypath [::child i])]
                         :context context
                         :$context $context})))
        children))
      (if children
        (compile
         {:elem children
          :$elem [$elem (list 'keypath :element/children)]
          :extra (get extra ::children)
          :$extra [$extra (list 'keypath ::children)]
          :context context
          :$context $context})
        ;; else
        (uicall drag-elem-target
                {:elem children
                 :$elem [$elem (list 'keypath :element/children)]})
        )))))



(defui editor [{:keys [elem]}]
  (if (nil? elem)
    (drag-elem-target {:elem elem})
    (compile
     {:elem elem
      :$elem $elem
      :extra extra
      :$extra $extra
      :context context
      :$context $context})))

(def app-state (atom {}))

(defui editor+component-picker [{:keys [elem]}]
  (dnd/drag-and-drop
   {:$body nil
    :body
    (ui/horizontal-layout
     (component-picker {})
     (ui/vertical-layout
      (basic/button {:text "clear"
                     :on-click (fn []
                                 [[:set $elem nil]])})
      (editor {:elem elem})))}))

(comment
  (reset! app-state
          {:elem nil})
  (skia/run
    (membrane.component/make-app #'editor+component-picker app-state))

  (clojure.pprint/pprint
   (sm/compile (:elem @app-state) ))
  ,)

;; selection


(;; :com.phronemophobic.schematic.model/code
 ;; :com.phronemophobic.schematic.model/group
 ;; :com.phronemophobic.schematic.model/for
 ;; :com.phronemophobic.schematic.model/flex-layout
 ;; :com.phronemophobic.schematic.model/let
 ;; :com.phronemophobic.schematic.model/component
 )





(defui drag-target [{:keys [object drag-object]}]
  (dnd/on-drop
   (fn [pos obj]
     [[:set $object obj]
      [:set $drag-object nil]])
   (on-drag-hover
    {:$body nil
     :object drag-object
     :body
     [(ui/spacer 100 100)
      (if drag-object
        (ui/fill-bordered [1 0 0 0.2]
                          0
                          (ui/label (pr-str drag-object)))
        (ui/label (pr-str object)))]
     }
    )))

(defui debug [{}]
  (dnd/drag-and-drop
   {:$body nil
    :body
    (ui/horizontal-layout
     (component-picker {})
     (drag-target {}))
    }))

(comment
  (skia/run
    (membrane.component/make-app #'debug {}))

  (s)
  ,)



(membrane.component/defui
   my-todo-item
   [{:keys [todo]}]
   (clojure.core/when-let
    [elem__30144__auto__
     (clojure.core/apply
      membrane.ui/horizontal-layout
      (clojure.core/when-let
       [elem__30144__auto__
        [(clojure.core/when-let
          [elem__30144__auto__
           (membrane.basic-components/checkbox
            {:checked? (:complete? todo)})]
          (clojure.core/with-meta
           elem__30144__auto__
           '#:com.phronemophobic.schematic.model{:ast
                                                 #:element{:id
                                                           #uuid "0da82766-7473-43fc-b67d-257b438cd870"}}))
         (clojure.core/when-let
          [elem__30144__auto__
           (membrane.basic-components/textarea
            {:text (:description todo)})]
          (clojure.core/with-meta
           elem__30144__auto__
           '#:com.phronemophobic.schematic.model{:ast
                                                 #:element{:id
                                                           #uuid "a7b77333-7b28-4353-bcc0-ec79c1443325"}}))]]
       (clojure.core/with-meta
        elem__30144__auto__
        '#:com.phronemophobic.schematic.model{:ast
                                              #:element{:id
                                                        #uuid "87792c32-5333-4de7-9145-b722a0dd271d"}})))]
    (clojure.core/with-meta
     elem__30144__auto__
     '#:com.phronemophobic.schematic.model{:ast
                                           #:element{:id
                                                     #uuid "038ada34-c5f3-42c3-adfa-a01075702235"}})))



(membrane.component/defui
   my-todo-list
   [{:keys [todos]}]
   (clojure.core/when-let
    [elem__30144__auto__
     (clojure.core/apply
      membrane.ui/horizontal-layout
      (clojure.core/when-let
       [elem__30144__auto__
        (clojure.core/vec
         (clojure.core/for [todo todos] (my-todo-item {:todo todo})))]
       (clojure.core/with-meta
        elem__30144__auto__
        '#:com.phronemophobic.schematic.model{:ast
                                              #:element{:id
                                                        #uuid "575828dc-3aa6-44f3-9203-c3597cc9f7ee"}})))]
    (clojure.core/with-meta
     elem__30144__auto__
     '#:com.phronemophobic.schematic.model{:ast
                                           #:element{:id
                                                     #uuid "881f943e-6953-403e-a0da-86de200f7ccc"}})))

(comment
  (skia/run
    (membrane.component/make-app
     #'my-todo-list
     {:todos [{:complete? true
               :description "drink cofffee"}
              {:complete? false
               :description "drink cofffee2"}]}))
  ,)



