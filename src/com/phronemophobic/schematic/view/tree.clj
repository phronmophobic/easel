(ns com.phronemophobic.schematic.view.tree
  (:refer-clojure :exclude [compile load-file])
  (:require [membrane.component :refer [defui defeffect]]
            [membrane.basic-components :as basic]
            [membrane.ui :as ui]
            [clojure.string :as str]
            [clojure.set :as set]
            [membrane.skia.paragraph :as para]
            [com.rpl.specter :as specter]
            [com.phronemophobic.schematic.model :as sm]
            [clojure.edn :as edn]
            [com.phronemophobic.schematic.view.component-picker
             :refer [component-picker]]
            [com.phronemophobic.schematic.view.util
             :refer [uicall
                     on-drag-hover
                     drag-elem-target
                     code-editor
                     string-editor
                     symbol-editor]]
            [membrane.alpha.component.drag-and-drop :as dnd]

            [membrane.skia :as skia])
)



;; declare
(defui editor [{:keys [elem]}])

(defui drag-elem-wrap-target [{:keys [elem drag-object body]}]
  (dnd/on-drop
   (fn [pos obj]
     (let [intents [[:set $drag-object nil]]]
       (if-let [new-elem (::sm/element obj)]
         (let [new-elem (if (::replace? obj)
                          new-elem
                          (sm/set-child new-elem elem))]
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
       [[::dnd/drag-start {::dnd/obj {::sm/element elem
                                      ::replace? true}
                           ::delete id}]
        [::sm/toggle-selection {:element/id id
                                :$selection $selection}]])
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
                                             element/id]
                                      :as elem} :elem
                                     :keys [
                                            $elem
                                            extra
                                            context
                                            $context
                                            $extra]}]
  (ui/vertical-layout
   (uicall
    component-title
    {:text "(component"
     :selection (:selection context)
     :$selection (:$selection context)
     :elem elem
     :$elem $elem
     :id id})
   (ui/horizontal-layout
    (ui/label "name: ")
    (uicall symbol-editor
            {:symbol name
             :$symbol [$elem (list 'keypath :component/name)]}))
   (ui/translate
    20 10
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
     :$selection (:$selection context)
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
     :$selection (:$selection context)
     :id id})
   (ui/translate
    20 3
    (ui/vertical-layout
     (compile
      {:elem text
       :$elem [$elem (list 'keypath :element/text)]
       :extra (get extra ::text)
       :$extra [$extra (list 'keypath ::text)]
       :context context
       :$context $context})))))


(defmethod compile* ::sm/radio-bar [{{:keys [element/size
                                             radio-bar/options
                                             element/id]
                                      :as elem} :elem
                                     :keys [$elem
                                            extra
                                            context
                                            $context
                                            $extra]}]
  (uicall
   component-title
   {:text "radio-bar"
    :elem elem
    :$elem $elem
    :selection (:selection context)
    :$selection (:$selection context)
    :id id}))

(defmethod compile* ::sm/number-slider [{{:keys [element/value
                                                 element/width
                                                 number-slider/max
                                                 number-slider/min
                                                 number-slider/integer?
                                                 element/id]
                                          :as elem} :elem
                                         :keys [$elem
                                                extra
                                                context
                                                $context
                                                $extra]}]
  (uicall
   component-title
   {:text "number-slider"
    :elem elem
    :$elem $elem
    :selection (:selection context)
    :$selection (:$selection context)
    :id id}))

(defmethod compile* ::sm/progress-bar [{{:keys [element/value
                                                element/width
                                                element/height
                                                element/id]
                                         :as elem} :elem
                                        :keys [$elem
                                               extra
                                               context
                                               $context
                                               $extra]}]
  (uicall
   component-title
   {:text "progress-bar"
    :elem elem
    :$elem $elem
    :selection (:selection context)
    :$selection (:$selection context)
    :id id}))

(defmethod compile* ::sm/defui [{{:keys [element/name
                                         element/function
                                         element/data
                                         element/id]
                                  :as elem} :elem
                                 :keys [$elem
                                        extra
                                        context
                                        $context
                                        $extra]}]
  (uicall
   component-title
   {:text name
    :elem elem
    :$elem $elem
    :selection (:selection context)
    :$selection (:$selection context)
    :id id}))

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
     :$selection (:$selection context)
     :id id})
   (ui/translate
    20 3
    (ui/vertical-layout
     (compile
      {:elem text
       :$elem [$elem (list 'keypath :element/text)]
       :extra (get extra ::text)
       :$extra [$extra (list 'keypath ::text)]
       :context context
       :$context $context})
     #_(uicall code-editor
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
     :$selection (:$selection context)
     :id id})
   (ui/translate
    20 3
    (ui/vertical-layout
     (uicall code-editor
             {:code checked?
              :$code [$elem (list 'keypath :element/checked?)]})))))


(defmethod compile* ::sm/code [{{:keys [element/code
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
    {:text "code"
     :elem elem
     :$elem $elem
     :selection (:selection context)
     :$selection (:$selection context)
     :id id})
   (ui/translate
    20 3
    (uicall code-editor
            {:code code
             :$code [$elem (list 'keypath :element/code)]})))
  )


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
        :$selection (:$selection context)
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
     :$selection (:$selection context)
     :elem elem
     :$elem $elem
     :id id})
   (if (seq children)
     (ui/translate
      20 0
      (ui/vertical-layout
       (apply
        ui/vertical-layout
        (eduction
         (map-indexed (fn [i child]
                        (ui/vertical-layout
                         (uicall drag-elem-target
                                 {:elem children
                                  :$elem [$elem (list 'keypath :element/children)
                                          (specter/before-index i)]
                                  :drag-object (get extra [::drag-object i])
                                  :$drag-object [$extra (list 'keypath [::drag-object i])]})
                         (compile
                          {:elem child
                           :$elem [$elem (list 'keypath :element/children) (list 'nth i)]
                           :extra (get extra [::child i])
                           :$extra [$extra (list 'keypath [::child i])]
                           :context context
                           :$context $context}))))
         children))
       (uicall drag-elem-target
               {:elem children
                :$elem [$elem (list 'keypath :element/children)
                        specter/AFTER-ELEM]})))
     ;; else
     (uicall drag-elem-target
             {:elem children
              :$elem [$elem (list 'keypath :element/children)
                      specter/AFTER-ELEM]}))))

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
     :$selection (:$selection context)
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
     :$selection (:$selection context)
     :id id
     :elem elem
     :$elem $elem})
   (ui/fixed-bounds
    (ui/bounds (para/paragraph "↔️"))
    (if (= (:direction layout)
           :column)
      (ui/on
       :mouse-down
       (fn [_]
         [[:set [$elem
                 (list 'keypath :flex/layout)
                 (list 'keypath :direction)]
           :row]])
       (para/paragraph "↕️"))
      (ui/on
       :mouse-down
       (fn [_]
         [[:set [$elem
                 (list 'keypath :flex/layout)
                 (list 'keypath :direction)]
           :column]])
       (para/paragraph "↔️"))
      ))
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

(defmethod compile* ::sm/relative-layout [{{:keys [element/children
                                                   element/id
                                                   relative/layout]
                                            :as elem} :elem
                                           :keys [$elem
                                                  extra
                                                  context
                                                  $context
                                                  $extra]}]
  (ui/vertical-layout
   (uicall
    component-title
    {:text "(relative"
     :selection (:selection context)
     :$selection (:$selection context)
     :id id
     :elem elem
     :$elem $elem})
   (if (seq children)
     (ui/translate
      20 0
      (ui/vertical-layout
       (apply
        ui/vertical-layout
        (eduction
         (map-indexed (fn [i child]
                        (ui/vertical-layout
                         (uicall drag-elem-target
                                 {:elem children
                                  :$elem [$elem (list 'keypath :element/children)
                                          (specter/before-index i)]
                                  :drag-object (get extra [::drag-object i])
                                  :$drag-object [$extra (list 'keypath [::drag-object i])]})
                         (compile
                          {:elem child
                           :$elem [$elem (list 'keypath :element/children) (list 'nth i)]
                           :extra (get extra [::child i])
                           :$extra [$extra (list 'keypath [::child i])]
                           :context context
                           :$context $context}))))
         children))
       (uicall drag-elem-target
               {:elem children
                :$elem [$elem (list 'keypath :element/children)
                        specter/AFTER-ELEM]})))
     ;; else
     (uicall drag-elem-target
             {:elem children
              :$elem [$elem (list 'keypath :element/children)
                      specter/AFTER-ELEM]}))))



(defui editor [{:keys [elem]}]
  (let [body (if (nil? elem)
               (drag-elem-target {:elem elem})
               (ui/on
                ;; this is a little too specific,
                ;; but need other unimplemented features
                ;; to avoid this specificity
                ::dnd/drag-start
                (fn [m]
                  (if-let [id (::delete m)]
                    [[::dnd/drag-start (update m ::dnd/init
                                               (fn [intents]
                                                 (let [new-intent [::sm/delete-by-id {:id id
                                                                                      :$elem $elem}]]
                                                   (if intents
                                                     (conj intents new-intent)
                                                     [new-intent]))))]]
                    [[::dnd/drag-start m]]))
                (compile
                 {:elem elem
                  :$elem $elem
                  :extra extra
                  :$extra $extra
                  :context context
                  :$context $context})))

        [cw ch :as container-size] (:membrane.stretch/container-size context)

        body (if container-size
               (basic/scrollview
                {:body body
                 :$body nil

                 :scroll-bounds
                 ;; save space for scroll bar
                 [(- cw 7)
                  (- ch 7)]})
               body)]
    body))

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



(comment
  (skia/run
    (membrane.component/make-app #'debug {}))

  (s)
  ,)





