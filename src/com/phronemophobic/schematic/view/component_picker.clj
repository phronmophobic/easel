(ns com.phronemophobic.schematic.view.component-picker
  (:require [membrane.component :refer [defui defeffect]]
            [membrane.basic-components :as basic]
            [membrane.ui :as ui]
            [com.phronemophobic.membrandt :as ant]
            [com.phronemophobic.viscous :as viscous]
            [com.phronemophobic.schematic.model :as sm]
            [membrane.alpha.component.drag-and-drop :as dnd]))

(defui inspector [{:keys [obj]}]
  (let [inspector-extra (get extra ::inspector)]
    (viscous/inspector
     {:obj (viscous/wrap obj)
      :width (get inspector-extra :width 40)
      :height (get inspector-extra :height 1)
      :show-context? (get inspector-extra :show-context?)
      :extra inspector-extra})))

(defn ->code [form]
  {:element/type ::sm/code
   :element/code form
   :element/id (random-uuid)})

(def component-starters
  {::sm/group (fn []
                {:element/type ::sm/group
                 :element/id (random-uuid)
                 :element/children []})
   ::sm/for (fn []
              {:element/type ::sm/for
               :element/id (random-uuid)
               :element/body nil
               :element.for/x 'x
               :element.for/xs {:element/type ::sm/code
                                :element/code [1 2 3]}})
   ::sm/code (fn []
               {:element/type ::sm/code
                :element/id (random-uuid)
                :element/code "asdf"})
   ::sm/flex-layout (fn []
                      {:element/type ::sm/flex-layout
                       :element/children {:element/type ::sm/group
                                          :element/id (random-uuid)
                                          :element/children []}
                       :flex/layout {:gap 8
                                     :width (->code nil)
                                     :height (->code nil)}
                       :element/id (random-uuid)})
   ::inspector (fn []
                 {:element/type ::sm/defui
                  :element/name "inspector"
                  :element/function #'inspector
                  :element/data {:obj (->code nil)}
                  :element/id (random-uuid)})

   ::sm/relative-layout (fn []
                          {:element/type ::sm/relative-layout
                           :element/children []
                           :relative/layout {}
                           :element/id (random-uuid)})
   ::sm/let (fn []
              {:element/type ::sm/let
               :element/body nil
               :element/bindings []
               :element/id (random-uuid)})
   ::sm/component (fn []
                    {:element/type ::sm/component
                     :component/name 'my-component
                     :component/body nil
                     :element/id (random-uuid)})
   ::sm/paragraph (fn []
                    {:element/type ::sm/paragraph
                     :element/text {:element/type ::sm/code
                                    :element/id (random-uuid)
                                    :element/code "hello"}
                     :element/width {:element/type ::sm/code
                                     :element/id (random-uuid)
                                     :element/code nil}
                     :element/paragraph-style
                     {:element/type ::sm/code
                      :element/id (random-uuid)
                      :element/code
                      #:paragraph-style
                      {:text-style
                       #:text-style{:font-size 14,
                                    :height 1.5714285714285714,
                                    :height-override true,
                                    :color [0.0 0.0 0.0 0.88]}}}
                     :element/id (random-uuid)})
   ::sm/button (fn []
                 {:element/type ::sm/button
                  :element/on-click {:element/type ::sm/code
                                     :element/id (random-uuid)
                                     :element/code '(fn [] [])}
                  :element/text {:element/type ::sm/code
                                 :element/id (random-uuid)
                                 :element/code "doit"}
                  :element/id (random-uuid)})
   ::sm/checkbox (fn []
                   {:element/type ::sm/checkbox
                    :element/checked? {:element/type ::sm/code
                                       :element/id (random-uuid)
                                       :element/code true}
                    :element/id (random-uuid)})
   ::sm/text-input (fn []
                     {:element/type ::sm/text-input
                      :element/text
                      {:element/type ::sm/code
                       :element/id (random-uuid)
                       :element/code "hello"}
                      :element/id (random-uuid)})
   ::sm/radio-bar (fn []
                    {:element/type ::sm/radio-bar
                     :element/size {:element/type ::sm/code
                                    :element/id (random-uuid)
                                    :element/code :middle}
                     :radio-bar/options {:element/type ::sm/code
                                         :element/id (random-uuid)
                                         :element/code (into []
                                                             (map (fn [kw]
                                                                    {:value kw
                                                                     :text (name kw)}))
                                                             [:a :b :c])}
                     :radio-bar/selection {:element/type ::sm/code
                                           :element/id (random-uuid)
                                           :element/code nil}
                     :element/id (random-uuid)})
   ;; ::sm/number-input (fn [])
   ::sm/progress-bar (fn []
                       {:element/type ::sm/progress-bar
                        :element/value {:element/type ::sm/code
                                        :element/id (random-uuid)
                                        :element/code 0.5}
                        :element/width {:element/type ::sm/code
                                        :element/id (random-uuid)
                                        :element/code 200}
                        :element/height {:element/type ::sm/code
                                         :element/id (random-uuid)
                                         :element/code :middle}
                        :element/id (random-uuid)})
   ::sm/number-slider (fn []
                        {:element/type ::sm/number-slider
                         :element/value {:element/type ::sm/code
                                         :element/id (random-uuid)
                                         :element/code 2}
                         :number-slider/max {:element/type ::sm/code
                                             :element/id (random-uuid)
                                             :element/code 10}
                         :number-slider/min {:element/type ::sm/code
                                             :element/id (random-uuid)
                                             :element/code 0}
                         :number-slider/integer? {:element/type ::sm/code
                                                  :element/id (random-uuid)
                                                  :element/code true}
                         :element/width {:element/type ::sm/code
                                         :element/id (random-uuid)
                                         :element/code 200}
                         :element/id (random-uuid)})})



(defui component-picker [{:keys [components]}]
  (let [body
        (ui/translate
         4 4
         (apply
          ui/vertical-layout
          (eduction
           (map
            (fn [kind]
              (ui/on
               :mouse-down
               (fn [_]
                 [[::dnd/drag-start {::dnd/obj {::sm/element
                                                ((get components kind))}}]])
               (ant/button {:text (name kind)
                            :size :small
                            :extra (get extra [:button kind])
                            ;;:hover? (get extra [:hover kind])
                            }))))

           (sort (keys components)))))

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

