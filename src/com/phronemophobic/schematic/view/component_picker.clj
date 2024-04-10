(ns com.phronemophobic.schematic.view.component-picker
  (:require [membrane.component :refer [defui defeffect]]
            [membrane.basic-components :as basic]
            [membrane.ui :as ui]
            [com.phronemophobic.membrandt :as ant]
            [com.phronemophobic.schematic.model :as sm]
            [membrane.alpha.component.drag-and-drop :as dnd]))


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
                      :element/id (random-uuid)})
   
   ::sm/let (fn []
             {:element/type ::sm/let
              :element/body nil
              :element/bindings []
              :element/id (random-uuid)})
   ::sm/component (fn []
                   {:element/type ::sm/component
                    :component/name 'my-component
                    :component/args []
                    :component/body nil
                    :element/id (random-uuid)})
   ::sm/paragraph (fn []
                   {:element/type ::sm/paragraph
                    :element/text {:element/type ::sm/code
                                   :element/id (random-uuid)
                                   :element/code "hello"}
                    :element/id (random-uuid)})
   ::sm/button (fn []
                {:element/type ::sm/button
                 :element/on-click '(fn [] [])
                 :element/text "doit"
                 :element/id (random-uuid)})
   ::sm/checkbox (fn []
                  {:element/type ::sm/checkbox
                   :element/checked? true
                   :element/id (random-uuid)})
   ::sm/text-input (fn []
                    {:element/type ::sm/text-input
                     :element/text
                     {:element/type ::sm/code
                      :element/id (random-uuid)
                      :element/code "hello"}
                     :element/id (random-uuid)})})

(defui component-picker [{:keys []}]
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
           [[::dnd/drag-start {::sm/element
                               ((get component-starters kind))}]])
         (ant/button {:text (name kind)
                      :size :small
                      ;;:hover? (get extra [:hover kind])
                      }))))

     (sort (keys component-starters))))))
