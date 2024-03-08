(ns com.phronemophobic.schematic.view.component-picker
  (:require [membrane.component :refer [defui defeffect]]
            [membrane.basic-components :as basic]
            [membrane.ui :as ui]
            [com.phronemophobic.schematic.model :as sm]
            [membrane.alpha.component.drag-and-drop :as dnd]))


(def component-starters
  {::sm/group {:element/type ::sm/group
               :element/children []}
   ::sm/for {:element/type ::sm/for
             :element/body nil
             :element.for/x 'x
             :element.for/xs {:element/type ::sm/code
                              :element/code [1 2 3]}}
   ::sm/code {:element/type ::sm/code
              :element/code [1 2 3]}
   ::sm/flex-layout {:element/type ::sm/flex-layout}
   
   ::sm/let {:element/type ::sm/let
             :element/body nil
             :element/bindings []}
   ::sm/component {:element/type ::sm/component
                   :component/name 'my-component
                   :component/args []
                   :component/body nil}
   ::sm/paragraph {:element/type ::sm/paragraph
                   :element/text nil}
   ::sm/button {:element/type ::sm/button
                :element/on-click '(fn [] [])
                :element/text "doit"}
   ::sm/checkbox {:element/type ::sm/checkbox
                  :element/checked? true}
   ::sm/text-input {:element/type ::sm/text-input
                    :element/text "hello"}})

(defui component-picker [{:keys []}]
  (apply
   ui/vertical-layout
   (eduction
    (map
     (fn [kind]
       (ui/on
        :mouse-down
        (fn [_]
          [[::dnd/drag-start {::sm/element
                              (assoc (get component-starters kind)
                                     :element/id (random-uuid))}]])
        (basic/button {:text (name kind)
                       :hover? (get extra [:hover kind])}))))

    (sort (keys component-starters)))))
