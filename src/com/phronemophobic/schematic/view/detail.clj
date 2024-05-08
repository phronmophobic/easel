(ns com.phronemophobic.schematic.view.detail
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
            [com.phronemophobic.membrandt :as ant]
            [clojure.edn :as edn]
            [com.phronemophobic.schematic.view.component-picker
             :refer [component-picker]]
            [com.phronemophobic.schematic.view.util
             :refer [uicall
                     on-drag-hover
                     drag-elem-target
                     code-editor]]
            [membrane.alpha.component.drag-and-drop :as dnd]
            [liq.buffer :as buffer]
            [membrane.skia :as skia]))




(defmulti compile*
  (fn [m]
    (:element/type (:elem m))))

(defn compile [m]
  (let [elem (:elem m)]
    (cond
      (map? elem)
      (compile* m)


      (or (string? elem)
          (nil? elem))
      (ui/label "no detail view.")

      (seq? elem)
      (ui/label "no detail view.")

      (seqable? elem)
      (ui/label "no detail view.")

      :else
      (ui/label "no detail view."))))

(defmethod compile* :default [m]
  (ui/label "no detail view."))

(defmacro defeditor [type & fn-tail]
  (let [namespace-sanitized (str/replace (namespace type)
                                         #"\."
                                         "-")
        fname (symbol
               (str "editor-" namespace-sanitized "-" (name type)))]
    `(do
       (defui ~fname ~@fn-tail)
       (defmethod compile* ~type [m#]
         (~fname m#)))))


(defeffect ::sm/delete-selection [{:keys [$elem selection $selection]}]
  (when-let [id (first selection)]
    (dispatch! :update
               $elem
               (fn [elem]
                 (specter/setval (sm/elem-by-id id)
                                 specter/NONE
                                 elem))))
  (dispatch! :set $selection #{}))

(defeditor ::sm/checkbox [{:keys [elem]}]
  (let [code (:element/checked? elem)
        src (:element/code code)]
   (ui/horizontal-layout
    (ui/label "checked:")
    (code-editor {:code src}))))


(defeditor ::sm/button [{:keys [elem]}]
  (let [code (:element/text elem)
        src (:element/code code)]
    (ui/horizontal-layout
     (ui/label "text:")
     (code-editor {:code src}))))


(defeditor ::sm/number-slider [{:keys [elem]}]
  (let [value (:element/value elem)
        src (:element/code value)]
    (ui/horizontal-layout
     (ui/label "value:")
     (code-editor {:code src}))))


(defeditor ::sm/radio-bar [{:keys [elem]}]

  (let [size (:element/size elem )
        options (:radio-bar/options elem)
        selection (:radio-bar/selection elem)]

    (ui/vertical-layout
     (ui/horizontal-layout
      (ui/label "options:")
      (code-editor {:code (:element/code options)}))
     (ui/horizontal-layout
      (ui/label "selection")
      (code-editor {:code (:element/code selection)})))))


(defeditor ::sm/progress-bar [{:keys [elem]}]
  (apply
   ui/vertical-layout
   (for [kw [:element/value
             :element/width
             :element/height]]
     (ui/horizontal-layout
      (ui/label (name kw))
      (let [code (get elem kw)
            src (get code :element/code)]
        (code-editor {:code src}))))))

(defeditor ::sm/number-slider [{:keys [elem]}]
  (apply
   ui/vertical-layout
   (for [kw [:element/value
             :number-slider/max
             :number-slider/min
             :number-slider/integer?
             :element/width]]
     (ui/horizontal-layout
      (ui/label (name kw))
      (let [code (get elem kw)
            src (get code :element/code)]
        (code-editor {:code src}))))))


(defeditor ::sm/defui [{:keys [elem]}]
  (let [data (:element/data elem)]
    (apply
     ui/vertical-layout
     (for [kw (keys data)]
       (ui/horizontal-layout
        (ui/label (name kw))
        (let [code (get data kw)
              src (get code :element/code)]
          (code-editor {:code src})))))))


(defeditor ::sm/flex-layout [{:keys [elem]}]
  (let [layout (:flex/layout elem)]
    (apply
     ui/vertical-layout
     (ant/radio-bar
      {:size :small
       :options [{:text "row"
                  :value :row}
                 {:text "column"
                  :value :column}]
       :selection (:direction layout)})
     (ui/label "justify-content")
     (ant/radio-bar
      {:size :small
       :options
       (into []
             (map (fn [kw]
                    {:text (name kw)
                     :value kw}))
             [:start :end :center :space-between :space-around :space-evenly])
       :selection (:justify-content layout)})
     (ui/label "align:")
     (ant/radio-bar
      {:size :small
       :options
       (into []
             (map (fn [kw]
                    {:text (name kw)
                     :value kw}))
             [:start :center :end])
       :selection (:align layout)})
     (for [kw [:width
               :height
               :gap]]
       (ui/horizontal-layout
        (ui/label (name kw))
        (let [src (get layout kw)]
          (code-editor {:code src})))))))



(defui editor [{:keys [elem] :as m}]
  (compile m))

(comment

  (def app-state (atom {:elem {:element/type ::sm/checkbox
                               :element/checked? {:element/type ::sm/code
                                                  :element/code true}}}))
  (skia/run
    (membrane.component/make-app #'editor
                                  app-state))

  ,)
