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
            [membrane.skia :as skia]))

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
