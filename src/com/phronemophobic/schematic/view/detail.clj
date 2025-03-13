(ns com.phronemophobic.schematic.view.detail
  (:refer-clojure :exclude [compile load-file])
  (:require [membrane.component :refer [defui defeffect]]
            [membrane.basic-components :as basic]
            [membrane.ui :as ui]
            [clojure.string :as str]
            [clojure.zip :as z]
            [clojure.set :as set]
            [membrane.skia.paragraph :as para]
            [com.phronemophobic.viscous :as viscous]
            [com.rpl.specter :as specter]
            [com.phronemophobic.schematic.model :as sm]
            [com.phronemophobic.membrandt :as ant]
            [com.phronemophobic.membrandt.icon.ui :as icon.ui]
            [clojure.edn :as edn]
            [com.phronemophobic.schematic.view.component-picker
             :refer [component-picker]]
            [com.phronemophobic.schematic.view.util
             :refer [uicall
                     on-drag-hover
                     drag-elem-target
                     code-editor
                     symbol-editor]]
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

(defn ^:private var->sym [v]
  (symbol (-> v .ns .name name)
          (-> v .sym name)))

(defui var-drop [{:keys [body drag-object]}]
  (dnd/on-drop
   (fn [pos obj]
     (let [intents [[:set $drag-object nil]]
           x (:x obj)]
       (if-let [v (when (and x
                             (instance? clojure.lang.IDeref x))
                    (let [v @x]
                      (when (var? v)
                        v)))]
         (conj intents [::var-drop {:sym (var->sym v)}])
         intents)))
   (on-drag-hover
    {:$body nil
     :object drag-object
     :body
     (let [[bw bh] (ui/bounds body)
           rect-width (max 50 bw)
           rect-height (max 50 bh)
           rect (ui/filled-rectangle [1 0 0 0.2]
                                     rect-width rect-height)]
       [(when (:drop-object context)
          rect)
        body])})))

(defeditor ::sm/button [{:keys [elem]}]
  (ui/vertical-layout
   (let [code (:element/text elem)
         src (:element/code code)]
     (ui/horizontal-layout
      (ui/label "text:")
      (code-editor {:code src})))
   (apply
    ui/vertical-layout
    (for [prop [{:kw :ant.style/size
                 :default :middle}
                {:kw :ant.style/type
                 :default :default}
                {:kw :ant.style/danger?
                 :default false}
                {:kw :ant.style/disabled?
                 :default false}]]
      (if-let [val (get elem (:kw prop))]
        (let [src (:element/code val)]
          (ui/horizontal-layout
           (ui/on-click
            (fn []
              [[:update $elem
                dissoc (:kw prop)]])
            (icon.ui/icon
             {:name "delete"
              :hover? (get extra [:delete-hover? (:kw prop)])}))
           (ui/label (:kw prop))
           (code-editor {:code src})))
        (ant/button {:text (str (:kw prop))
                     :size :small
                     :extra (get extra prop)
                     :on-click
                     (fn []
                       [[:update $elem
                         (fn [elem]
                           (assoc elem
                                  (:kw prop)
                                  {:element/type ::sm/code
                                   :element/id (random-uuid)
                                   :element/code (:default prop)}))]])}))))
   (let [on-click (:element/on-click elem)
         click-type (:element/type on-click)]
     (ui/on
      ::var-drop
      (fn [m]
        [[::config-on-click (assoc m :element elem )]])
      (var-drop
       {:$body nil
        :body
        (when (= click-type ::sm/code)
          (let [src (:element/code on-click)]
            (code-editor {:code src})))})))))

(defeditor ::sm/text-input [{:keys [elem]}]
  (ui/vertical-layout
   (let [code (:element/text elem)
         src (:element/code code)]
     (ui/horizontal-layout
      (ui/label "text:")
      (code-editor {:code src})))
   (apply
    ui/vertical-layout
    (for [prop [{:kw :ant.style/size
                 :default :middle}
                {:kw :ant.style/status
                 :default :ok}
                {:kw :ant.style/disabled?
                 :default false}]]
      (if-let [val (get elem (:kw prop))]
        (let [src (:element/code val)]
          (ui/horizontal-layout
           (ui/on-click
            (fn []
              [[:update $elem
                dissoc (:kw prop)]])
            (icon.ui/icon
             {:name "delete"
              :hover? (get extra [:delete-hover? (:kw prop)])}))
           (ui/label (:kw prop))
           (code-editor {:code src})))
        (ant/button {:text (str (:kw prop))
                     :size :small
                     :extra (get extra prop)
                     :on-click
                     (fn []
                       [[:update $elem
                         (fn [elem]
                           (assoc elem
                                  (:kw prop)
                                  {:element/type ::sm/code
                                   :element/id (random-uuid)
                                   :element/code (:default prop)}))]])}))))
   (ant/radio-bar
    {:size :small
     :options
     (into []
           (map (fn [num]
                  {:text (pr-str num)
                   :value num}))
           [nil 1.0 2.0])
     :selection (:flex.grow/width elem)})
   (ui/horizontal-layout
    (ui/label :flex.grow/width)
    (let [src (get elem :flex.grow/width)]
      (code-editor {:code src})))))


(defeditor ::sm/component [{:keys [elem]}]
  (ui/flex-layout
   [(ui/horizontal-layout
     (ui/label "name:")
     (symbol-editor {:symbol (:component/name elem)}))
    (ui/horizontal-layout
     (ui/label "defaults:")
     (code-editor {:code (:component/defaults elem)}))]
   {:direction :column
    :gap 8}))


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
     (ant/radio-bar
      {:size :small
       :options
       (into []
             (map (fn [num]
                    {:text (str num)
                     :value num}))
             [4 8 12])
       :selection (:gap layout)})
     (ui/horizontal-layout
      (ui/label (name :gap))
      (let [src (get layout :gap)]
        (code-editor {:code src})))
     (for [kw [:gap
               :width
               :height]]
       (ui/horizontal-layout
        (ui/label (name kw))
        (let [code (get layout kw)
              src (get code :element/code)]
          (code-editor {:code src})))))))



(def ->tree** (memoize
               (fn [root]
                 (viscous/wrap
                  (tree-seq
                   sm/model-branch?
                   sm/model-children
                   root)))))


(defeffect ::config-on-click [m]
  (let [{:keys [$root element sym]} m
        eid (:element/id element)]
    (dispatch! :update
               $root
               (fn [root]
                 (let [zelem (sm/zelem-by-id root eid)]
                   (if-not zelem
                     root
                     (let [bindings (sm/collect-bindings zelem)]
                       (specter/transform
                          [(sm/elem-by-id (:element/id element))]
                          (fn [elem]
                            (assoc elem
                                   :element/on-click
                                   {:element/type ::sm/code
                                    :element/id (random-uuid)
                                    :element/code `(fn []
                                                     (~sym ~(into {}
                                                                  (mapcat (fn [sym]
                                                                            [[(keyword sym) sym]
                                                                             [(keyword (str "$" (name sym))) (symbol (str "$" (name sym)))]]))
                                                                  bindings)))}
                                   ))
                          root))))))))


;; This is a bit more fiddly than I would like
;; - membrane.component can only parse
;;   one level of nth or get for inline attributes
;; - this only supports get and nth
(defn ^:private path->attribute-code
  [path]
  (when (and (= 'find
                (-> path first first))
             (= '(val)
                (second path)))
    (let [sym (symbol (-> path first second name))]
      (if-let [path (->> path (drop 2) seq)]
        (let [cnt (count path)]
          (cond
            (and (= 2 cnt)
                 (= 'find
                    (-> path first first))
                 (= '(val)
                    (second path)))
            (let [k (-> path first second)]
              (if (keyword? k)
                `(~k ~sym)
                `(~'get ~sym (quote ~k))))

            (and (= 1 cnt)
                 (= 'nth (-> path first first)))
            `(~'nth ~sym ~(-> path first second))

            ;; else nil
            ))
        ;; else, just an attribute name
        sym))))

(defui editor [{:keys [elem root] :as m}]
  (ui/vertical-layout
   (ui/on
    ::config-on-click
    (fn [m]
      [[::config-on-click (assoc m :$root $root)]])
    (compile m))
   (let [inspector-extra (get extra ::inspector)]
     (ui/on
      ::dnd/drag-start
      (fn [m]
        (let [path (-> m ::dnd/obj :path)]
          (if-let [code (path->attribute-code path)]
            [[::dnd/drag-start
              (update m
                      ::dnd/obj
                      #(assoc % ::sm/component-attribute-code code))]]
            [[::dnd/drag-start m]])))
      (when (and (= ::sm/component
                    (:element/type root)))
        (when-let [defaults (:component/defaults root)]
          (viscous/inspector
           {:obj (viscous/wrap defaults)
            :width (get inspector-extra :width 40)
            :height (get inspector-extra :height 1)
            :show-context? (get inspector-extra :show-context?)
            :extra inspector-extra})))))
   #_(let [inspector-extra (get extra ::inspector2)]
       (viscous/inspector
        {:obj (->tree** root)
         :width (get inspector-extra :width 40)
         :height (get inspector-extra :height 1)
         :show-context? (get inspector-extra :show-context?)
         :extra inspector-extra}))))

(comment

  (def app-state (atom {:elem {:element/type ::sm/checkbox
                               :element/checked? {:element/type ::sm/code
                                                  :element/code true}}}))
  (skia/run
    (membrane.component/make-app #'editor
                                  app-state))

  ,)
