(ns com.phronemophobic.schematic.view.util
  (:require [clojure.string :as str]
            [membrane.component :refer [defui defeffect]]
            [membrane.basic-components :as basic]
            [membrane.ui :as ui]
            [com.phronemophobic.schematic.model :as sm]
            [membrane.alpha.component.drag-and-drop :as dnd]
            [liq.buffer :as buffer]
            [com.phronemophobic.viscous :as viscous]
            [membrane.components.code-editor.code-editor :as code-editor]
            [clojure.set :as set]))

(defn uicall* [component m $extra## extra## $context## context## ]
  (let [state-key (into []
                        (keep (fn [[k v]]
                                (when (str/starts-with? (name k)
                                                        "$")
                                  v)))
                        m)
        component-var (resolve component)
        component-meta (meta component-var)
        arglists (:arglists component-meta)
        first-arglist (first arglists)
        arg-map (first first-arglist)
        args (:keys arg-map)
        implicit-keys (set/difference (into #{}
                                            (map keyword)
                                            args)
                                      (set (keys m))
                                      #{:extra :$extra
                                        :context :$context})

        contextual? (into #{}
                          (comp (filter (fn [sym]
                                          (:membrane.component/contextual
                                           (meta sym))))
                                (map keyword))
                          args)

        k## (gensym "k")]
    `(let [~k## ~state-key
           ~extra## ~extra##
           extra# (get ~extra## ~k##)
           $extra# [~$extra## (list (quote ~'keypath) ~k##)]
           context# ~context##
           $context# ~$context##]
       (~component
        (assoc ~m
               ~@(eduction
                  (mapcat (fn [k]
                            (if (contextual? k)
                              [k `(get ~context## ~k)
                               (keyword (str "$" (name k))) [$context##
                                                             [(list
                                                               'list
                                                               '(quote keypath)
                                                               k)]]]
                              [k `(get ~extra## [~k## ~k])
                               (keyword (str "$" (name k))) [$extra##
                                                             [(list
                                                               'list
                                                               '(quote keypath)
                                                               [k##
                                                                k])]]])))
                  implicit-keys)
               :extra extra#
               :$extra $extra#
               :context context#
               :$context $context#)))))

(defmacro uicall
  ([component m]
   (uicall* component m '$extra 'extra '$context 'context)))


(defui on-drag-hover
  "Component for adding a hover? state."
  [{:keys [object body]}]
  (if object
    (ui/wrap-on
     :mouse-move-global
     (fn [handler [x y :as pos]]
       (let [[w h] (ui/bounds body)
             child-intents (handler pos)]
         (if (or (neg? x)
                 (> x w)
                 (neg? y)
                 (> y h))
           (conj child-intents
                 [:set $object nil])
           child-intents)))
     body)
    (dnd/on-drop-move
     (fn [_ o]
       [[:set $object o]])
     body)))

(defui drag-elem-target [{:keys [elem drag-object]}]
  (dnd/on-drop
   (fn [pos obj]
     (let [intents [[:set $drag-object nil]]]
       (if-let [new-elem (::sm/element obj)]
         (conj intents [:set $elem new-elem])
         intents)))
   (on-drag-hover
    {:$body nil
     :object drag-object
     :body
     (let [drop-object (:drop-object context)
           w 100
           h 8
           body
           (cond
             drag-object (ui/filled-rectangle
                          [1 0 0 0.2]
                          w h)

             (and drop-object
                  (::sm/element drop-object))
             (ui/filled-rectangle
              [0.88 0.88 0.88 1]
              w h)

             :else
             (ui/spacer w h))]
       body)})))

(defui code-editor [{:keys [code editing? buf
                            ^:membrane.component/contextual
                            eval-ns] :as m}]
  (if (not editing?)
    (let [inspector-extra (get extra ::inspector-extra)
          body (viscous/inspector {:obj (viscous/wrap code)
                                   :width (get inspector-extra :width 40)
                                   :height (get inspector-extra :height 1)
                                   :show-context? (get inspector-extra :show-context?)
                                   :extra inspector-extra})
          drag-object (:drag-object extra)
          body (cond

                 drag-object
                 (ui/fill-bordered [1 0 0 0.2]
                                   0
                                   body)

                 (:drop-object context)
                 (ui/fill-bordered [0.88 0.88 0.88 1]
                                   0
                                   body)

                 :else
                 body)]
      (ui/horizontal-layout
       (basic/button {:text "O"
                      :on-click
                      (fn []
                        [[:set $editing? true]
                         [:set $buf (buffer/buffer (with-out-str
                                                     (clojure.pprint/pprint code))
                                                   {:mode :insert})]])})
       (dnd/on-drop
        (fn [pos obj]
          (let [intents [[:set $drag-object nil]]
                x (:x obj)
                ]
            (if-let [v (when (and x
                                  (instance? clojure.lang.IDeref x))
                         @x)]
              (conj intents [:set $code v])
              intents)))
        (on-drag-hover
         {:body body
          :$body nil
          :object drag-object}))))
    (ui/horizontal-layout
     (basic/button {:text "O"
                    :on-click
                    (fn []
                      [[:set $editing? false]
                       [:update $code
                        (fn [old-code]
                          (try
                            (if eval-ns
                              (binding [*ns* eval-ns]
                                (read-string (buffer/text buf)))
                              (read-string (buffer/text buf)))
                            (catch Exception e
                              old-code)))]])})
     (basic/button {:text "X"
                    :on-click
                    (fn []
                      [[:set $editing? false]])})
     (code-editor/text-editor {:buf buf}))))

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
