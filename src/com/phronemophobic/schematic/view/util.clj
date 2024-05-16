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
                            [k `(get ~extra## [~k## ~k])
                             (keyword (str "$" (name k))) [$extra##
                                                           [(list
                                                             'list
                                                             '(quote keypath)
                                                             [k##
                                                              k])]]]))
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
     (let [body (ui/label "empty")
           body (if drag-object
                  (ui/fill-bordered
                   [1 0 0 0.2]
                   0
                   body)
                  body)]
       body)})))

(defui code-editor [{:keys [code editing? buf
                            ^:membrane.component/contextual
                            eval-ns] :as m}]
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
