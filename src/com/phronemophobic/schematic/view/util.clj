(ns com.phronemophobic.schematic.view.util
  (:require [clojure.string :as str]
            [membrane.component :refer [defui defeffect]]
            [membrane.basic-components :as basic]
            [membrane.ui :as ui]
            [com.phronemophobic.schematic.model :as sm]
            [membrane.alpha.component.drag-and-drop :as dnd]
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
