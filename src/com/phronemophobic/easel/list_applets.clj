(ns com.phronemophobic.easel.list-applets
  (:require
   [com.phronemophobic.easel.model :as model]
   [membrane.basic-components :as basic]
   [membrane.component
    :refer [defui defeffect]]
   [membrane.ui :as ui]
))




(defui list-applets-ui [{:keys [applets url $ref]}]
  (ui/vertical-layout
   (basic/button {:text "Add Term"
                  ;; :hover? (get applet [::hover?])
                  :on-click
                  (fn []
                    [[:com.phronemophobic.easel/add-applet
                      (requiring-resolve 'com.phronemophobic.easel.term/termlet)]])})

   (ui/horizontal-layout
    (basic/button {:text "Add browser"
                   ;; :hover? (get applet [::hover?])
                   :on-click
                   (fn []
                     [[:com.phronemophobic.easel/add-applet
                       (fn [handler]
                         ((requiring-resolve 'com.phronemophobic.easel.browser/browslet)
                          handler
                          url))]])})
    (basic/textarea {:text url}))))



(defrecord ListApplets [dispatch!]
  model/IApplet
  (-start [this $ref]
    (assoc this
           :$ref $ref
           :$extra [$ref (list 'keypath :extra)]
           :$context [$ref (list 'keypath :context)]
           :url "https://github.com"
           :$url [$ref (list 'keypath :url)]
           :size [300 300]
           :$applets
           [$ref (list 'keypath :applets)]))
  (-stop [this])
  (-ui [this $context context]
    (ui/scissor-view
     [0 0]
     (:size this)
     (list-applets-ui this)))
  model/IResizable
  (-resize [this [w h] _content-scale]
    (assoc this
           :size [w h])))

(defn list-applets [handler]
  (-> (->ListApplets handler)
      (assoc :applets
             [{:name "term"
               :make-applet (requiring-resolve 'com.phronemophobic.easel.term/termlet)}
              {:name "browser"
               :make-applet (requiring-resolve 'com.phronemophobic.easel.browser/browslet)}])
      (assoc :label "list-applets")))
