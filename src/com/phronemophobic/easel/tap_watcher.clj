(ns com.phronemophobic.easel.tap-watcher
  (:require
   [membrane.component :refer
    [defui
     defeffect]]
   [membrane.ui :as ui]
   [membrane.alpha.component.drag-and-drop :as dnd]
   [com.phronemophobic.easel.model :as model]
   [membrane.basic-components :as basic]
   [clojure.core.async :as async]
   [clojure.set :as set]
   [com.phronemophobic.viscous :as viscous]

   [com.phronemophobic.membrandt :as ant]))


(defui tap-view [{:keys [tap-vals size]}]
  (basic/scrollview
   {:scroll-bounds size
    :$body nil
    :body
    (apply
     ui/vertical-layout
     (ant/button {:size :small
                  :text "X"
                  :on-click (fn []
                              [[:set $tap-vals []]])})
     (eduction
      (map-indexed
       (fn [i obj]
         (let [inspector-extra (get extra [::inspector [i obj]])]
           (ui/vertical-layout
            (viscous/inspector
             {:obj obj
              :width (get inspector-extra :width 40)
              :height (get inspector-extra :height 1)
              :show-context? (get inspector-extra :show-context?)
              :extra inspector-extra})))))
      tap-vals))}))

(defn tap-ui [this $context context]
  (let [state (-> this
                  (assoc :context context
                         :$context $context
                         :extra (:extra this)
                         :$extra [(:$ref this) '(keypath :extra)]))]
    (tap-view state)))


(defrecord TapWatcher [dispatch!]
  model/IApplet
  (-start [this $ref size _content-scale]
    (let [$tap-vals [$ref '(keypath :tap-vals)]
          tapf (fn [o]
                 (dispatch! :update $tap-vals conj (viscous/wrap o)))]
      (add-tap tapf)
      (assoc this
             :extra {}
             :tap-vals []
             :tapf tapf
             :$tap-vals $tap-vals
             :$ref $ref
             :size size)))
  (-stop [this]
    (when-let [tapf (:tapf this)]
      (remove-tap tapf))
    nil)
  model/IUI
  (-ui [this $context context]
    (tap-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn tap-watcher-applet [handler]
  (-> (->TapWatcher handler)
      (assoc :label (str "tap watcher") )))


