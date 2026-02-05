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
   [com.phronemophobic.membrandt.impl.grid :as grid]
   [com.phronemophobic.membrandt :as ant]))

(defeffect ::tap-drop-object [drop-object]
  (when-let [x (:x drop-object)]
    (tap> @x)))


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


(defui drag-target [{:keys [drag-object body]}]
  (dnd/on-drop
   (fn [pos obj]
     (let [intents [[:set $drag-object nil]]]
       (conj intents [::tap-drop-object obj])))
   (on-drag-hover
    {:$body nil
     :object drag-object
     :body
     (let [body [body (ui/spacer 200 200)]
           body (if drag-object
                  (ui/fill-bordered
                   [1 0 0 0.2]
                   0
                   body)
                  body)]
       body)})))


(defui tap-view [{:keys [tap-vals size] :as this}]
  (let [[cw ch] size
        grid-extra (get extra ::grid)
        grid-scroll-state (get grid-extra ::scroll-state)
        inspector-extras (get grid-extra ::inspectors)]
    (drag-target
     {:$body nil
      :body 
      (ui/vertical-layout
       (ui/horizontal-layout
        (ant/button {:size :small
                     :text "X"
                     :on-click (fn []
                                 [[::clear-taps this]])})
        (ant/button {:size :small
                     :text "scroll top"
                     :on-click (fn []
                                 [[:set $grid-extra nil]])})
        (ant/button {:size :small
                     :text "scroll bottom"
                     :on-click (fn []
                                 [[:update $grid-scroll-state
                                   (fn [m]
                                     (prn m)
                                     (assoc m
                                            :row-index (dec (count tap-vals))
                                            :row-offset 0))]])}))
       (grid/list-view
        {:row-fn (fn [{:keys [row]}]
                   (let [inspector-extra (get inspector-extras row)]
                     (ui/padding 
                      2
                      (viscous/inspector {:obj (nth tap-vals row)
                                          :width (get inspector-extra :width 40)
                                          :height (get inspector-extra :height 1)
                                          :extra inspector-extra}))))
         :num-rows (count tap-vals)
         :width cw
         :height (- ch 80)
         :scroll-state grid-scroll-state
         :extra grid-extra}))})))

(defn tap-ui [this $context context]
  (let [state (-> this
                  (assoc :context context
                         :$context $context
                         :extra (:extra this)
                         :$extra [(:$ref this) '(keypath :extra)]))]
    (tap-view state)))


(defrecord TapWatcher [dispatch!]
  model/IApplet
  (-start [this {:keys [$ref size]}]
    (let [$tap-vals [$ref '(keypath :tap-vals)]
          tapf (fn [o]
                 (dispatch! :update $tap-vals conj (viscous/wrap o)))]
      (assoc this
             :extra {}
             :tap-vals []
             :tapf tapf
             :$tap-vals $tap-vals
             :$ref $ref
             :size size
             ::model/queue
             [(fn []
                (add-tap tapf))])))
  (-stop [this]
    (when-let [tapf (:tapf this)]
      (remove-tap tapf))
    nil)
  model/IUI
  (-ui [this {:keys [$context context]}]
    (tap-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn tap-watcher-applet [handler]
  (-> (->TapWatcher handler)
      (assoc :label (str "tap watcher") )))


(defn ^:private clear-taps [applet]
  (-> applet
      (assoc :tap-vals [])
      (update :extra (fn [extra]
                       (dissoc extra ::grid ::inspectors)))))

(defeffect ::clear-taps [{:keys [$ref]}]
  (if $ref
    (dispatch! :update $ref clear-taps)
    ;; else clear all taps
    (let [applets (dispatch! :com.phronemophobic.easel/get-applets)
          tap-watchers (->> applets
                            vals
                            (filter (fn [applet]
                                      (instance? TapWatcher applet))))]
      (doseq [tw tap-watchers]
        (when-let [$ref (:$ref tw)]
          (dispatch! :update $ref clear-taps))))))

