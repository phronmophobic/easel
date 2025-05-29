(ns com.phronemophobic.easel.clobber
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
   [com.phronemophobic.clobber.modes.clojure.ui :as cui]

   [com.phronemophobic.clobber.modes.clojure :as clojure-mode]
   [com.phronemophobic.clobber.modes.text :as text-mode]

   [com.phronemophobic.membrandt :as ant]))


(defui clobber-ui* [{:keys [this]}]
  (let [state (:state this)
        editor (:editor state)]
    (when editor
      (cui/code-editor
       {:editor editor
        :extra (:extra state)}))))

(defn clobber-ui [this $context context]
  (clobber-ui* {:this this
                :$this [(:$ref this)]
                :context context
                :$context $context}))


(defn load-editor [dispatch! $ref size]
  (let [
        eval-ns (the-ns 'user)
        ns-sym (ns-name eval-ns)

        height (nth size 1)
        editor (-> (cui/make-editor 'com.phronemophobic.clobber.modes.clojure.ui)
                   (cui/editor-set-height height)
                   (text-mode/editor-update-viewport))]
    (dispatch!
     :update
     $ref
     (fn [applet]
       (-> applet
           (assoc-in [:state :editor] editor)
           (assoc-in [:state :$editor] [$ref '(keypath :state) '(keypath :editor)])
           ))))
  )

(defrecord ClobberApplet [dispatch!]
  model/IApplet
  (-start [this $ref size _content-scale]
    (let [
          ]
      (assoc this
             :extra {}
             :tap-vals []
             :$ref $ref
             :size size
             ::model/queue
             [(fn []
                (load-editor dispatch! $ref size))])))
  (-stop [this]
    nil)
  model/IUI
  (-ui [this $context context]
    (clobber-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (let [height (nth size 1)]
      (-> this
          (assoc :size size)
          (update-in [:state :editor] cui/editor-set-height height)))))

(defn clobber-applet [handler]
  (-> (->ClobberApplet handler)
      (assoc :label (str "Clobber") )))



