(ns com.phronemophobic.easel.video
  (:require
   [membrane.component :refer
    [defui
     defeffect]]
   [membrane.ui :as ui]
   [clojure.java.io :as io]
   [membrane.alpha.component.drag-and-drop :as dnd]
   [membrane.basic-components :as basic]
   [clojure.core.async :as async]
   [clojure.set :as set]
   [com.phronemophobic.clj-media.impl.skia :as video]
   [com.phronemophobic.easel.model :as model]
   [com.phronemophobic.membrandt :as ant]))

(defn video-ui [this $context context]
  (let [state (-> this
                  (assoc :context context
                         :$context $context
                         :extra (:extra this)
                         :$extra [(:$ref this) '(keypath :extra)]))]
    (def video-state state)
    (video/video-player state)))


(defrecord VideoWidget [dispatch! path]
  model/IApplet
  (-start [this $ref size _content-scale]
    (let [
          
          $video-state [$ref '(keypath :video-state)]
          this (assoc this
                      :extra {}
                      :$ref $ref
                      :size size
                      :$video-state $video-state
                      ::model/queue
                      [(fn []
                         (dispatch! ::video/play
                                    {:path path
                                     :$video-state $video-state}))]
                      :path path)
          ]

      this))
  (-stop [this]
    (dispatch! ::video/cleanup {:video-state (:video-state this)})

    nil)
  model/IUI
  (-ui [this $context context]
    (video-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn video-applet [handler title path]
  (-> (->VideoWidget handler path)
      (assoc :label title )))


