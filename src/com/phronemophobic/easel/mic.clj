(ns com.phronemophobic.easel.mic
  (:require
   [membrane.component :refer
    [defui
     defeffect]]
   [membrane.ui :as ui]
   [membrane.basic-components :as basic]
   [membrane.skia.paragraph :as para]
   [membrane.alpha.component.drag-and-drop :as dnd]
   [com.phronemophobic.easel.model :as model]
   [clojure.core.async :as async]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [com.phronemophobic.membrandt :as ant]
   [com.phronemophobic.whisper :as whisper]
   ))




(defeffect ::debug [{}]
  
  ,)

(defonce recording (atom nil))
(defeffect ::record-start [{}]
  (let [x (swap! recording
               (fn [old]
                 (if old
                   old
                   (delay
                     (whisper/record-and-transcribe)))))]
    @x))

(defeffect ::record-stop [{:keys [$messages]}]

  (let [text (@@recording)]
    (reset! recording nil)
    (dispatch! :update $messages conj text)))

(defui mic-ui* [{:keys [state size thread-id]}]
  (let [messages (get state :messages [])

        [cw ch] size]
    (ui/vertical-layout
     (ant/button {:size :small
                  :text "debug"
                  :on-click
                  (fn []
                    [[::debug {}]])})
     (ui/horizontal-layout
      (let [record-hover (get state :record-hover)
            not-record-hover (get state :not-record-hover)
            btn (ant/button {:size :small
                             :hover? record-hover
                             :$hover? $not-record-hover
                             :text "record"})
            btn (if (not record-hover)
                  (ui/on
                   :mouse-down
                   (fn [_]
                     [[:set $record-hover true]
                      [::record-start {}]])
                   btn)
                  (ui/on
                   :mouse-up
                   (fn [_]
                     [[:set $record-hover false]
                      [::record-stop {:$messages $messages}]])
                   btn))]
        btn))
     (apply
      ui/vertical-layout
      (for [message messages]
        (para/paragraph message
                        cw)))
     )))

(defn mic-ui [this $context context]
  (mic-ui*
   (assoc this
          :context context
          :$context $context)))

(defrecord Miclet [dispatch! thread-id]
  model/IApplet
  (-start [this $ref size _content-scale]
    (assoc this
           :state {}
           :$state [$ref '(keypath :state)]
           :extra {}
           :$extra [$ref '(keypath :extra)]
           :$ref $ref
           :size size))
  (-stop [this]
    nil)
  model/IUI
  (-ui [this $context context]
    (mic-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn mic-applet [handler thread-id]
  (-> (->Miclet handler thread-id)
      (assoc :label (str "mic"))))

