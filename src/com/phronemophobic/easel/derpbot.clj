(ns com.phronemophobic.easel.derpbot
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
   [com.phronemophobic.viscous :as viscous]

   [com.phronemophobic.membrandt :as ant]
   [com.phronemophobic.derpbot :as derpbot]
   [com.phronemophobic.derpbot.tools.audio :as derpbot.audio]
   [com.phronemophobic.clj-media.impl.audio :as audio]
   [com.phronemophobic.clj-media :as clj-media]
   [com.phronemophobic.clobber.modes.text.ui :as text-ui]
   [com.phronemophobic.clobber.modes.text :as text-mode]
   [com.phronemophobic.clobber.modes.markdown.wysiwyg :as md]))

(defui tailing-scrollview [{:keys [body scroll-bounds tail?]
                            :or {tail? true}}]
  (let [[w h] scroll-bounds
        offset (get extra :offset [0 0])
        $offset $offset
        [bw bh] (ui/bounds body)
        [ox oy] offset
        offset (if tail?
                 [ox (max 0
                          (- bh h))]
                 offset)
        cb (basic/checkbox {:checked? tail?})
        [cb-width cb-height] (ui/bounds cb)]
    (ui/wrap-on
     :scroll
     (fn [handler offset pos]
       (let [intents (handler offset pos)]
         (when (seq intents)
           (cons [:set $tail? false]
                 intents))))
     (ui/on
      :membrane.component/start-scroll
      (fn [f]
        [[:membrane.component/start-scroll f]
         [:set $tail? false]])
      [(basic/scrollview
        {:scroll-bounds scroll-bounds
         :offset offset
         :$offset $offset
         :$body nil
         :body body})
       (ui/translate
        (- w cb-width 8)
        (- h cb-height)
        cb)]))))


(defeffect ::ask [{:keys [thread-id prompt  messages $messages]}]
  (dispatch! :update $messages conj prompt)

  (dispatch! :update $messages conj "...")

  (let [n (dec (count (dispatch! :get $messages)))
        ch (async/chan 5)]
    (derpbot/respond {:ch ch
                      :thread/id thread-id
                      :prompt prompt} )
    (async/go
      (loop []
        (when-let [chunk (async/<! ch)]
          ;; (tap> chunk)
          (cond
            (instance? Exception chunk)
            (dispatch! :update $messages #(assoc % n (str chunk)))

            (:tool_calls chunk)
            (let [tool-calls
                  (str "tool calls: "
                       (str/join
                        ", "
                        (eduction
                         (map :function)
                         (map :name)
                         (:tool_calls chunk))))]
              (dispatch! :update $messages #(assoc % n tool-calls)))

            (:content chunk)
            (dispatch! :update $messages #(assoc % n (:content chunk)))
            

            :else
            nil)
          (recur)))))

  ;; (derpbot/respond )
  
  )

(defeffect ::debug [{:keys [thread-id prompt $prompt messages $messages]}]

  (clojure.pprint/pprint messages)

  ;; (derpbot/respond )
  
  )

(defonce recording (atom nil))
(defeffect ::record-start [{}]
  (let [x (swap! recording
               (fn [old]
                 (if old
                   old
                   (delay
                     (audio/record-audio)))))]
    @x))

(defeffect ::record-stop [{:keys [$prompt]}]

  (future
    (let [bs (@@recording)
          f (io/file "/var/tmp/transcribe.mp3")]
      (reset! recording nil)
      (clj-media/write!
       (let [format (clj-media/audio-format
                     {:channel-layout "mono"
                      :sample-rate 44100
                      :sample-format :sample-format/s16})]
         (clj-media/make-media
          format
          [(clj-media/make-frame
            {:format format
             :bytes bs
             :time-base 44100
             :pts 0})]))
       (.getCanonicalPath f))
      (let [text (derpbot.audio/transcribe-file f)]
        (dispatch! :set $prompt text)))))

(defui derpbot-ui* [{:keys [state size thread-id]}]
  (let [editor (:editor state)
        messages (get state :messages [])
        [cw ch] size
        
        footer
        (ui/vertical-layout
         (ui/horizontal-layout
          #_(let [record-hover (get state :record-hover)
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
                            [::record-stop {:$prompt $prompt}]])
                         btn))]
              btn)
          
          (ui/padding
           8 0 0 8
           (ant/button {:size :small
                        :text "ask"
                        :on-click
                        (fn []
                          [[::ask {:thread-id thread-id
                                   :prompt (str (:rope editor))
                                   :messages messages
                                   :$messages $messages}]
                           [::text-ui/update-editor {:$editor $editor
                                                     :op text-mode/editor-clear}]])}))
          
          (ui/padding
           8
           [(text-ui/text-editor {:editor editor})
            (ui/spacer 800 100)])
          #_(ant/text-input {:size :small
                             :text (get state :prompt "")})))
        
        margin 10
        max-width (- cw 10)
        text-width (min 600
                        (- max-width
                           (* 2 margin)))
        
        responses
        (ui/translate
         (quot (- max-width
                  text-width)
               2)
         margin
         (apply
          ui/vertical-layout
          (for [message messages]
            
            (let [base-style
                  #:text-style
                  {:font-size 18
                  :height 1.2
                  :height-override true}
                  editor (-> (md/make-editor)
                             (assoc :base-style base-style)
                             (text-mode/editor-self-insert-command message))
                  styled-text (md/editor->styled-text editor)]
              (para/paragraph styled-text
                              text-width
                              {:paragraph-style/text-style base-style})))))
        
        scroll-offset (get state :scroll-offset [0 0])]
    (ui/vertical-layout
     (tailing-scrollview
      {:scroll-bounds [(- cw 10)
                       (- (- ch
                             (ui/height footer))
                          10)]
       :offset scroll-offset
       :$body nil
       :body responses})
     (ui/translate 
      (max 0 (quot (- cw (ui/width footer)) 2))
      0
      footer))))

(defn derpbot-ui [this $context context]
  (derpbot-ui*
   (assoc this
          :context context
          :$context $context)))

(defrecord Derpbotlet [dispatch! thread-id]
  model/IApplet
  (-start [this $ref size _content-scale]
    (let [
          ;; interns (ns->interns ns)
          ;; $interns [$ref '(keypath :interns)
          state {:editor (text-ui/make-editor)}]
      (assoc this
             :state state
             :$state [$ref '(keypath :state)]
             :extra {}
             :$extra [$ref '(keypath :extra)]
             :$ref $ref
             :size size)))
  (-stop [this]
    nil)
  model/IUI
  (-ui [this $context context]
    (derpbot-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn derpbot-applet [handler thread-id]
  (-> (->Derpbotlet handler thread-id)
      (assoc :label (str "derpbot"))))


;; get current buffer
;; emacsclient --eval '(buffer-name (window-buffer (selected-window)))'

;; insert text
;; (with-current-buffer \"your-buffer-name\" (insert \"Your text here\"))

;; echo "Your text here" | emacsclient --eval '(with-current-buffer (buffer-name (window-buffer (selected-window))) (insert (current-kill 0)))'

;; hello



