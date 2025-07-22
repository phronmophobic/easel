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

   [com.phronemophobic.membrandt.icon.ui :as icon.ui]
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


(defeffect ::ask [{:keys [$conversation prompt]}]
  (let [conversation (dispatch! :get $conversation)
        conversation (update conversation
                             :messages
                             conj {:role "user"
                                   :content prompt})
        _ (dispatch! :set $conversation conversation)
        ch (derpbot/ask conversation)]
    
    (async/go
      (loop []
        (when-let [chunk (async/<! ch)]
          (if (instance? Exception chunk)
            (prn chunk)
            ;; else
            (do (dispatch! :set $conversation chunk)
                (recur))))))))

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

(defeffect ::record-stop [{:keys [$editor]}]

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
        (dispatch! ::text-ui/update-editor
                   {:$editor $editor
                    :op
                    (fn [editor]
                      (-> editor
                          (text-mode/editor-clear)
                          (text-mode/editor-self-insert-command text)))})))))

(defui derpbot-ui* [{:keys [state size thread-id]}]
  (let [editor (:editor state)
        conversation (:conversation state)
        messages (get conversation :messages)
        
        [cw ch] size
        
        footer
        (ui/vertical-layout
         (ui/flex-layout
          [(ui/translate
            0 8
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
                            [::record-stop {:$editor $editor}]])
                         btn))]
              btn))
          
           (ui/translate 
            0 8
            (ant/button {:size :small
                         :text "ask"
                         :on-click
                         (fn []
                           [[::ask {:prompt (str (:rope editor))
                                    :$conversation $conversation}]
                            [::text-ui/update-editor {:$editor $editor
                                                      :op text-mode/editor-clear}]])}))
          
          
          (ui/padding
           8
           [(text-ui/text-editor {:editor editor})
            (ui/spacer 800 100)])]
          {:direction :row
           :gap 8}))
        
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
          (for [[i message] (map-indexed vector messages)]
            (if-let [content (:content message)]
              
              (if (= "tool" (:role message))
                (let [extra (get extra [::messages :tool-responses i])]
                  (viscous/inspector {:obj (viscous/wrap message)
                                      :width (get extra :width 40)
                                      :height (get extra :height 1)
                                      :show-context? false
                                      :extra extra}))
                
                ;; regular assistant response
                (let [base-style
                    #:text-style
                    {:font-size 18
                     :height 1.2
                     :height-override true}
                    editor (-> (md/make-editor)
                               (assoc :base-style base-style)
                               (text-mode/editor-self-insert-command content))
                    styled-text (try
                                  (md/editor->styled-text editor)
                                  (catch Exception e
                                    (tap> (str (:rope editor)))
                                    #_(prn e)
                                    "There was an error displaying the markdown"))]
                
                (ui/on
                 ::md/markdown-event
                 (fn [events]
                   (into []
                         (keep (fn [{:keys [type] :as event}]
                                 (prn event)
                                 (cond
                                   (= "uri_autolink" type)
                                   [:com.phronemophobic.easel/add-applet
                                    {:make-applet
                                     (fn [handler]
                                       ((requiring-resolve 'com.phronemophobic.easel.browser/browslet)
                                        handler
                                        (:text event)))}]
                                   
                                   (= "link" type)
                                   [:com.phronemophobic.easel/add-applet
                                    {:make-applet
                                     (fn [handler]
                                       ((requiring-resolve 'com.phronemophobic.easel.browser/browslet)
                                        handler
                                        (:destination event)))}])))
                         events)
                   )
                 
                 
                 (let [para (md/wrap-events
                             (para/paragraph styled-text
                                             text-width
                                             {:paragraph-style/text-style base-style}))
                       icon
                       (ui/on
                        :mouse-down
                        (fn [_]
                          [[:clipboard-copy (str (:rope editor))]])
                        (icon.ui/icon {:name "copy"
                                           :hover? (get extra [::hover i])}))
                       [iw ih] (ui/bounds icon )
                       pw (ui/width para)]
                   [(ui/translate 0 ih para)
                    (ui/translate (- pw iw) 0
                                  icon)]))))
              ;; else
              (when-let [tool-calls (:tool_calls message)]
                
                (let [extra (get extra [::messages :tool-calls i])]
                  (viscous/inspector {:obj (viscous/wrap tool-calls)
                                      :width (get extra :width 40)
                                      :height (get extra :height 1)
                                      :show-context? false
                                      :extra extra})))))))
        
        scroll-offset (get state :scroll-offset [0 0])]
    (ui/vertical-layout
     (tailing-scrollview
      {:scroll-bounds [(- cw 10)
                       (- (- ch
                             (ui/height footer))
                          10)]
       :offset scroll-offset
       :tail? (get extra ::tail? true)
       :$body nil
       :body (ui/vertical-layout
              responses
              (ui/spacer 0 30))})
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
          state {:editor (text-ui/make-editor)
                 :conversation
                 {:tools (into []
                               (vals @derpbot/tools))
                  :messages
                  [{:role "system"
                    :content "You are a helpful AI assitant. Be concise. If you don't know, say so."}]}}]
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

