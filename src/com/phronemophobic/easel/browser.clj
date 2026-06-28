(ns com.phronemophobic.easel.browser
  (:require
   [com.phronemophobic.easel.model :as model]
   [membrane.skia :as skia]
   [membrane.ui :as ui]
   [membrane.component :refer [defeffect defui]]
   [clojure.java.io :as io]
   [com.phronemophobic.gen3 :as gen3]
   [com.phronemophobic.cef :as cef]
   [com.phronemophobic.cef.browser :as b]
   #_[com.phronemophobic.replog :as replog])
  (:import com.sun.jna.Pointer
           com.sun.jna.Function
           com.phronemophobic.membrane.Skia
           com.phronemophobic.gen3.structs.cef_browser_host_t
           ;;com.phronemophobic.cljcef.CefBrowser
           ))

;; (replog/load-log (into []
;;                        (filter #(= (ns-name *ns*)
;;                                    (::replog/ns %)))
;;                        (replog/get-main-log)))


(def ^:private EVENTFLAG_NONE  0),
(def ^:private EVENTFLAG_CAPS_LOCK_ON (bit-shift-left 1 0))
(def ^:private EVENTFLAG_SHIFT_DOWN (bit-shift-left 1 1))
(def ^:private EVENTFLAG_CONTROL_DOWN (bit-shift-left 1 2))
(def ^:private EVENTFLAG_ALT_DOWN (bit-shift-left 1 3))
(def ^:private EVENTFLAG_LEFT_MOUSE_BUTTON (bit-shift-left 1 4))
(def ^:private EVENTFLAG_MIDDLE_MOUSE_BUTTON (bit-shift-left 1 5))
(def ^:private EVENTFLAG_RIGHT_MOUSE_BUTTON (bit-shift-left 1 6))
;; /// Mac OS-X command key.
(def ^:private EVENTFLAG_COMMAND_DOWN (bit-shift-left 1 7))
(def ^:private EVENTFLAG_NUM_LOCK_ON (bit-shift-left 1 8))
(def ^:private EVENTFLAG_IS_KEY_PAD (bit-shift-left 1 9))
(def ^:private EVENTFLAG_IS_LEFT (bit-shift-left 1 10))
(def ^:private EVENTFLAG_IS_RIGHT (bit-shift-left 1 11))
(def ^:private EVENTFLAG_ALTGR_DOWN (bit-shift-left 1 12))
(def ^:private EVENTFLAG_IS_REPEAT (bit-shift-left 1 13))

(defn ^:private glfw-mods->cef-mods [mods repeat?]
  (let [alt? (not (zero? (bit-and ui/ALT-MASK mods)))
        super? (not (zero? (bit-and ui/SUPER-MASK mods)))
        shift? (not (zero? (bit-and ui/SHIFT-MASK mods)))
        ctrl? (not (zero? (bit-and ui/CONTROL-MASK mods)))
        caps-lock? (not (zero? (bit-and ui/CAPS-LOCK-MASK mods)))]
    (cond-> 0
      alt? (bit-or EVENTFLAG_ALT_DOWN)
      super? (bit-or EVENTFLAG_COMMAND_DOWN)
      shift? (bit-or EVENTFLAG_SHIFT_DOWN)
      ctrl? (bit-or EVENTFLAG_CONTROL_DOWN)
      caps-lock? (bit-or EVENTFLAG_CAPS_LOCK_ON)
      repeat? (bit-or EVENTFLAG_IS_REPEAT))))


;;;;;;;;;;;;;;;;;;
;; AUTO GENERATED
;;;;;;;;;;;;;;;;;;
(membrane.component/defui
  browser-bar
  [{:keys [width url]}]
  (membrane.ui/flex-layout
   [(com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/browser-back]]),
      :text "<"})
    (com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/browser-forward]]),
      :text ">"})
    (com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/browser-refresh]]),
      :text "O"})
    (com.phronemophobic.membrandt/text-input
     {:flex.grow/width 1.0, :text url})]
   {:gap 8, :width width, :height nil}))
(membrane.component/defui
  browser-bar
  [{:keys [width url]}]
  (membrane.ui/flex-layout
   [(com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/browser-back]]),
      :text "<"})
    (com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/browser-forward]]),
      :text ">"})
    (com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/browser-refresh]]),
      :text "O"})
    (com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/load-url url]]),
      :text "go"})
    (com.phronemophobic.membrandt/text-input
     {:flex.grow/width 1.0, :text url})]
   {:gap 8, :width width, :height nil}))
(membrane.component/defui
  browser-bar
  [{:keys [width url]}]
  (membrane.ui/flex-layout
   [(com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/browser-back]]),
      :text "<"})
    (com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/browser-forward]]),
      :text ">"})
    (com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/browser-refresh]]),
      :text "O"})
    (com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/load-url url]]),
      :text "go"})
    (com.phronemophobic.membrandt/text-input
     {:flex.grow/width 1.0, :text url})]
   {:gap 0, :width width, :height nil}))
(membrane.component/defui
  browser-bar
  [{:keys [width url]}]
  (membrane.ui/flex-layout
   [(com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/browser-back]]),
      :text "<"})
    (com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/browser-forward]]),
      :text ">"})
    (com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/browser-refresh]]),
      :text "O"})
    (com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/load-url url]]),
      :text "go"})
    (com.phronemophobic.membrandt/text-input
     {:flex.grow/width 1.0, :text url})]
   {:gap 8, :width width, :height nil}))
(membrane.component/defui
  browser-bar
  [{:keys [width url]}]
  (membrane.ui/flex-layout
   [(com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/browser-back]]),
      :text "<"})
    (com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/browser-forward]]),
      :text ">"})
    (com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/browser-refresh]]),
      :text "O"})
    (com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/load-url url]]),
      :text "go"})
    (com.phronemophobic.membrandt/text-input
     {:flex.grow/width 1.0, :text url})]
   {:gap 0, :width width, :height nil}))
(membrane.component/defui
  browser-bar
  [{:keys [width url]}]
  (membrane.ui/flex-layout
   [(com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/browser-back]]),
      :text "<"})
    (com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/browser-forward]]),
      :text ">"})
    (com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/browser-refresh]]),
      :text "O"})
    (com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/show-inspector]]),
      :text "?"})
    (com.phronemophobic.membrandt/button
     {:on-click
      (fn [] [[:com.phronemophobic.easel.browser/load-url url]]),
      :text "go"})
    (com.phronemophobic.membrandt/text-input
     {:flex.grow/width 1.0, :text url})]
   {:gap 0, :width width, :height nil}))
;;;;;;;;;;;;;;;;;;
;; END AUTO GENERATED
;;;;;;;;;;;;;;;;;;


(defeffect ::more! [$num]
  (dispatch! :update $num inc))

(defeffect ::browser-back [{:keys [browser]}]
  (gen3/call browser :go_back))

(defeffect ::browser-refresh [{:keys [browser]}]
  (gen3/call browser :reload_ignore_cache))

(defeffect ::show-inspector []
  (dispatch!
   :com.phronemophobic.easel/add-applet
   {:make-applet
    (fn [handler]
      ((requiring-resolve 'com.phronemophobic.easel.browser/browslet)
       handler
       "http://localhost:8888/"))}))

(defeffect ::browser-forward [{:keys [browser]}]
  (gen3/call browser :go_forward))

(defeffect ::load-url [{:keys [browser url]}]
  (let [frame (gen3/call browser :get_main_frame)]
    (gen3/call frame :load_url url)))


(def skialib @#'skia/membraneskialib)

(skia/defc skia_bgra8888_draw skialib Void/TYPE [skia-resource buffer width height row-bytes])
(defn skia-bgra8888-draw [resource buffer width height row-bytes]
  (skia_bgra8888_draw resource buffer (int width) (int height) (int row-bytes)))

(skia/defc skia_direct_bgra8888_buffer skialib Pointer [buf width height row-bytes])
(defn skia-direct-bgra8888-buffer [buf width height row-bytes]
  (skia_direct_bgra8888_buffer buf (int width) (int height) (int row-bytes)))

(skia/defc skia_cleanup skialib Void/TYPE [skia-resource])

(skia/defc skia_draw_surface skialib Void/TYPE [destination source])


(skia/defc skia_browser_buffer skialib Pointer [width height])
(defn skia-browser-buffer [width height]
  (skia_browser_buffer (int width) (int height)))

(skia/defc skia_browser_update skialib Void/TYPE [skia-resource dirty-rects-count dirty-rects buffer width height])
(defn skia-browser-update [resource dirty-rects-count dirty-rects buffer width height]
  (skia_browser_update resource (int dirty-rects-count) dirty-rects buffer (int width) (int height)))

(skia/defc skia_browser_draw skialib Void/TYPE [skia-resource buffer width height])
(defn skia-browser-draw [resource buffer width height]
  (skia_browser_draw resource buffer (int width) (int height)))


(defn skia-draw [dispatch! $browser-info content-scale paint-type nrects rects buffer width height]
  (when (zero? paint-type)
    (let [browser-info (dispatch! :get $browser-info)]
      (when-let [draw-lock (:draw-lock browser-info)]
        (locking draw-lock
          (if (:resource browser-info)
            (if (and (= width (:width browser-info))
                     (= height (:height browser-info))
                     (= content-scale (:content-scale browser-info)))
              (when (pos? (.intValue nrects))
                (skia-browser-update (:resource browser-info) (.intValue nrects) rects buffer width height)
                (dispatch! :update $browser-info update :browser-id (fnil inc 0)))
              (do
                (dispatch! :update
                           $browser-info
                           dissoc
                           :resource)
                (skia_cleanup (:resource browser-info))
                (skia-draw dispatch! $browser-info content-scale paint-type nrects rects buffer width height)))
            (let [resource (skia-browser-buffer width height)
                  browser-info {:resource resource
                                :content-scale content-scale
                                :width width
                                :height height}]
              (skia-browser-draw resource buffer width height)
              (dispatch! :update $browser-info update :browser-id (fnil inc 0))

              (dispatch! :update $browser-info merge browser-info)
              (dispatch! :repaint!)))))))
  ;; always return nil. don't leak cache
  nil)

(defrecord Browser [browser browser-id focused? content-scale width height resource draw-lock mods $mods]
  ui/IOrigin
  (-origin [_]
    [0 0])


  ui/IMouseMove
  (-mouse-move [elem pos]
    (when browser
      (gen3/call (gen3/call browser :get_host)
                 :send_mouse_move_event
                 (gen3/map->mouse-event
                  {:x (first pos)
                   :y (second pos)
                   :modifiers gen3/EVENTFLAG_LEFT_MOUSE_BUTTON})
                 0)

      #_(.sendMouseMoveEvent (.getHost ^CefBrowser browser)
                           (cef/map->mouse-event
                            {:x (first pos)
                             :y (second pos)})
                           0)))
  
  ui/IMouseEvent
  (-mouse-event [elem pos button mouse-down? mods]
    (when browser
      (gen3/call (gen3/call browser :get_host)
                 :send_mouse_click_event
                 (gen3/map->mouse-event
                  {:x (first pos)
                   :y (second pos)
                   :modifiers gen3/EVENTFLAG_LEFT_MOUSE_BUTTON})
                 button
                 (if mouse-down?
                   0
                   1)
                 1)
      #_(.sendMouseClickEvent (.getHost ^CefBrowser browser)
                            (cef/map->mouse-event
                             {:x (first pos)
                              :y (second pos)})
                            button
                            (if mouse-down?
                              0
                              1)
                            1)
      ))

  ui/IScroll
  (-scroll [elem delta mpos]
    (when browser
      (gen3/call (gen3/call browser :get_host)
                 :send_mouse_wheel_event
                 (gen3/map->mouse-event
                  {:x (first mpos)
                   :y (second mpos)})
                 (first delta)
                 (second delta))
      #_(.sendMouseWheelEvent (.getHost ^CefBrowser browser)
                            (cef/map->mouse-event
                             {:x (first mpos)
                              :y (second mpos)})
                            (first delta)
                            (second delta))
      ))

  ui/IHasKeyPress
  (has-key-press [this]
    focused?)
  ui/IKeyPress
  (-key-press [elem k]
    (when (and browser focused?)
      (let [c (if (keyword? k)
                (if (= k :enter)
                  \return
                  nil
                  ;;(char (get skia/keycodes k))
                  )
                (.charAt k 0))]
        (when c
          (let [key-event {:type 3
                           :modifiers (glfw-mods->cef-mods mods false)
                           :character c
                           :unmodified-character c}]
            [[::send-key-event {:browser browser
                                :key-event key-event}]])
          #_(gen3/call (gen3/call browser :get_host)
                     :send_key_event
                     (gen3/map->key-event
                      ))
          #_(.sendKeyEvent (.getHost browser)
                         (cef/map->key-event
                          {:type 3
                           :modifiers 0
                           :character c
                           :unmodified-character c}))))))

  ui/IHasKeyEvent
  (has-key-event [this]
    focused?)
  ui/IKeyEvent
  (-key-event [elem key code action mods]
    (when (and focused? browser)
      (when (#{:press :release :repeat}
             action)
        (let [key-event {:type (case action
                                 :press 1
                                 :release 2
                                 :repeat 1
                                 )
                         :modifiers (glfw-mods->cef-mods mods (= action :repeat))
                         :native-key-code code
                         :character (char key)
                         :unmodified-character (char key)}]
          [[::send-key-event {:browser browser
                              :key-event key-event}]
           [:set $mods mods]]
          #_(gen3/call (gen3/call browser :get_host)
                     :send_key_event
                     (gen3/map->key-event key-event))
          #_(.sendKeyEvent (.getHost ^CefBrowser browser)
                         (cef/map->key-event
                          key-event)))))
    )
  

  ui/IBounds
  (-bounds [this]
    (if content-scale
      [(/ width content-scale) (/ height content-scale)]
      [width height]))

  skia/IDraw
  (draw [this]
    (when draw-lock
      (locking draw-lock
        (when resource
          (skia/save-canvas
           (when (not= 1 content-scale)
             (let [scale (float (/ 1 content-scale))]
               (Skia/skia_set_scale skia/*skia-resource* scale scale)))
           (skia_draw_surface skia/*skia-resource* resource)))))))

(defn browser-ui [this $context context]
  (let [focus (:focus context)
        focus? (= focus (:id this))
        browser-info (:browser-info this)
        ui-state (:ui-state this)

        extra (:extra ui-state)
        $extra (:$extra ui-state)
        mods (:mods extra 0)
        $mods [$extra '(keypath :mods)]

        view
        (assoc
         (->Browser (:browser browser-info)
                    (:browser-id browser-info)
                    focus?
                    (:content-scale browser-info)
                    (:width browser-info)
                    (:height browser-info)
                    (:resource browser-info)
                    (:draw-lock browser-info)
                    mods $mods)
         :id2 (:id this))

        view (if focus?
               (ui/wrap-on
                :mouse-down
                (fn [handler mpos]
                  (cons
                   [:set [$context (list 'keypath :focus)] (:id this)]
                   (handler mpos)))
                view)
               (ui/on
                :mouse-down
                (fn [_]
                  [[:set [$context (list 'keypath :focus)]
                    (:id this)]])
                (ui/no-events view)))]
    (ui/vertical-layout
     (ui/on
      ::browser-forward
      (fn []
        [[::browser-forward {:browser (:browser browser-info)}]])
      ::browser-refresh
      (fn []
        [[::browser-refresh {:browser (:browser browser-info)}]])
      ::load-url
      (fn [url]
        [[::load-url {:browser (:browser browser-info)
                      :url url}]])
      ::browser-back
      (fn []
        [[::browser-back {:browser (:browser browser-info)}]])
      (browser-bar
       (assoc ui-state
              :width (:width browser-info)
              :context context
              :$context $context)))
     view)))



(defrecord Browslet [dispatch! initial-url]
  model/IApplet
  (-start [this {:keys [$ref size content-scale]}]
    (let [[initial-content-sx initial-content-sy] content-scale
          [initial-width initial-height] size
          dispatch-main
          (fn [work]
            (dispatch! :dispatch-main work))

          $url [$ref '(keypath :ui-state) '(keypath :url)]
          ui-state {:url initial-url
                    :$url $url
                    :extra {}
                    :$extra [$ref '(keypath :ui-state) '(keypath :extra)]}
          $browser-info [$ref
                         '(keypath :browser-info)]

          cef-path (doto (io/file
                          "/Users/adrian/workspace/easel"
                          ".cef")
                     (.mkdirs))
          cache-path (doto (io/file
                            "/Users/adrian/workspace/easel"
                            ".browser-cache")
                       (.mkdirs))

          ;; If content scale isn't equal for x and y, just use 1.
          initial-content-scale (if (= initial-content-sx initial-content-sy)
                                  initial-content-sx
                                  1)]
      (assoc this
             :ui-state ui-state
             ::model/queue
             [(fn []
                (future
                  (b/create-browser [initial-width initial-height initial-content-scale]
                                    initial-url
                                    dispatch-main
                                    {:on-after-created
                                     (fn [browser]
                                       (dispatch! :update $browser-info
                                                  assoc :browser browser)
                                       
                                       (let [host (gen3/call browser :get_host)]
                                         (gen3/call host :set_focus (int 1))
                                         
                                         #_(.setFocus host 1)))
                                     :remote-debugging-port 8888

                                     
                                     #_#_:on-before-close
                                     (fn [browser]
                                       (dispatch! :update $browser-info
                                                  dissoc :browser))
                                     :life-span-handler/on-before-popup
                                     (fn [{:keys [target-url]}]
                                       (dispatch! :com.phronemophobic.easel/add-applet
                                                  {:make-applet
                                                   (fn [handler]
                                                     ((requiring-resolve 'com.phronemophobic.easel.browser/browslet)
                                                      handler
                                                      target-url))}))
                                     :load-handler/on-load-start
                                     (fn [browser frame transition-type]
                                       (when (= 1 (gen3/call frame :is_main))
                                         (-> browser
                                             (gen3/call :get_host)
                                             (gen3/call :set_focus (int 1)))
                                         (let [;; todo: needs memory management
                                               ;; The resulting string must be freed by calling cef_string_userfree_free().
                                               url (gen3/call frame :get_url)]
                                           (dispatch! :set $url url))))
                                     :cef-path cef-path
                                     :cache-path cache-path
                                     :on-paint+content-scale
                                     (fn [browser content-scale  paint-type nrects rects buffer width height]
                                       (skia-draw dispatch! $browser-info content-scale paint-type nrects rects buffer width height)
                                       (dispatch! :repaint!))})))]
             :browser-info
             {:draw-lock (Object.)
              :width initial-width
              :height initial-height})))

  (-stop [this]
    (when-let [browser (-> this :browser-info :browser)]
      (b/close browser))
    (update this :browser-info dissoc :browser))
  model/IUI
  (-ui [this {:keys [$context context]}]
    (browser-ui this $context context))
  model/IResizable
  (-resize [this size content-scale]
    (when-let [browser (-> this :browser-info :browser)]
      (let [[sx sy] content-scale]
        (if (= sx sy)
          (b/resize browser size sx)
          (b/resize browser size 1))))
    this))


(defn browslet [handler url]
  (-> (->Browslet handler url)
      (assoc :label "web")))

(comment
  (cef/download-and-extract-framework
   (doto (io/file ".cef")
     (.mkdirs))
   )
  (cef/download-and-prepare-environment!
   (doto (io/file ".cef")
     (.mkdirs)))

  (-> (b/list-browsers)
      first
      (gen3/call :get_host)
      (gen3/call :set_focus (int 1)))
  ,)

(defeffect ::send-key-event [{:keys [browser key-event]}]
  (gen3/call (gen3/call browser :get_host)
             :send_key_event
             (gen3/map->key-event key-event)))



