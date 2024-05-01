(ns com.phronemophobic.easel.browser
  (:require
   [com.phronemophobic.easel.model :as model]
   [membrane.skia :as skia]
   [membrane.ui :as ui]
   [clojure.java.io :as io]
   [com.phronemophobic.gen3 :as gen3]
   [com.phronemophobic.cef :as cef]
   [com.phronemophobic.cef.browser :as b])
  (:import com.sun.jna.Pointer
           com.sun.jna.Function
           com.phronemophobic.membrane.Skia
           com.phronemophobic.gen3.structs.cef_browser_host_t
           ;;com.phronemophobic.cljcef.CefBrowser
           ))


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

(defrecord Browser [browser browser-id focused? content-scale width height resource draw-lock]
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
          (gen3/call (gen3/call browser :get_host)
                     :send_key_event
                     (gen3/map->key-event
                      {:type 3
                       :modifiers 0
                       :character c
                       :unmodified-character c}))
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
                         :modifiers mods
                         :native-key-code code
                         :character (char key)
                         :unmodified-character (char key)}]
          (gen3/call (gen3/call browser :get_host)
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
        view
        (assoc
         (->Browser (:browser browser-info)
                    (:browser-id browser-info)
                    focus?
                    (:content-scale browser-info)
                    (:width browser-info)
                    (:height browser-info)
                    (:resource browser-info)
                    (:draw-lock browser-info))
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
    view))



(defrecord Browslet [dispatch! initial-url]
  model/IApplet
  (-start [this $ref [initial-width initial-height]]
    (let [dispatch-main
          (fn [work]
            (dispatch! :dispatch-main work))

          $browser-info [$ref
                         '(keypath :browser-info)]

          cef-path (doto (io/file ".cef")
                     (.mkdirs))
          cache-path (doto (io/file ".browser-cache")
                       (.mkdirs))]
      (future
        (b/create-browser [initial-width initial-height]
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
                           :cef-path cef-path
                           :cache-path cache-path
                           :on-paint+content-scale
                           (fn [browser content-scale  paint-type nrects rects buffer width height]
                             (skia-draw dispatch! $browser-info content-scale paint-type nrects rects buffer width height)
                             (dispatch! :repaint!))}))
      (assoc this
             :browser-info
             {:draw-lock (Object.)
              :width initial-width
              :height initial-height})))

  (-stop [this]
    (when-let [browser (-> this :browser-info :browser)]
      (b/close browser))
    (update this :browser-info dissoc :browser))
  (-ui [this $context context]
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
