(ns com.phronemophobic.easel.term
  (:require
   [com.phronemophobic.easel.model :as model]
   [clojure.core.async :as async]
   [membrane.skia :as skia]
   [asciinema.vt :as vt]
   [com.phronemophobic.membrane.term :as term]
   [membrane.ui :as ui]
   [clojure.java.io :as io])
  (:import [com.pty4j PtyProcess WinSize]))


(def term-events #'term/term-events)
(def term-view @#'term/term-view)

(def repaint! @#'skia/glfw-post-empty-event)

(def menlo
  (#'term/load-terminal-font skia/toolkit
                             "Menlo"
                             11))

(defn term-ui [this $context context]
  (let [focus (:focus context)
        focus? (= focus (:id this))
        view
        (ui/on
         :mouse-down
         (fn [_]
           [[:set [$context (list 'keypath :focus)]
             (:id this)]])
         (term-view term/default-color-scheme
                    menlo
                    (:vt this)))
        view (if focus?
               (ui/wrap-on
                :key-event
                (fn [handler key scancode action mods]
                  (when (not= 39 key)
                    (handler key scancode action mods)))
                (term-events (:pty this)
                             view))
               view)
        view (ui/fill-bordered
              (if focus?
                [0.8 0.8 0.8]
                [0.95 0.95 0.95])
              10
              view)]
    view))

(defn debounce-chan
  "Reads from channel `in`. Will wait `ms` milliseconds
  and write the latest value read from `in` to `out`."
  [in out ms]
  (async/go
    (loop []
      (let [x (async/<! in)]
        (when x
          (loop [to (async/timeout ms)
                 x x]
            (async/alt!
              to ([_] (async/>! out x))
              in ([x] (recur to x))))
          (recur))))))

(defn repaint-chan []
  (let [debounced (async/chan (async/sliding-buffer 1))
        in (async/chan)]
    (debounce-chan in debounced 30)
    (async/thread
      (loop []
        (let [x (async/<!! debounced)]
          (when x
            (repaint!)
            (recur)))))
    in))

(defrecord Termlet [dispatch!]
  model/IApplet
  (-start [this $ref [w h]]
    (let [w (- w 20)
          h (- h 20)
          ;; enforce min size. current virtual term library struggles with
          ;; very small terminals.
          cols (max 80
                    (quot w (:membrane.term/cell-width menlo)))
          rows (max 13
                    (quot h (:membrane.term/cell-height menlo)))]
      (async/thread
        (let [cmd (into-array String ["/bin/bash" "-l"])
              pty (PtyProcess/exec ^"[Ljava.lang.String;" cmd
                                   ^java.util.Map (merge (into {} (System/getenv))
                                                         {"TERM" "xterm-256color"}))

              ;; doesn't quite work anyway?
              ;; pty (while
              ;;         (not(doto pty
              ;;            (.setWinSize (WinSize. width height)))))
              is (io/reader (.getInputStream pty))
              os (.getOutputStream pty)
              repaint-ch (repaint-chan)]
          (dispatch! :update $ref
                     (fn [this]
                       (assoc this
                              :pty pty
                              :os os
                              :is is)))
          (try
            (with-open [is is]
              (loop []
                (let [input (.read is)]
                  (when (not= -1 input)
                    (dispatch! :update [$ref '(keypath :vt)]
                               (fn [vt]
                                 (vt/feed-one vt input)))
                    (async/put! repaint-ch true)
                    (recur)))))
            (catch Exception e
              (prn e)))))
      (assoc this
             :vt (vt/make-vt cols rows))))
  (-stop [this]
    (.destroy (:pty this))
    (.close (:os this)))
  (-ui [this $context context]
    (term-ui this $context context))
  model/IResizable
  (-resize [this [w h :as new-size] _content-scale]
    (let [w (- w 20)
          h (- h 20)
          ;; enforce min size. current virtual term library struggles with
          ;; very small terminals.
          cols (max 80
                    (quot w (:membrane.term/cell-width menlo)))
          rows (max 13
                    (quot h (:membrane.term/cell-height menlo)))]
      (if (= [cols rows]
             [(-> this :vt :screen :width)
              (-> this :vt :screen :height)])
        this
        ;; else
        (do
          (.setWinSize (:pty this)
                       (WinSize. cols rows))

          (assoc this :vt (vt/make-vt cols rows)))))))

(defn termlet [handler]
  (-> (->Termlet handler)
      (assoc :label "term")))
