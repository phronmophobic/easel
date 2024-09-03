(ns com.phronemophobic.easel.term
  (:require
   [com.phronemophobic.easel.model :as model]
   [clojure.core.async :as async]
   [membrane.skia :as skia]
   [asciinema.vt :as vt]
   [com.phronemophobic.membrane.term :as term]
   [membrane.ui :as ui]
   [clojure.java.io :as io])
  (:import [com.pty4j WinSize PtyProcessBuilder]))


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
                ;; todo send events through cmd-ch
                (if-let [pty (:pty this)]
                  (term-events pty view)
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
  (-start [this $ref [w h] _content-scale]
    (let [w (- w 20)
          h (- h 20)
          ;; enforce min size. current virtual term library struggles with
          ;; very small terminals.
          cols (int
                (max 80
                     (quot w (:membrane.term/cell-width menlo))))
          rows (int
                (max 13
                     (quot h (:membrane.term/cell-height menlo))))
          cmd-ch (async/chan 20)]
      (async/thread
        (let [cmd (into-array String ["/bin/bash" "-l"])
              pty-builder (doto (PtyProcessBuilder. cmd)
                            (.setInitialColumns cols)
                            (.setInitialRows rows)
                            (.setEnvironment (merge (into {} (System/getenv))
                                                    {"TERM" "xterm-256color"})))
              pty (.start pty-builder)
              is (io/reader (.getInputStream pty))
              os (.getOutputStream pty)
              repaint-ch (repaint-chan)]
          (async/thread
            (try
              (loop []
                (when-let [msg (async/<!! cmd-ch)]
                  (case (:type msg)

                    :resize (do (.setWinSize pty (WinSize. (:cols msg) (:rows msg)))
                                (recur))

                    ;; else
                    (prn "unrecognized term message" msg))))
              (catch Exception e
                (prn e))
              (finally
                (.destroy pty)
                (.close os))))
          (dispatch! :update $ref
                     (fn [this]
                       (assoc this
                              ;; still used by term-events
                              ;; todo: update term events to use
                              ;; cmd-ch
                              :pty pty)))
          (try
            (with-open [is is]
              (loop []
                (let [input (.read is)]
                  (when (not= -1 input)
                    (try
                      (dispatch! :update [$ref '(keypath :vt)]
                                 (fn [vt]
                                   (vt/feed-one vt input)))
                      (catch IllegalArgumentException e
                        ;; ignore
                        nil)
                      )
                    (async/offer! repaint-ch true)
                    (recur)))))
            (catch Exception e
              (prn e)))))
      (assoc this
             :cmd-ch cmd-ch
             :vt (vt/make-vt cols rows))))
  (-stop [this]
    (async/close! (:cmd-ch this)))
  model/IUI
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
          (async/put! (:cmd-ch this)
                      {:type :resize
                       :cols cols
                       :rows rows})
          (assoc this :vt (vt/make-vt cols rows)))))))

(defn termlet [handler]
  (-> (->Termlet handler)
      (assoc :label "term")))
