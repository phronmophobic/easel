(ns com.phronemophobic.easel.ns-watcher
  (:require
   [membrane.component :refer
    [defui
     defeffect]]
   [membrane.ui :as ui]
   [membrane.basic-components :as basic]
   [membrane.alpha.component.drag-and-drop :as dnd]
   [com.phronemophobic.easel.model :as model]
   [clojure.core.async :as async]
   [clojure.set :as set]
   [com.phronemophobic.viscous :as viscous]

   [com.phronemophobic.membrandt :as ant]
   ))

(defonce watch-list* (atom {}))

(defn unwatch-ns [key]
  (let [[old-watch-list watch-list] (swap-vals! watch-list* dissoc key)]
    (when-let [old-watch (get old-watch-list key)]
      (async/close! (:quit-ch old-watch)))))

(defn watch-ns [ns key cb]
  (let [[old-watch-list watch-list]
        (swap-vals! watch-list*
                    (fn [watch-list]
                      (if (get watch-list key)
                        watch-list
                        (assoc watch-list key {:callback-ch (async/chan)
                                               :quit-ch (async/chan)}))))

        {:keys [quit-ch callback-ch]} (get watch-list key)
        var-change-ch (async/chan (async/dropping-buffer 1))
        initial-interns (ns-interns ns)]

    (if (get old-watch-list key)
      (async/put! callback-ch cb)
      ;; else new watch
      (async/go
        (try
          (doseq [[_ var] initial-interns]
            (add-watch var key
                       (fn [_ _ _ _]
                         (async/put! var-change-ch var))))

          (loop [old-interns initial-interns
                 cb cb]
            (async/alt!
              callback-ch ([cb] (recur old-interns cb))
              quit-ch ([_]
                       (doseq [[_ var] old-interns]
                         (remove-watch var key))
                       nil)
              var-change-ch ([_]
                             (cb)
                             (recur old-interns cb))
              (async/timeout 250) ([_]
                                    (let [new-interns (ns-interns ns)]
                                      (when (not (identical? new-interns
                                                             old-interns))

                                        ;; update watches
                                        (let [old-vars (set (vals old-interns))
                                              new-vars (set (vals new-interns))
                                              removed-vars (set/difference old-vars new-vars)
                                              added-vars (set/difference new-vars old-vars)]
                                          (doseq [var removed-vars]
                                            (remove-watch var key))
                                          (doseq [var added-vars]
                                            (add-watch var key
                                                       (fn [_ _ _ _]
                                                         (async/put! var-change-ch true)))))

                                        (cb))
                                      (recur new-interns cb)))
              :priority true))
          (catch Exception e
            (prn e))
          (finally
            (swap! watch-list* dissoc key)
            (async/close! var-change-ch)
            (println "closing")))))

    (fn []
      (async/close! quit-ch))))

(comment
  (def stop-watch (watch-ns *ns*
                            ::test
                            (fn []
                              (println "change3"))))
  ,)

(defui ns-view [{:keys [interns size ns]}]
  (basic/scrollview
   {:scroll-bounds size
    :$body nil
    :body
    (ui/table-layout
     (eduction
      (map (fn [[sym obj]]
             (let [inspector-extra (get extra [::inspector sym])]
               [
                (ui/on
                 :mouse-down
                 (fn [_]
                   [[::dnd/drag-start {::dnd/obj {:x (delay
                                                       (ns-resolve ns sym))}}]])
                 (ui/label
                  (name sym)
                  (ui/font "Menlo" 11)))
                (viscous/inspector
                 {:obj obj
                  :width (get inspector-extra :width 40)
                  :height (get inspector-extra :height 1)
                  :show-context? (get inspector-extra :show-context?)
                  :extra inspector-extra})])))
      interns)
     4
     2)}))

(defn watcher-ui [this $context context]
  (let [state (-> this
                  (assoc :context context
                         :$context $context
                         :extra (:extra this)
                         :$extra [(:$ref this) '(keypath :extra)]))]
    (ns-view state)))


(defn ns->interns [ns]
  (into []
        (map (fn [[sym var]]
               [sym (viscous/wrap (deref var))]))
        (sort-by
         (fn [[sym var]]
           (let [{:keys [line column]} (meta var)]
             [(or line -1)
              (or column -1)]))
         (ns-interns ns))))

(defrecord NSWatcher [dispatch! ns]
  model/IApplet
  (-start [this $ref size _content-scale]
    (let [watch-key [::ns-watcher ns]
          interns (ns->interns ns)
          $interns [$ref '(keypath :interns)]]
      (assoc this
             :extra {}
             :ns ns
             :interns interns
             ;; :unwatch unwatch
             :$ref $ref
             ::model/queue
             [(fn []
                (let [unwatch (watch-ns ns watch-key
                                        (fn []
                                          (dispatch! :set $interns (ns->interns ns))))]
                  (dispatch! :set [$ref '(keyath :unwatch)] unwatch)))]
             :size size)))
  (-stop [this]
    (when-let [unwatch (:unwatch this)]
      (unwatch))
    nil)
  model/IUI
  (-ui [this $context context]
    (watcher-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn ns-watcher-applet [handler ns]
  (-> (->NSWatcher handler ns)
      (assoc :label (str "ns-watcher") )))


