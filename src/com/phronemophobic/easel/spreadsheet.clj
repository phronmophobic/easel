(ns com.phronemophobic.easel.spreadsheet
  (:require
   [com.phronemophobic.easel.model :as model]
   [clojure.core.async :as async]
   [com.phronemophobic.membrane.spreadsheet
    :as ss]))

(defn spreadsheet-ui [this $context context]
  (let [state(-> this
                 (assoc :context
                        (-> context
                            (assoc :membrane.stretch/container-size (:size this))))
                 (assoc :$context $context
                        :extra (:extra this)
                        :$extra [(:$ref this) '(keypath :extra)]
                        :$ss [(:$ref this) '(keypath :ss)]
                        :$results [(:$ref this) '(keypath :results)]
                        :$ns-info [(:$ref this) '(keypath :ns-info)]
                        :drop-object (:drop-object context)
                        :$drop-object [$context '(keypath :drop-object)]))]
    (ss/spreadsheet-editor state)))

(defn run-results
  ([dispatch! $ss-state]
   (let [ss-chan (async/chan (async/sliding-buffer 1))
         result-thread (let [binds (clojure.lang.Var/getThreadBindingFrame)]
                         (doto (Thread. (fn []
                                          (clojure.lang.Var/resetThreadBindingFrame binds)
                                          (loop []
                                            (when-let [ss (async/<!! ss-chan)]
                                              (try
                                                (let [state (dispatch! :get $ss-state)
                                                      cache (get state :cache {})
                                                      side-effects (get state :side-effects {})

                                                      ns-info (get state :ns-info)
                                                      _ (assert ns-info)
                                                      eval-ns (the-ns (:name ns-info))

                                                      {results :results
                                                       next-cache :cache
                                                       side-effects :side-effects} (ss/calc-spreadsheet eval-ns cache side-effects ss)]
                                                  (dispatch! :update $ss-state
                                                             assoc :results results
                                                             :side-effects side-effects
                                                             :cache next-cache
                                                             :last-ns-info ns-info)
                                                  (dispatch! :repaint! ))
                                                (catch Throwable e
                                                  (prn e)))
                                              (recur))))
                                        "Result-Thread")))]
     (.start result-thread)

     (dispatch! :watch
                [$ss-state ::update-check]
                $ss-state
                (fn [key ref old new]
                  (when (not= (:ss new)
                              (:ss old))
                    (async/put! ss-chan (:ss new)))))
     {:ss-chan ss-chan
      :result-thread result-thread})))

(defrecord SpreadSheet [dispatch! eval-ns]
  model/IApplet
  (-start [this $ref size]
    (merge
     (assoc this
            :ss []
            :ns-info {:name eval-ns
                      :require '([membrane.ui :as ui
                                  :refer [vertical-layout
                                          horizontal-layout]]
                                 [clojure.java.io :as io]
                                 [com.phronemophobic.membrane.spreadsheet :as ss]
                                 [clojure.edn :as edn]
                                 [xtdb.api :as xt]
                                 clojure.set
                                 [clojure.string :as str]
                                 [clojure.data.json :as json])
                      :import '(java.io.PushbackReader
                                java.time.Instant)}
            :$ref $ref
            :size size)
     (run-results dispatch! $ref))) 
  (-stop [this])
  (-ui [this $context context]
    (spreadsheet-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn spreadsheet-applet [handler]
  (-> (->SpreadSheet handler 'com.phronemophobic.easel)
      (assoc :label "spreadsheet")))

