(ns com.phronemophobic.easel.schematic
  (:require
   [com.phronemophobic.easel.model :as model]
   [com.phronemophobic.membrane.schematic3
    :as schematic]))

(defn schematic-ui [this $context context]
  (let [root (get context ::root (:root schematic/initial-state))
        $root [$context
               (list 'keypath ::root)
               (list 'nil->val (:root schematic/initial-state))]
        state (-> (:state this)
                  (assoc :context
                         (-> context
                             (assoc :membrane.stretch/container-size (:size this))
                             (dissoc ::root)))
                  (assoc :$context $context
                         :extra (:extra this)
                         :$extra [(:$ref this) '(keypath :extra)]
                         :root root
                         :$root $root
                         :$selection [(:$ref this) '(keypath :state) '(keypath :selection)]
                         :$collapsed [(:$ref this) '(keypath :state) '(keypath :collapsed)]
                         :$interns [(:$ref this) '(keypath :state) '(keypath :interns)]
                         :$selected-tool [(:$ref this) '(keypath :state) '(keypath :selected-tool)]
                         :$components [(:$ref this) '(keypath :state) '(keypath :components)]))]
    (schematic/main-view state)))



(defrecord Schematlet [dispatch! eval-ns]
  model/IApplet
  (-start [this $ref size]
    (assoc this
           ;; :dispatch! dispatch!
           :state schematic/initial-state
           :$ref $ref
           :size size))
  (-stop [this])
  (-ui [this $context context]
    (schematic-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn schematlet [handler]
  (-> (->Schematlet handler (the-ns 'com.phronemophobic.easel.schematic))
      (assoc :label "schematic")))

(comment

  ,)
