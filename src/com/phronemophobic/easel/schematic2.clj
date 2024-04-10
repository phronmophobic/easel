(ns com.phronemophobic.easel.schematic2
  (:require
   [membrane.ui :as ui]
   [com.phronemophobic.easel.model :as model]
   [com.phronemophobic.schematic.model :as sm]
   [com.phronemophobic.schematic.view.component-picker :as component-picker]
   [com.phronemophobic.schematic.view.tree :as tree]
   [com.phronemophobic.schematic.view.preview :as preview]))


(defn toolbar-ui [this $context context]
  (let [size (:size this)
        
        elem (get context ::elem)
        $elem [$context
               (list 'keypath ::elem)]

        selection (get context ::selection)
        $selection [$context
                    (list 'keypath ::selection)]
        
        state (-> (:state this)
                  (assoc :context
                         (-> context
                             (assoc :membrane.stretch/container-size size)
                             (dissoc ::elem)))
                  (assoc :$context $context
                         :elem elem
                         :$elem $elem
                         :selection selection
                         :$selection $selection
                         :extra (:extra this)
                         :$extra [(:$ref this) '(keypath :extra)]))]
    (ui/scissor-view
     [0 0]
     size
     (preview/toolbar state))))

(defrecord ToolbarApplet []
  model/IApplet
  (-start [this $ref size]
    (assoc this
           ;; :dispatch! dispatch!
           :$ref $ref
           :size size))
  (-stop [this])
  (-ui [this $context context]
    (toolbar-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn toolbar-applet [handler]
  (-> (->ToolbarApplet)
      (assoc :label "Toolbar")))

(defn preview-ui [this $context context]
  (let [size (:size this)
        
        elem (get context ::elem)
        $elem [$context
               (list 'keypath ::elem)]

        selection (get context ::selection)
        $selection [$context
                    (list 'keypath ::selection)]
        
        state (-> (:state this)
                  (assoc :context
                         (-> context
                             (assoc :membrane.stretch/container-size size)
                             (assoc :selection selection
                                    :$selection $selection)
                             (dissoc ::elem)))
                  (assoc :$context $context
                         :elem elem
                         :$elem $elem
                         :extra (:extra this)
                         :$extra [(:$ref this) '(keypath :extra)]))]
    (ui/scissor-view
     [0 0]
     size
     (preview/editor state))))

(defrecord PreviewApplet []
  model/IApplet
  (-start [this $ref size]
    (assoc this
           ;; :dispatch! dispatch!
           :$ref $ref
           :size size))
  (-stop [this])
  (-ui [this $context context]
    (preview-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn preview-applet [handler]
  (-> (->PreviewApplet)
      (assoc :label "Preview")))

(defn tree-ui [this $context context]
  (let [size (:size this)
        elem (get context ::elem)
        $elem [$context
               (list 'keypath ::elem)]

        selection (get context ::selection)
        $selection [$context
                    (list 'keypath ::selection)]
               
        state (-> (:state this)
                  (assoc :context
                         (-> context
                             (assoc :membrane.stretch/container-size (:size this))
                             (assoc :selection selection
                                    :$selection $selection)
                             (dissoc ::elem)))
                  (assoc :$context $context
                         :elem elem
                         :$elem $elem
                         :extra (:extra this)
                         :$extra [(:$ref this) '(keypath :extra)]))]
    (ui/scissor-view
     [0 0]
     size
     (tree/editor state))))

(defrecord TreeApplet []
  model/IApplet
  (-start [this $ref size]
    (assoc this
           ;; :dispatch! dispatch!
           :$ref $ref
           :size size))
  (-stop [this])
  (-ui [this $context context]
    (tree-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn tree-applet [handler]
  (-> (->TreeApplet)
      (assoc :label "Tree View")))

(defn component-picker-ui [this $context context]
  (let [size (:size this)
        selection (get context ::selection)
        $selection [$context
                    (list 'keypath ::selection)]
        state (-> (:state this)
                  (assoc :context
                         (-> context
                             (assoc :membrane.stretch/container-size (:size this))
                             (assoc :selection selection
                                    :$selection $selection)
                             (dissoc ::elem)))
                  (assoc :$context $context
                         :extra (:extra this)
                         :$extra [(:$ref this) '(keypath :extra)]))]
    (ui/scissor-view
     [0 0]
     size
     (component-picker/component-picker state))))




(defrecord ComponentPickerApplet []
  model/IApplet
  (-start [this $ref size]
    (assoc this
           ;; :dispatch! dispatch!
           :$ref $ref
           :size size))
  (-stop [this])
  (-ui [this $context context]
    (component-picker-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn component-picker-applet [handler]
  (-> (->ComponentPickerApplet)
      (assoc :label "component-picker")))



(comment

  ,)
