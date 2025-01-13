(ns com.phronemophobic.easel.schematic2
  (:require
   [membrane.ui :as ui]
   [membrane.component
    :refer [defeffect]]
   [com.rpl.specter :as specter]
   [com.phronemophobic.easel.model :as model]
   [com.phronemophobic.schematic.model :as sm]
   [com.phronemophobic.schematic.view.component-picker :as component-picker]
   [com.phronemophobic.schematic.view.tree :as tree]
   [com.phronemophobic.schematic.view.preview :as preview]
   [com.phronemophobic.schematic.view.detail :as detail]))


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
                             (assoc :membrane.stretch/container-size size
                                    :eval-ns (:eval-ns this))
                             (dissoc ::elem)))
                  (assoc :$context $context
                         :elem elem
                         :$elem $elem
                         :eval-ns (:eval-ns this)
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
  (-start [this $ref size _content-scale]
    (assoc this
           ;; :dispatch! dispatch!
           :$ref $ref
           :size size))
  (-stop [this])
  model/IUI
  (-ui [this $context context]
    (toolbar-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn toolbar-applet [handler eval-ns]
  (-> (->ToolbarApplet)
      (assoc :label "Toolbar"
             :eval-ns eval-ns)))

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
                             (assoc :eval-ns (:eval-ns this))
                             (assoc :selection selection
                                    :$selection $selection)
                             (dissoc ::elem)))
                  (assoc :$context $context
                         :elem elem
                         :$elem $elem
                         :eval-ns (:eval-ns this)
                         :extra (:extra this)
                         :$extra [(:$ref this) '(keypath :extra)]))]
    (ui/scissor-view
     [0 0]
     size
     (preview/editor state))))

(defrecord PreviewApplet []
  model/IApplet
  (-start [this $ref size _content-scale]
    (assoc this
           ;; :dispatch! dispatch!
           :$ref $ref
           :size size))
  (-stop [this])
  model/IUI
  (-ui [this $context context]
    (preview-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn preview-applet [handler eval-ns]
  (-> (->PreviewApplet)
      (assoc :label "Preview"
             :eval-ns eval-ns)))

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
                             (assoc :eval-ns (:eval-ns this))
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
  (-start [this $ref size _content-scale]
    (assoc this
           ;; :dispatch! dispatch!
           :$ref $ref
           :size size))
  (-stop [this])
  model/IUI
  (-ui [this $context context]
    (tree-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn tree-applet [handler eval-ns]
  (-> (->TreeApplet)
      (assoc :label "Tree View"
             :eval-ns eval-ns)))

(defn component-picker-ui [this $context context]
  (let [size (:size this)
        components (get context ::component-picker-components)
        selection (get context ::selection)
        $selection [$context
                    (list 'keypath ::selection)]
        state (-> (:state this)
                  (assoc :context
                         (-> context
                             (assoc :membrane.stretch/container-size (:size this))
                             (assoc :eval-ns (:eval-ns this))
                             (assoc :selection selection
                                    :$selection $selection)
                             (dissoc ::elem)))
                  (assoc :$context $context
                         :components components
                         :extra (:extra this)
                         :$extra [(:$ref this) '(keypath :extra)]))]
    (ui/scissor-view
     [0 0]
     size
     (component-picker/component-picker state))))




(defrecord ComponentPickerApplet [dispatch!]
  model/IApplet
  (-start [this $ref size _content-scale]

    (assoc this
           ;; :dispatch! dispatch!
           :$ref $ref
           :size size
           ::model/queue
           [(fn []
              ;; cheat for now
              (dispatch! :update
               '[(keypath :membrane.component/context)]
               (fn [context]
                 (assoc context
                        ::component-picker-components component-picker/component-starters))))]))
  (-stop [this])
  model/IUI
  (-ui [this $context context]
    (component-picker-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn component-picker-applet [handler]
  (-> (->ComponentPickerApplet handler)
      (assoc :label "component-picker")))



(comment

  ,)

(def elem-by-id (memoize sm/elem-by-id))
(defn detail-ui [this $context context]
  (let [size (:size this)
        root (get context ::elem)
        $root [$context
               (list 'keypath ::elem)]

        selection (get context ::selection)
        $selection [$context
                    (list 'keypath ::selection)]

        selection-id (first selection)
        path (when selection-id
               (elem-by-id selection-id))
        elem (when path
               (specter/select-one path root))
        $elem (when elem
                [$root (list 'path path)])

        state (-> (:state this)
                  (assoc :context
                         (-> context
                             (assoc :membrane.stretch/container-size (:size this))
                             (assoc :eval-ns (:eval-ns this))
                             (dissoc ::elem)))
                  (assoc :$context $context
                         :root root
                         :$root $root
                         :elem elem
                         :$elem $elem
                         :extra (:extra this)
                         :$extra [(:$ref this) '(keypath :extra)]))]
    (ui/scissor-view
     [0 0]
     size
     (detail/editor state))))

(defrecord DetailApplet []
  model/IApplet
  (-start [this $ref size _content-scale]
    (assoc this
           ;; :dispatch! dispatch!
           :$ref $ref
           :size size))
  (-stop [this])
  model/IUI
  (-ui [this $context context]
    (detail-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn detail-applet [handler eval-ns]
  (-> (->DetailApplet)
      (assoc :label "Detail View"
             :eval-ns eval-ns)))


