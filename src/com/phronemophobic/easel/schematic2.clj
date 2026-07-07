(ns com.phronemophobic.easel.schematic2
  (:require
   [membrane.ui :as ui]
   [membrane.component
    :refer [defui defeffect]]
   [membrane.basic-components :as basic]
   [com.rpl.specter :as specter]
   [datalevin.core :as d]
   [clojure.java.io :as io]
   [membrane.alpha.component.drag-and-drop :as dnd]
   [com.phronemophobic.membrandt.impl.grid :as grid]
   [membrane.component.present :as present]
   [com.phronemophobic.easel.model :as model]
   [com.phronemophobic.schematic.model :as sm]
   [com.phronemophobic.schematic.view.component-picker :as component-picker]
   [com.phronemophobic.schematic.view.tree :as tree]
   [com.phronemophobic.schematic.view.preview :as preview]
   [com.phronemophobic.schematic.view.detail :as detail]))


(def db-schema {;; :aka  {:db/cardinality :db.cardinality/many}
                ;; :db/valueType is optional, if unspecified, the attribute will be
                ;; treated as EDN blobs, and may not be optimal for range queries
                ;; :element/id {:db/valueType :db.type/string
                ;;              :db/unique    :db.unique/identity}
                ;; :replay/initial-save {:db/cardinality :db.cardinality/one
                ;;                       :db/valueType :db.type/ref}
                
                :component/version {:db/valueType :db.type/uuid
                                    :db/unique :db.unique/identity
                                    :db/cardinality :db.cardinality/one}
                :branch/component-name {:db/valueType :db.type/symbol
                                        :db/unique :db.unique/identity
                                        :db/cardinality :db.cardinality/one}
                :branch/current-version {:db/cardinality :db.cardinality/one
                                         :db/valueType :db.type/ref}
                })

(def db-conn (delay
               (d/get-conn 
                (.getCanonicalPath (io/file "../easel/schematic.db"))
                db-schema)))

(comment
  @db-conn
  (d/close @db-conn)
  ,)

(defn save-new-component! [component-name eval-ns]
  (let [name-sym (symbol component-name)
        component-version (random-uuid)
        component {:element/type :com.phronemophobic.schematic.model/component,
                   :component/name name-sym
                   ;; :component/body nil,
                   :element/eval-ns (ns-name eval-ns)
                   :save/inst (java.time.Instant/now)
                   :component/version component-version
                   :element/id (random-uuid)
                   }
        component-branch {:branch/component-name component-name
                          :branch/current-version [:component/version component-version]}]
    (d/transact! @db-conn
                 [component
                  component-branch])))



(defn list-components []
  (into []
        (map first)
        (d/q '[:find
               (pull ?component [:component/version
                                 :element/type
                                 :component/name
                                 :element/eval-ns
                                 :save/inst
                                 :element/id
                                 :component/body])
               :where
               [?branch :branch/component-name]
               [?branch :branch/current-version ?component]]
             (d/db @db-conn))))

(defn save-component! [component]
  (let [component-version (random-uuid)
        component (-> component
                      (dissoc :db/id)
                      (assoc :component/version component-version)
                      (assoc :save/inst (java.time.Instant/now)))
        component-branch {:branch/component-name (:component/name component)
                          :branch/current-version [:component/version component-version]}]
    (d/transact! @db-conn
                 [component
                  component-branch])))

(comment
  (show-component-list!)
  (list-components)

  (update-component component )
  (save-component! '{:db/id 1,
                     :component/version #uuid "5601d17f-9158-4320-a559-7520fbc93ec5",
                     :element/type :com.phronemophobic.schematic.model/component,
                     :component/name foo,
                     :component/body {}
                     :element/eval-ns com.phronemophobic.easel.schematic2,
                     :save/inst #inst "2026-07-07T17:31:01.797884000-00:00",
                     :element/id #uuid "7668afb8-fe38-4c00-be5b-c5cbcf13f0d0"})
  
  


  
  (save-new-component! 'foo2 *ns*)
  ,)

(defui with-row-hover [{:keys [row hover body cell-width cell-height]}]
  (let [hover? (= hover row)]
    (if hover?
      [(ui/filled-rectangle 
        [0.9 0.9 0.9]
        cell-width cell-height)
       body]
      (ui/on
       :mouse-move
       (fn [_]
         [[:set $hover row]])
       (ui/fixed-bounds
        [cell-width cell-height]
        body)))))



(defeffect ::load-components [{:keys [$components]}]
  (future
    (let [components (list-components)]
      (dispatch! :set $components components))))

(defui component-row [{:keys [component]
                       :membrane.ui/keys [width height]}]
  (ui/on
   :mouse-down
   (fn [_]
     [[::dnd/drag-start {::dnd/obj {:x
                                    (delay
                                      component)}}]
      #_[::open-game component]])
   (basic/flex-layout
    {:$elems nil 
     :elems
     [(ui/label (:component/name component))]
     :pad 4
     :layout {:direction :row
              :width width
              :height height}})))

(defui component-list [{:keys [components]}]
  (case components
    
    nil
    (present/on-present
     (fn []
       [[:set $components ::loading]
        [::load-components {:$components $components}]])
     (ui/label "Loading..."))
    
    ::loading (ui/label "Loading...")
    
    ;; else
    (let [[cw ch] (:membrane.stretch/container-size context)
          cw (- cw 20)
          ch (- ch 20)
          
          scroll-state (get extra ::scroll-state)
          $scroll-state $scroll-state
          scroll-state (assoc scroll-state
                              :col-offset 0
                              :col-index 0)
          hover (get extra ::hover)
          
          table (grid/list-view
                 {:row-fn 
                  (fn [{:keys [row]}]
                    (component-row
                     {:component (nth components row)
                      ::ui/width cw
                      ::ui/height 20}))
                  
                  :num-rows (count components)
                  :width cw
                  :height ch
                  :scroll-state scroll-state
                  :$scroll-state $scroll-state})]
      table)))

(defn show-component-list! [] 
  ((requiring-resolve 'dev/add-component-as-applet)
   #'component-list
   {}))




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
  (-start [this {:keys [$ref size]}]
    (assoc this
           ;; :dispatch! dispatch!
           :$ref $ref
           :size size))
  (-stop [this])
  model/IUI
  (-ui [this {:keys [$context context]}]
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
    (preview/editor state)))

(defrecord PreviewApplet []
  model/IApplet
  (-start [this {:keys [$ref size]}]
    (assoc this
           ;; :dispatch! dispatch!
           :$ref $ref
           :size size))
  (-stop [this])
  model/IUI
  (-ui [this {:keys [$context context]}]
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
  (-start [this {:keys [$ref size]}]
    (assoc this
           ;; :dispatch! dispatch!
           :$ref $ref
           :size size))
  (-stop [this])
  model/IUI
  (-ui [this {:keys [$context context]}]
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


(comment
  ;; update components in component picker to defaults
  (com.phronemophobic.easel/handler :update
             '[(keypath :membrane.component/context)]
             (fn [context]
               (assoc context
                      ::component-picker-components component-picker/component-starters)))
  ,)

(defrecord ComponentPickerApplet [dispatch!]
  model/IApplet
  (-start [this {:keys [$ref size]}]

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
  (-ui [this {:keys [$context context]}]
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
  (-start [this {:keys [$ref size]}]
    (assoc this
           ;; :dispatch! dispatch!
           :$ref $ref
           :size size))
  (-stop [this])
  model/IUI
  (-ui [this {:keys [$context context]}]
    (detail-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn detail-applet [handler eval-ns]
  (-> (->DetailApplet)
      (assoc :label "Detail View"
             :eval-ns eval-ns)))


