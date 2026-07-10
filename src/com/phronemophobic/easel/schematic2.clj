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
   [com.phronemophobic.membrandt.icon.ui :as icon.ui]
   [membrane.component.present :as present]
   [com.phronemophobic.easel.model :as model]
   [com.phronemophobic.easel :as-alias easel]
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

(defn save-new-component! 
  ([component-name]
   (when (not (qualified-symbol? component-name))
     (throw (ex-info "component name must be a qualified symbol"
                     {:component-name component-name})))
   
   (let [eval-ns (the-ns (symbol (namespace component-name)))

         name-sym (symbol component-name)
         component-version (random-uuid)
         
         component {:element/type ::sm/component,
                    :component/name name-sym
                    :element/eval-ns (ns-name eval-ns)
                    :save/inst (java.time.Instant/now)
                    :component/version component-version
                    :element/id (random-uuid)}
         component-branch {:branch/component-name component-name
                           :branch/current-version [:component/version component-version]}]
     (d/transact! @db-conn
                  [component
                   component-branch]))))

(defn load-component [component-name]
  (when (not (qualified-symbol? component-name))
    (throw (ex-info "component name must be a qualified symbol"
                    {:component-name component-name})))
  (let [component
        (ffirst
         (d/q '[:find
                (pull ?component [:component/version
                                  :element/type
                                  :component/name
                                  :element/eval-ns
                                  :save/inst
                                  :element/id
                                  :component/defaults
                                  :component/body])
                :in $ ?component-name
                :where
                [?branch :branch/component-name ?component-name]
                [?branch :branch/current-version ?component]]
              (d/db @db-conn)
              component-name))]
    component))

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
    (tap> {:saving component})
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
  
  


  
  (save-new-component! `hello-world *ns*)
  (load-component `foo2)
  ,)

(defui with-row-hover [{:keys [hover? body]
                        ::ui/keys [width height]}]
  (if hover?
    [(ui/filled-rectangle 
      [0.9 0.9 0.9]
      width height)
     body]
    (ui/on
     :mouse-move
     (fn [_]
       [[::do-hover {}]])
     (ui/fixed-bounds
      [width height]
      body))))



(defeffect ::load-components [{:keys [$components]}]
  (future
    (let [components (list-components)]
      (dispatch! :set $components components))))

(declare toolbar-applet
         preview-applet
         tree-applet
         detail-applet)
(defeffect ::open-component [{:keys [component/name
                                     element/eval-ns]}]
  (let [eval-ns (the-ns eval-ns)]
    (doseq [applet-fn [toolbar-applet
                       preview-applet
                       tree-applet
                       detail-applet]]
      (dispatch!
       :com.phronemophobic.easel/add-applet
       {:make-applet
        (fn [handler]
          (applet-fn handler
                     name
                     eval-ns))}))))

(comment
  (load-component `foo2)
  ,)

(defui component-row [{:keys [component hover]
                       :membrane.ui/keys [width height]}]
  (let [hover? (= hover (:component/name component))]
    (ui/on
     ::do-hover
     (fn [_]
       [[:set $hover (:component/name component)]])
     (with-row-hover
      {:hover? hover?
       ::ui/width width
       ::ui/height height
       :$body nil
       :body 
       (ui/on
        :mouse-down
        (fn [_]
          [[::dnd/drag-start {::dnd/obj {:x
                                         (delay
                                           component)}}]
           [::open-component component]])
        (basic/flex-layout
         {:$elems nil 
          :elems
          [(ui/label (:component/name component))]
          :pad 4
          :layout {:direction :row
                   :width width
                   :height height}}))}))))

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
          
          menu-bar
          (ui/on
           :mouse-down
           (fn [_]
             [[:set $components ::loading]
              [::load-components {:$components $components}]])
           (icon.ui/icon {:name "reload"}))

          table-height (- ch (ui/height menu-bar)) 


          table (grid/list-view
                 {:row-fn 
                  (fn [{:keys [row]}]
                    (component-row
                     {:component (nth components row)
                      :hover hover
                      ::ui/width cw
                      ::ui/height 20}))
                  
                  :num-rows (count components)
                  :width cw
                  :height table-height
                  :scroll-state scroll-state
                  :$scroll-state $scroll-state})]
      (ui/vertical-layout
       menu-bar
       table))))

(defn show-component-list! [] 
  ((requiring-resolve 'dev/add-component-as-applet)
   #'component-list
   {}))

(defn load-shared-component [{:keys [dispatch! component-name $elem]}]
  (let [component (load-component component-name)]
    (when (not (dispatch! :get $elem))
      (dispatch! :set $elem component))))

(defeffect ::save-elem [{:keys [elem]}]
  (save-component! elem))

(defn toolbar-ui [this component-state $context context]
  (let [size (:size this)
        
        elem (get component-state ::elem)
        selection (get component-state ::selection)
        
        state (-> this
                  (assoc :context
                         (-> context
                             (assoc :membrane.stretch/container-size size
                                    :eval-ns (:eval-ns this))
                             (dissoc ::elem)))
                  (assoc :$context $context
                         :elem elem
                         :selection selection))]
    (ui/scissor-view
     [0 0]
     size
     (ui/on 
      ::preview/save-elem
      (fn [m]
        [[::save-elem m]])
      (preview/toolbar state)))))

(defrecord ToolbarApplet []
  model/IApplet
  (-start [{:keys [component-name]
            :as this} 
           {:keys [$ref $shared size]}]
    (let [$component-state [$shared '(keypath ::components) (list 'keypath component-name)]
          $elem (conj $component-state '(keypath ::elem)) 
          $selection (conj $component-state '(keypath ::selection))
          this (assoc this
                      :$shared $shared
                      :extra {}
                      :$extra [$ref '(keypath :extra)]
                      :$elem $elem
                      :$selection $selection
                      ::easel/shared-keys [::components]
                      :$ref $ref
                      :size size)]
      (assoc this ::model/queue
             [(fn []
                (load-shared-component this))])))
  (-stop [this])
  model/IUI
  (-ui [{:keys [component-name] :as this} {:keys [$context context shared]}]
    (let [component-state (-> shared
                              ::components
                              (get component-name))]
      (toolbar-ui this component-state $context context)))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn toolbar-applet [handler component-name eval-ns]
  (-> (->ToolbarApplet)
      (assoc :label "Toolbar"
             :dispatch! handler
             :component-name component-name
             :eval-ns eval-ns)))

(comment
  
  (com.phronemophobic.easel/handler
   :com.phronemophobic.easel/add-applet
   {:make-applet
    (fn [handler]
      (toolbar-applet
       handler
       `foo2
       *ns*))})
  
  ,)

(def preview-ui (constantly nil))
(defn preview-ui [this component-state $context context]
  (let [size (:size this)
        elem (get component-state ::elem)


        selection (get component-state ::selection)
        $selection (get this :$selection)
        
        state (-> this
                  (assoc :elem elem)
                  (assoc :context
                         (-> context
                             (assoc :membrane.stretch/container-size size)
                             (assoc :eval-ns (:eval-ns this))
                             (assoc :selection selection
                                    :$selection $selection)
                             (dissoc ::elem)))
                  (assoc :$context $context))]

    
    (if elem
      (preview/editor state)
      (ui/label "loading..."))))



(defrecord PreviewApplet []
  model/IApplet
  (-start [{:keys [component-name]
            :as this} 
           {:keys [$ref $shared size]}]
    (let [
          $component-state [$shared '(keypath ::components) (list 'keypath component-name)]
          $elem (conj $component-state '(keypath ::elem)) 
          $selection (conj $component-state '(keypath ::selection))
          this 
          (assoc this
                 :$shared $shared
                 :extra {}
                 :$extra [$ref '(keypath :extra)]
                 :$elem $elem
                 :$selection $selection
                 ::easel/shared-keys [::components]
                 :$ref $ref
                 :size size)]
      (assoc this ::model/queue
             [(fn []
                (load-shared-component this))])))
  (-stop [this])
  model/IUI
  (-ui [{:keys [component-name] :as this} {:keys [$context context shared]}]
    (let [component-state (-> shared
                              ::components
                              (get component-name))]
      (preview-ui this component-state $context context)))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn preview-applet [handler component-name eval-ns]
  (-> (->PreviewApplet)
      (assoc :label "Preview"
             :dispatch! handler
             :component-name component-name
             :eval-ns eval-ns)))

(comment
  
  (com.phronemophobic.easel/handler
   :com.phronemophobic.easel/add-applet
   {:make-applet
    (fn [handler]
      (preview-applet
       handler
       `foo2
       *ns*))})
  
  ,)

(defn tree-ui [this component-state $context context]
  (let [size (:size this)
        elem (get component-state ::elem)

        selection (get component-state ::selection)
        $selection (get this :$selection)
               
        state (-> this
                  (assoc :context
                         (-> context
                             (assoc :membrane.stretch/container-size (:size this))
                             (assoc :eval-ns (:eval-ns this))
                             (assoc :selection selection
                                    :$selection $selection)
                             (dissoc ::elem)))
                  (assoc :$context $context
                         :elem elem))]
    (ui/scissor-view
     [0 0]
     size
     (tree/editor state))))

(defrecord TreeApplet []
  model/IApplet
  (-start [{:keys [component-name] :as this} {:keys [$ref size $shared]}]
    
    (let [$component-state [$shared '(keypath ::components) (list 'keypath component-name)]
          $elem (conj $component-state '(keypath ::elem))
          $selection (conj $component-state '(keypath ::selection))
          this (assoc this
                      :$shared $shared
                      :extra {}
                      :$extra [$ref '(keypath :extra)]
                      :$elem $elem
                      :$selection $selection
                      ::easel/shared-keys [::components]
                      :$ref $ref
                      :size size)]
      (assoc this ::model/queue
             [(fn []
                (load-shared-component this))])
      ))
  (-stop [this])
  model/IUI
  (-ui [{:keys [component-name] :as this} {:keys [$context context shared]}]
    (let [component-state (-> shared
                              ::components
                              (get component-name))]
      (tree-ui this component-state $context context))
    )
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn tree-applet [handler component-name eval-ns]
  (-> (->TreeApplet)
      (assoc :label "Tree View"
             :dispatch! handler
             :component-name component-name
             :eval-ns eval-ns)))

(comment
  (com.phronemophobic.easel/handler
   :com.phronemophobic.easel/add-applet
   {:make-applet
    (fn [handler]
      (tree-applet
       handler
       `foo2
       *ns*))})
  ,)

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
(defn detail-ui [this component-state $context context]
  (let [size (:size this)
        root (get component-state ::elem)
        $root (get this :$elem)

        selection (get component-state ::selection)
        $selection (get this :$selection)
        

        selection-id (first selection)
        path (when selection-id
               (elem-by-id selection-id))
        elem (when path
               (specter/select-one path root))
        $elem (when elem
                [$root (list 'path path)])

        state (-> this
                  (assoc :context
                         (-> context
                             (assoc :membrane.stretch/container-size (:size this))
                             (assoc :eval-ns (:eval-ns this))
                             (dissoc ::elem)))
                  (assoc :$context $context
                         :root root
                         :$root $root
                         :elem elem
                         :$elem $elem))]
    (ui/scissor-view
     [0 0]
     size
     (detail/editor state))))

(defrecord DetailApplet []
  model/IApplet
  (-start [{:keys [component-name]
            :as this}
           {:keys [$ref size $shared]}]
    
    (let [$component-state [$shared '(keypath ::components) (list 'keypath component-name)]
          $elem (conj $component-state '(keypath ::elem))
          $selection (conj $component-state '(keypath ::selection))
          this
          (assoc this
                 :$shared $shared
                 :extra {}
                 :$extra [$ref '(keypath :extra)]
                 :$elem $elem
                 :$selection $selection
                 ::easel/shared-keys [::components]
                 :$ref $ref
                 :size size)]
      (assoc this ::model/queue
             [(fn []
                (load-shared-component this))])))
  (-stop [this])
  model/IUI
  (-ui [{:keys [component-name] :as this} {:keys [$context context shared]}]
    (let [component-state (-> shared
                              ::components
                              (get component-name))]
      (detail-ui this component-state $context context)))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn detail-applet [handler component-name eval-ns]
  (-> (->DetailApplet)
      (assoc :label "Detail View"
             :dispatch! handler
             :component-name component-name
             :eval-ns eval-ns)))


(comment
  
  (com.phronemophobic.easel/handler
   :com.phronemophobic.easel/add-applet
   {:make-applet
    (fn [handler]
      (detail-applet
       handler
       `foo2
       *ns*))})
  


  ,)
