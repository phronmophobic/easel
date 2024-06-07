(ns com.phronemophobic.easel.list-applets
  (:require
   [com.phronemophobic.easel.model :as model]
   [membrane.basic-components :as basic]
   [com.phronemophobic.membrandt :as ant]
   [membrane.skia.paragraph :as para]
   [membrane.component
    :refer [defui defeffect]]
   [membrane.ui :as ui]
))



(defui button [{:keys [text on-click]}]
  (ant/button {:text text
               :size :small
               :on-click on-click}))

(defui list-applets-ui [{:keys [applets url $ref]}]
  (let [eval-ns-name (get extra ::eval-ns-name "sevengui")
        eval-ns* (fn []
                   (try
                     (let [ns-sym (symbol eval-ns-name)]
                       (require ns-sym)
                       (the-ns ns-sym))
                     (catch Exception e
                       nil)))]
    (ui/vertical-layout
     (ui/flex-layout
      [(para/paragraph
        "eval-ns: "
        nil
        #:paragraph-style
        {:text-style (ant/text-input-text-style {:size :small})})
       (ant/text-input {:text eval-ns-name
                        :size :small})]
      {:gap 8
       :align :center})
     (button {:text "Add Term"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  (requiring-resolve 'com.phronemophobic.easel.term/termlet)]])})

     (ui/horizontal-layout
      (button {:text "Add browser"
               ;; :hover? (get applet [::hover?])
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-applet
                   (fn [handler]
                     ((requiring-resolve 'com.phronemophobic.easel.browser/browslet)
                      handler
                      url))]])})
      (ant/text-input {:text url
                       :size :small})
      #_(basic/textarea {:text url}))
     #_(button {:text "Add Schematic"
                ;; :hover? (get applet [::hover?])
                :on-click
                (fn []
                  [[:com.phronemophobic.easel/add-applet
                    (requiring-resolve 'com.phronemophobic.easel.schematic/schematlet)]])})
     (button {:text "Add Spreadsheet"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  (requiring-resolve 'com.phronemophobic.easel.spreadsheet/spreadsheet-applet)]])})
     #_(button {:text "Add Preview"
                ;; :hover? (get applet [::hover?])
                :on-click
                (fn []
                  [[:com.phronemophobic.easel/add-applet
                    (requiring-resolve 'com.phronemophobic.easel.ui/schema-preview)]])})
     
     (button {:text "Toolbar"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  (fn [handler]
                    ((requiring-resolve 'com.phronemophobic.easel.schematic2/toolbar-applet)
                     handler
                     (eval-ns*)))]])})
     (button {:text "Component Picker"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  (requiring-resolve 'com.phronemophobic.easel.schematic2/component-picker-applet)]])})
     (button {:text "Tree View"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  (fn [handler]
                    ((requiring-resolve 'com.phronemophobic.easel.schematic2/tree-applet)
                     handler
                     (eval-ns*)))]])})
     (button {:text "Preview"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  (fn [handler]
                    ((requiring-resolve 'com.phronemophobic.easel.schematic2/preview-applet)
                     handler
                     (eval-ns*)))]])})
     (button {:text "Detail View"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  (fn [handler]
                    ((requiring-resolve 'com.phronemophobic.easel.schematic2/detail-applet)
                     handler
                     (eval-ns*)))]])})

     (button {:text "NS Watcher"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  (fn [handler]
                    ((requiring-resolve 'com.phronemophobic.easel.ns-watcher/ns-watcher-applet)
                     handler
                     (eval-ns*)))]])})
     (button {:text "Tap Watcher"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  (fn [handler]
                    ((requiring-resolve 'com.phronemophobic.easel.tap-watcher/tap-watcher-applet)
                     handler))]])})
     )))



(defrecord ListApplets [dispatch!]
  model/IApplet
  (-start [this $ref size]
    (assoc this
           :$ref $ref
           :$extra [$ref (list 'keypath :extra)]
           :$context [$ref (list 'keypath :context)]
           :url "https://eugenkiss.github.io/7guis/tasks/"
           :$url [$ref (list 'keypath :url)]
           :size size
           :$applets
           [$ref (list 'keypath :applets)]))
  (-stop [this])
  (-ui [this $context context]
    (ui/scissor-view
     [0 0]
     (:size this)
     (list-applets-ui
      (assoc this
             :context context
             :$context $context))))
  model/IResizable
  (-resize [this [w h] _content-scale]
    (assoc this
           :size [w h])))

(defn list-applets [handler]
  (-> (->ListApplets handler)
      ;; unused
      ;; UI is currently custom
      #_(assoc :applets
               [{:name "term"
                 :make-applet (requiring-resolve 'com.phronemophobic.easel.term/termlet)}
                {:name "browser"
                 :make-applet (requiring-resolve 'com.phronemophobic.easel.browser/browslet)}
                {:name "schemalet"
                 :make-applet (requiring-resolve 'com.phronemophobic.easel.schematic/schematlet)}])
      (assoc :label "list-applets")))
