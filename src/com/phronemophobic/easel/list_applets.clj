(ns com.phronemophobic.easel.list-applets
  (:require
   [com.phronemophobic.easel.model :as model]
   [membrane.basic-components :as basic]
   [com.phronemophobic.membrandt :as ant]
   [com.phronemophobic.membrandt.icon.ui :as icon.ui]
   [membrane.skia.paragraph :as para]
   [membrane.component
    :refer [defui defeffect]]
   [membrane.ui :as ui]
))



(defui button [{:keys [text on-click]}]
  (ui/flex-layout
   [(ant/button {:text text
                 :size :small
                 :on-click on-click})
    (ui/on
     :com.phronemophobic.easel/add-applet
     (fn [m]
       [[:com.phronemophobic.easel/add-applet (assoc m :pop-out? true)]])
     (ui/on
      :mouse-down
      (fn [_]
        (on-click))
      (icon.ui/icon {:name "export"})
      #_(ant/button {:text "\u2197"
                  :size :small
                  :on-click on-click})))]
   {:gap 5
    :align :center}))

(defui list-applets [{:keys [shared-state]}]
  (let [url (get shared-state ::url "https://duckduckgo.com")
        eval-ns-name (get shared-state ::eval-ns-name "user")
        eval-ns* (fn []
                   (try
                     (let [ns-sym (symbol eval-ns-name)]
                       (require ns-sym)
                       (the-ns ns-sym))
                     (catch Exception e
                       nil)))]
    (ui/translate 
     10 5
     (ui/flex-layout
      [(ui/flex-layout
       [(para/paragraph
         "eval-ns: "
         nil
         #:paragraph-style
         {:text-style (ant/text-input-text-style {:size :small})})
        (ant/text-input {:text eval-ns-name
                         :size :small})]
       {:gap 8
        :align :center})
      (button {:text "Clojure Editor"
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-applet
                   {:make-applet
                    (let [f (requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)]
                      #(f % {:source ""
                             :mode :clojure
                             :eval-ns (the-ns 'user)}))}]])})
       (button {:text "Org Editor"
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-applet
                   {:make-applet
                    (let [f (requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)]
                      #(f % {:source ""
                             :mode :org}))}]])})
      (button {:text "Text Editor"
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-applet
                   {:make-applet
                    (let [f (requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)]
                      #(f % {:source ""}))}]])})
      (button {:text "Clobber Editor"
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-applet
                   {:make-applet
                    (let [f (requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)]
                      #(f % {:ns 'com.phronemophobic.clobber.modes.clojure.ui}))}]])})
       (button {:text "Tap Watcher"
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-applet
                   {:id @(requiring-resolve 'com.phronemophobic.easel.tap-watcher/id)
                    :make-applet
                    (fn [handler]
                      ((requiring-resolve 'com.phronemophobic.easel.tap-watcher/tap-watcher-applet)
                       handler))}]])})
       (button {:text "Add Term"
                :on-click
                (fn []
                  [[:com.phronemophobic.easel/add-applet
                    {:make-applet (requiring-resolve 'com.phronemophobic.easel.term/termlet)}]])})
       (button {:text "Search"
                :on-click
                (fn []
                  [[:com.phronemophobic.easel/add-applet
                    {:make-applet
                     (fn [handler]
                       ((requiring-resolve 'com.phronmophobic.inquery/search-applet) handler {:query ""}))}]])})
      
      (ui/horizontal-layout
       (button {:text "Add browser"
                :on-click
                (fn []
                  [[:com.phronemophobic.easel/add-applet
                    {:make-applet
                     (fn [handler]
                       ((requiring-resolve 'com.phronemophobic.easel.browser/browslet)
                        handler
                        url))}]])})
       #_(ant/text-input {:text url
                        :size :small})
       #_(basic/textarea {:text url}))
      #_(button {:text "Add Schematic"
                 ;; :hover? (get applet [::hover?])
                 :on-click
                 (fn []
                   [[:com.phronemophobic.easel/add-applet
                     {:make-applet
                      (requiring-resolve 'com.phronemophobic.easel.schematic/schematlet)}]])})
      (button {:text "Add Spreadsheet"
               ;; :hover? (get applet [::hover?])
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-applet
                   {:make-applet
                    (requiring-resolve 'com.phronemophobic.easel.spreadsheet/spreadsheet-applet)}]])})
      #_(button {:text "Add Preview"
                 ;; :hover? (get applet [::hover?])
                 :on-click
                 (fn []
                   [[:com.phronemophobic.easel/add-applet
                     {:make-applete
                      (requiring-resolve 'com.phronemophobic.easel.ui/schema-preview)}]])})
      
      (button {:text "Toolbar"
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-applet
                   {:make-applet
                    (fn [handler]
                      ((requiring-resolve 'com.phronemophobic.easel.schematic2/toolbar-applet)
                       handler
                       (eval-ns*)))}]])})
      (button {:text "Component Picker"
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-applet
                   {:make-applet
                    (requiring-resolve 'com.phronemophobic.easel.schematic2/component-picker-applet)}]])})
      (button {:text "Tree View"
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-applet
                   {:make-applet
                    (fn [handler]
                      ((requiring-resolve 'com.phronemophobic.easel.schematic2/tree-applet)
                       handler
                       (eval-ns*)))}]])})
      (button {:text "Preview"
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-applet
                   {:make-applet
                    (fn [handler]
                      ((requiring-resolve 'com.phronemophobic.easel.schematic2/preview-applet)
                       handler
                       (eval-ns*)))}]])})
      (button {:text "Detail View"
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-applet
                   {:make-applet
                    (fn [handler]
                      ((requiring-resolve 'com.phronemophobic.easel.schematic2/detail-applet)
                       handler
                       (eval-ns*)))}]])})
      
      (button {:text "NS Watcher"
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-applet
                   {:make-applet
                    (fn [handler]
                      ((requiring-resolve 'com.phronemophobic.easel.ns-watcher/ns-watcher-applet)
                       handler
                       (eval-ns*)))}]])})
       
      (button {:text "Derpbot"
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-applet
                   {:make-applet
                    (fn [handler]
                      ((requiring-resolve 'com.phronemophobic.easel.derpbot/derpbot-applet)
                       handler
                       (random-uuid)))}]])})
      #_(button {:text "Mic"
                 ;; :hover? (get applet [::hover?])
                 :on-click
                 (fn []
                   [[:com.phronemophobic.easel/add-applet
                     {:make-applet
                      (fn [handler]
                        ((requiring-resolve 'com.phronemophobic.easel.mic/mic-applet)
                         handler
                         (random-uuid)))}]])})
      (button {:text "Super Mario Bros"
               ;; :hover? (get applet [::hover?])
               :on-click
               (fn []
                 [[:com.phronemophobic.bowsertalk.retro-api/open-game
                   {:save/game-path
                    @(requiring-resolve 'com.phronemophobic.bowsertalk.retro-api/mario-game-path)}]])})
      (button {:text "Show Slides"
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-component-as-applet
                   (requiring-resolve
                    'com.phronemophobic.easel.slides/show-picker)
                   {}
                   ]])})
      (button {:text "File List"
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-component-as-applet
                   (requiring-resolve
                    'com.phronemophobic.bowsertalk.retro-api/file-list)
                   {:folder 
                    (clojure.java.io/file "roms")}
                   ]])})
      (button {:text "Save list"
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-component-as-applet
                   (requiring-resolve
                    'com.phronemophobic.bowsertalk.retro-api/save-list)
                   {}]])})
      (button {:text "Input Viewer"
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-component-as-applet
                   (requiring-resolve
                    'com.phronemophobic.bowsertalk.retro-api/input-viewer)
                   {}]])})
      (button {:text "Flow "
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-applet
                   {:make-applet
                    (fn [handler]
                      ((requiring-resolve 'com.phronemophobic.easel.flow/flow-applet)
                       handler))}]])})
      #_(button {:text "Klein Bottle"
                 ;; :hover? (get applet [::hover?])
                 :on-click
                 (fn []
                   [[:com.phronemophobic.easel/add-applet
                     {:make-applet
                      (fn [handler]
                        ((requiring-resolve 'com.phronemophobic.easel.d3/d3-applet)
                         handler))}]])})
      ]
      {:gap 5
       :direction :column}))))
