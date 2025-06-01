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
  (ui/horizontal-layout
   (ant/button {:text text
                :size :small
                :on-click on-click})
   (ui/on
    :com.phronemophobic.easel/add-applet
    (fn [m]
      [[:com.phronemophobic.easel/add-applet (assoc m :pop-out? true)]])
    (ant/button {:text "\u2197"
                 :size :small
                 :on-click on-click}))))

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
     (button {:text "Clobber Editor"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  {:make-applet
                   (let [f (requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)]
                     #(f % {:ns 'com.phronemophobic.clobber.modes.clojure.ui}))}]])})
     (button {:text "Add Term"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  {:make-applet (requiring-resolve 'com.phronemophobic.easel.term/termlet)}]])})

     (ui/horizontal-layout
      (button {:text "Add browser"
               ;; :hover? (get applet [::hover?])
               :on-click
               (fn []
                 [[:com.phronemophobic.easel/add-applet
                   {:make-applet
                    (fn [handler]
                      ((requiring-resolve 'com.phronemophobic.easel.browser/browslet)
                       handler
                       url))}]])})
      (ant/text-input {:text url
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
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  {:make-applet
                   (fn [handler]
                     ((requiring-resolve 'com.phronemophobic.easel.schematic2/toolbar-applet)
                      handler
                      (eval-ns*)))}]])})
     (button {:text "Component Picker"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  {:make-applet
                   (requiring-resolve 'com.phronemophobic.easel.schematic2/component-picker-applet)}]])})
     (button {:text "Tree View"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  {:make-applet
                   (fn [handler]
                     ((requiring-resolve 'com.phronemophobic.easel.schematic2/tree-applet)
                      handler
                      (eval-ns*)))}]])})
     (button {:text "Preview"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  {:make-applet
                   (fn [handler]
                     ((requiring-resolve 'com.phronemophobic.easel.schematic2/preview-applet)
                      handler
                      (eval-ns*)))}]])})
     (button {:text "Detail View"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  {:make-applet
                   (fn [handler]
                     ((requiring-resolve 'com.phronemophobic.easel.schematic2/detail-applet)
                      handler
                      (eval-ns*)))}]])})

     (button {:text "NS Watcher"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  {:make-applet
                   (fn [handler]
                     ((requiring-resolve 'com.phronemophobic.easel.ns-watcher/ns-watcher-applet)
                      handler
                      (eval-ns*)))}]])})
     (button {:text "Tap Watcher"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  {:make-applet
                   (fn [handler]
                     ((requiring-resolve 'com.phronemophobic.easel.tap-watcher/tap-watcher-applet)
                      handler))}]])})
     (button {:text "Derpbot"
              ;; :hover? (get applet [::hover?])
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
     #_(button {:text "Actual Tetris"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  {:make-applet
                   (fn [handler]
                     ((requiring-resolve 'com.phronemophobic.easel.emulator/emulator-applet)
                      handler))}]])})
     #_(button {:text "Learn Clojure in 30 minutes"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  {:make-applet
                   (fn [handler]
                     ((requiring-resolve 'com.phronemophobic.easel.video/video-applet)
                      handler
                      "Simplicity\nFirehose"
                      "/var/tmp/firehose/firehose.mp4"))}]])})
     #_(button {:text "Steamboat Willie"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  {:make-applet
                   (fn [handler]
                     ((requiring-resolve 'com.phronemophobic.easel.video/video-applet)
                      handler
                      "Steamboat Willie"
                      "steamboat-willie.mp4"))}]])})
     #_(button {:text "Klein Bottle"
              ;; :hover? (get applet [::hover?])
              :on-click
              (fn []
                [[:com.phronemophobic.easel/add-applet
                  {:make-applet
                   (fn [handler]
                     ((requiring-resolve 'com.phronemophobic.easel.d3/d3-applet)
                      handler))}]])})
     )))
