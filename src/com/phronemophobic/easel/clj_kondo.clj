(ns com.phronemophobic.easel.clj-kondo
  (:require [clojure.core.async :as async]
            [clj-kondo.core :as clj-kondo]
            [clojure.string :as str]
            [clojure.java.io :as io]
            clj-kondo.impl.config
            [membrane.ui :as ui]
            [membrane.basic-components :as basic]
            membrane.skia
            [membrane.skia.paragraph :as para]
            [membrane.alpha.component.drag-and-drop :as dnd]
            [membrane.component :refer [defui
                                        defeffect]]
            [com.phronemophobic.clobber.modes.text.ui :as text.ui]
            [com.phronemophobic.clobber.modes.text :as text-mode]
            [com.phronemophobic.clobber.util.ui.key-binding :as key-binding]
            [com.phronemophobic.easel.clobber :as-alias easel.clobber]
            [com.phronemophobic.membrandt.impl.grid :as grid]
            [com.phronemophobic.membrandt :as ant]
            [com.phronemophobic.membrandt.icon.ui :as icon.ui])
  (:import (java.util.regex Pattern
                            MatchResult)
           io.lacuna.bifurcan.Rope
           java.io.File))

(defn ^:private ->iter
  ^java.util.Iterator [o]
  (clojure.lang.RT/iter o))

(defn ^:private has-next? [^java.util.Iterator iter]
  (.hasNext iter))
(defn ^:private iter-next [^java.util.Iterator iter]
  (.next iter))


(defn ^:private ->queue 
  ([]
   clojure.lang.PersistentQueue/EMPTY)
  ([xs] (into clojure.lang.PersistentQueue/EMPTY xs)))

(defn ^:private truncate [s max-length]
  (if (< (count s) max-length)
    s
    (subs s 0 max-length)))

(def ^:private
  default-font-families
  ["Menlo"
   (membrane.skia/logical-font->font-family :monospace)])

(def ^:private
  default-text-style
  #:text-style {:font-families default-font-families
                :font-size 12
                :height 1.2
                :height-override true})

(def ^:private default-paragraph-style
  {:paragraph-style/text-style default-text-style})

(def ^:private
  default-text-style
  #:text-style {:font-families default-font-families
                :font-size 12
                :height 1.2
                :height-override true})

(defn ^:private file-tree [f]
  (tree-seq 
   #(-> % File/.listFiles seq)
   #(-> % File/.listFiles seq)
   (io/file f)))

(defn ^:private relativize
  "Returns a relative path as a string from `root` to `f`.
  
  If `root` does not contain `f`, then return the absolute path of `f`."
  [^File root ^File f]
  (let [directory? (File/.isDirectory f)
        original-file f
        f (File/.getCanonicalFile f)
        canonical-root (File/.getCanonicalPath root)]
    (loop [parts (if directory?
                   []
                   [(.getName f)])
           f (if directory?
               f
               (.getParentFile f))]
      (let [parent (File/.getParentFile f)
            parts (conj parts (.getName f))]
        (cond
          (nil? parent) (File/.getAbsolutePath original-file)

          (= (File/.getCanonicalPath parent)
             canonical-root)
          (str "./" (str/join "/" (reverse parts)))
          
          :else (recur parts
                       parent))))))


(comment
  (->> (clj-kondo/run! {:lint ["src"]
                        ;;:config {:linters all-linters}
                        })
       :findings
       
       (map :type)
       frequencies
       )
  
  (require 'clj-kondo.main)
  
  ,)

(defeffect ::start-editing [{:keys [$extra folder]}]
  (dispatch! :update $extra
             (fn [extra]
               (let [state {:search-editor (text.ui/make-editor {:source ""})
                            :folder folder}]
                 (assoc extra ::file-picker-state state)))))

(def ^:private user-home*
  (delay (System/getProperty "user.home")))
(defui folder-picker [{:keys [folder focused?] 
                       ::ui/keys [width]
                       :as this}]
  (let [file-picker-state (::file-picker-state extra)
        fp-folder (:folder file-picker-state)
        ^File current-folder (if file-picker-state
                               fp-folder
                               folder)

        search-editor (get file-picker-state :search-editor)
        editing? (some? file-picker-state)

        base-style default-text-style
        offset (get extra :offset 0)
        search-str (if search-editor
                     (.toString ^Rope (:rope search-editor))
                     "")
        search-str-lower (str/lower-case search-str)
        
        fs (into 
            []
            (comp (filter (fn [^File f]
                            (File/.isDirectory f)))
                  (remove (fn [^File f]
                            (let [fname (.getName f)]
                              (or (and (str/starts-with? fname ".")
                                       (not (str/starts-with? search-str-lower ".")))
                                  (and (str/starts-with? fname "#")
                                       (not (str/starts-with? search-str-lower "#")))))))
                  (remove (fn [^File f]
                            (let [fname (.getName f)]
                              (and (seq search-str-lower)
                                   (not (str/includes? (str/lower-case fname)
                                                       search-str-lower))))))
                  (drop offset))
            (sort (.listFiles current-folder)))
        ps (into 
            [{:text (str (.getCanonicalPath current-folder) "/")
              :style (assoc base-style :text-style/color [0 0 0.843])}
             search-str
             " | "]
            (comp (map-indexed 
                   (fn [i ^File f]
                     (let [style (if (zero? i)
                                   (assoc base-style 
                                          :text-style/font-style
                                          {:font-style/weight :bold})
                                   base-style)]
                       (if (.isFile f)
                         {:style style
                          :text (.getName f)}
                         {:text (str (.getName f) "/")
                          :style (assoc style :text-style/color [0.909 0.2784 0.3411])}))))
                  (interpose " | "))
            fs)
        
        search-editor-body 
        (para/paragraph
         ps
         width
         {:paragraph-style/text-style base-style})
        
]
    (ui/on
     ::select-file
     (fn [m]
       (let [^File f (if (and (= search-str "~")
                              @user-home*)
                       (io/file @user-home*)
                       (first fs))]
         (when f
           [[:set $fp-folder f]
            [:update $search-editor (fn [editor]
                                      (-> editor
                                          (text-mode/editor-beginning-of-buffer)
                                          (text-mode/editor-set-string "")))]
            [:set $offset 0]])))
     ::next-offset
     (fn [m]
       [[:update $offset 
                       (fn [offset]
                         (if (<= (count fs) 1)
                           0
                           (inc offset)))]])
     ::backspace
     (fn [m]
       (if (= search-str "")
         (when-let [parent (-> current-folder
                               .getCanonicalFile
                               .getParentFile)]
           [[:set $fp-folder parent]
            [:set $offset 0]])
         [[:update $search-editor #'text-mode/editor-delete-backward-char]
          [:set $offset 0]]))
     ::goto-root
     (fn [m]
       (if (= search-str "")
         [[:set $fp-folder (io/file "/")]
          [:set $offset 0]]
         (let [^File 
               f (if (and (= search-str "~")
                          @user-home*)
                   (io/file @user-home*)
                   ;; else, check for exact match
                   
                   (let [exact-file (io/file current-folder search-str)]
                     (if (and (File/.exists exact-file)
                              (File/.isDirectory exact-file))
                       exact-file
                       ;; else use first match
                       (first fs))))]
           (when (and f (File/.isDirectory f))
             [[:set $fp-folder f]
              [:update $search-editor (fn [editor]
                                        (-> editor
                                            (text-mode/editor-beginning-of-buffer)
                                            (text-mode/editor-set-string "")))]
              [:set $offset 0]]))))
     ::kill-word-backward
     (fn [m]
       (if (= search-str "")
         (when-let [parent (-> current-folder
                               .getCanonicalFile
                               .getParentFile)]
           [[:set $fp-folder parent]
            [:set $offset 0]])
         [[:update $search-editor (fn [editor]
                                      (-> editor
                                          (text-mode/editor-beginning-of-buffer)
                                          (text-mode/editor-set-string "")))]
          [:set $offset 0]]))
     ::select-folder
     (fn [_]
       [[:set $folder (:folder file-picker-state)]
        [:update $extra dissoc ::file-picker-state]])
     ::cancel-search
     (fn [_]
       [[:update $extra dissoc ::file-picker-state]])
     ::update-search-editor
     (fn [{:keys [;; shadowing with $editor is buggy
                  ;; use $search-editor directly
                  #_$editor
                  op] :as m}]
       
       [[:set $offset 0]
        [:update $search-editor op]])
     (ui/vertical-layout
      (let [search-editor-body (if editing?
                                 (key-binding/wrap-editor-key-bindings
                                  {:body search-editor-body
                                   :$body nil
                                   :editor search-editor
                                   :$editor $search-editor
                                   :update-editor-intent ::text.ui/update-editor
                                   :key-bindings {"C-g" ::cancel-search
                                                  "RET" ::select-file
                                                  "M-DEL" ::kill-word-backward
                                                  "/" ::goto-root
                                                  "S-RET" ::select-folder
                                                  "C-s" ::next-offset
                                                  "DEL" ::backspace }})
                                 
                                 (ui/on
                                  :mouse-down
                                  (fn [_]
                                    [[::start-editing this]])
                                  :key-press
                                  (fn [s]
                                    (when (and focused? (= :enter s))
                                      [[::start-editing this]]))
                                  (para/paragraph
                                   [(File/.getCanonicalPath folder)]
                                   width
                                   {:paragraph-style/text-style base-style})))]
        search-editor-body)))))

(defui simple-editor [{:keys [editor]}]
  (let [body (text.ui/editor-view {:editor editor
                                   :focused? true})
        
        body (key-binding/wrap-editor-key-bindings 
              {:key-bindings (:key-bindings editor)
               :editor editor
               :update-editor-intent ::text.ui/update-editor
               :body body
               :$body nil})
        
        body (ui/wrap-on
              :mouse-down
              (fn [handler mpos]
                (let [intents (handler mpos)]
                  (cons [:com.phronemophobic.clobber.modes.clojure.ui/request-focus]
                        intents)))
              body)
        
        body (ui/on-clipboard-copy
                (fn []
                  [[::editor-copy {:editor editor
                                   :$editor $editor}]])
                (ui/on-clipboard-paste
                 (fn [s]
                   [[::editor-paste {:editor editor
                                     :$editor $editor
                                     :s s}]])
                 
                 body))]
    [(ui/spacer 200 20)
     body]))


(defn kondo-search-loop
  "Starts a thread that can run clj-kondo. Returns a channel."
  []
  (let [ch (async/chan (async/sliding-buffer 1))]
    (async/thread
     (try
       (loop [{:keys [$search-state results root files dispatch!] :as state} nil]
         (let [msg (if files
                     (async/poll! ch)
                     (async/<!! ch))]
           
           (cond
             
             msg
             (case (:op msg)
               :quit nil
               
               :stop (recur nil)
               
               :search (recur (assoc msg :results [])))
             
             ;; ch is closed
             (and (nil? files) (nil? msg)) nil
             
             ;; else we expect files
             :else
             (do
               (when-not files
                 (throw (ex-info "Expected to have files"
                                 {:files files})))
               
               (let [f (first files)
                     relative-path (relativize root f)
                     {:keys [summary]} (clj-kondo/run! {:lint [f]} )
                     result (assoc summary
                                   :file f
                                   :relative-path relative-path)
                     
                     results (conj results result)]
                 (dispatch! :update $search-state
                            assoc :results results)
                 
                 (recur (assoc state
                               :results results
                               :files (next files)))))
             
             )))
       (catch Throwable t
         (tap> t))))
    
    ch
    ))

(defeffect ::refresh-kondo [{:keys [search-state $search-state folder file-regexp-editor kondo-chan]}]
  (let [file-regexp-str (-> file-regexp-editor :rope str)
        file-regexp-value (Pattern/compile file-regexp-str)
        
        files (->> (file-tree folder)
                   (filter #(File/.isFile %))
                   (filter #(re-matches file-regexp-value (-> % File/.getName))))]
    (async/put! kondo-chan
                {:op :search
                 :root folder
                 :files (seq files)
                 :dispatch! dispatch!
                 :$search-state $search-state})))

(defeffect ::stop-search [{:keys [kondo-chan]}]
  (async/put! kondo-chan
              {:op :stop}))


(defui kondo-ui [{:keys [folder
                         file-regexp-editor
                         search-state
                         ]
                  :as this}]
  (let [focus (:focus context)
        folder-picker-focus-id [$folder ::focus]
        
        file-regexp-editor-focus-id [$file-regexp-editor ::focus]
        file-regexp-editor-focused? (= focus file-regexp-editor-focus-id)

        [cw ch] (:membrane.stretch/container-size context)

        results (:results search-state)
        search-id (:search-id search-state)
        *results (when results
                   results)
        result-count (when results
                       (count results))
        
        header
        (ui/vertical-layout
         (ui/horizontal-layout
          
          (ui/on-click
           (fn []
             [[::refresh-kondo this]])
           (icon.ui/icon {:name "search"}))
          (ui/on-click
           (fn []
             [[::stop-search {:$ref $search-state}]])
           (icon.ui/icon {:name "stop"}))
          (ui/on-click
           (fn []
             [[::clear-results {:$ref $search-state}]])
           (icon.ui/icon {:name "delete"})))
         (ui/label "files")
         (let [editor (simple-editor {:editor file-regexp-editor
                                      :focused? true})]
           (if file-regexp-editor-focused?
             (ui/on
              ::focus-next
              (fn [_]
                [[:com.phronemophobic.easel/request-focus folder-picker-focus-id]])
              editor)
             (ui/on-mouse-down
              (fn [_]
                [[:com.phronemophobic.easel/request-focus file-regexp-editor-focus-id]])
              (ui/fill-bordered [0.9 0.9 0.9  ]
                                2
                                (ui/no-events editor)))))

         (ui/label "file root")
         (let [focused? (= focus folder-picker-focus-id)
               folder-picker-ui (folder-picker {:folder folder
                                                :focused? focused?})]
           (if focused?
             folder-picker-ui
             (ui/on-mouse-down
              (fn [_]
                [[:com.phronemophobic.easel/request-focus folder-picker-focus-id]])
              (ui/fill-bordered [0.9 0.9 0.9]
                                2
                                (ui/no-events folder-picker-ui)))))
         

         (ui/on
          :mouse-down
          (fn [_]
            [[::dnd/drag-start {::dnd/obj {:x results}}]])
          (ui/label 
           (let [lbl (if (= :running (:state search-state))
                       "Results..."
                       "Results")]
             (if results
               (str lbl " (" result-count ")")
               lbl)))))
        
        
        results-view
        (when results
          (grid/list-view
           {:row-fn (fn [{:keys [row]}]
                      (let [extra (get extra [::inspector search-id row])
                            {:keys [file relative-path error warning]} (nth results row)
                            body (ui/padding
                                  3
                                  (para/paragraph
                                   [relative-path
                                    " "
                                    (str error)
                                    " "
                                    (str warning)]
                                   cw
                                   default-paragraph-style))
                            hover? (get extra ::hover)
                            body (if hover?
                                   (ui/fill-bordered [0.9 0.9 0.9  ] 0 body)
                                   body)
                            body (ui/on
                                  :mouse-down
                                  (fn [_]
                                    [[::easel.clobber/open-editor {:file file}]])
                                  body)]
                        (basic/on-hover {:hover? hover?
                                         :body body})
                        
                        ))
            :num-rows result-count
            :width cw
            :height (- ch (ui/height header) 20)
            :extra (get extra [:results search-id])}))]
    (ui/vertical-layout
     header
     results-view)))

(comment
  
  run!
  ;; The configurations are merged in the following order, where a later config overrides an earlier config:
  ;;   home dir config
  ;;   :config-paths in project config
  ;;   project config
  ;;   CLJ_KONDO_EXTRA_CONFIG_DIR environment variable
  ;;   command line config
  ;;   namespace local config

  
  ,)

(defn kondo-ui-init-state []
  {:folder (io/file ".")
   :kondo-chan (kondo-search-loop)
   :file-regexp-editor (-> (text.ui/make-editor {:source ".*(\\.clj|\\.cljs|\\.cljc)"})
                           (update :key-bindings
                                   assoc "TAB" ::focus-next))
   
   :search-state nil})

(defeffect ::show-kondo [_]
  (dispatch! :com.phronemophobic.easel/add-component-as-applet
             #'kondo-ui
             (kondo-ui-init-state)))

(defn show-kondo []
  ((requiring-resolve 'dev/add-component-as-applet)
   #'kondo-ui
   (kondo-ui-init-state)))

(comment
  (show-kondo )
  ,)