(ns com.phronemophobic.easel.fidgit
  (:require 
   [clojure.java.io :as io]
   [clojure.string :as str]
   [membrane.ui :as ui]
   [membrane.skia.paragraph :as para]
   [membrane.component.present :as present]
   [membrane.basic-components :as basic]
   [membrane.alpha.component.drag-and-drop :as dnd]
   [clojure.datafy :as datafy]
   [com.phronemophobic.viscous :as viscous]
   [com.phronemophobic.clobber.modes.clojure.ui :as cui]
   [com.phronemophobic.clobber.editor :as clobber.editor]
   [com.phronemophobic.clobber.modes.text :as clobber.text]
   [com.phronemophobic.clobber.modes.text.ui :as text.ui]
   [com.phronemophobic.clobber.util.ui :as clobber.util.ui]
   [com.phronemophobic.clobber.util :as clobber.util]
   
   [com.phronemophobic.membrandt.icon.ui :as icon.ui]
   [com.phronemophobic.membrandt.impl.grid :as grid]
   [com.phronemophobic.easel.model :as model]
   [membrane.component :refer [defui defeffect]])
  
  (:import java.io.File
           java.util.HashSet
           java.nio.file.Path
           com.github.difflib.DiffUtils

           io.lacuna.bifurcan.Rope
           org.eclipse.jgit.storage.file.FileRepositoryBuilder

           [java.io ByteArrayOutputStream File]
           [org.eclipse.jgit.api Git]
           [org.eclipse.jgit.lib Repository ObjectInserter Constants FileMode]
           [org.eclipse.jgit.dircache DirCache DirCacheEditor DirCacheEditor$PathEdit DirCacheEntry]
           [org.eclipse.jgit.treewalk TreeWalk]
           [org.eclipse.jgit.treewalk.filter PathFilter]
           
           (com.github.difflib.text
            DiffRowGenerator
            DiffRowGenerator$Builder
            DiffRow)
           (com.github.difflib
            DiffUtils)
           (com.github.difflib.patch
            AbstractDelta
            InsertDelta DeleteDelta ChangeDelta
            Chunk
            DeltaType
            Patch)
           
           io.lacuna.bifurcan.Rope))

;; Notes
;; The current work is ok a first pass, but could definitely be cleaned up and refactored
;; The side by side and unified diffs could be easier to use a generic components
;; Maybe they should move to their own namespace at some point.
;; Additionally, the API should be updated to do a better job
;; of considering which parts should be public vs private.

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

(defn ^:private count-newlines
  "Count newlines from `char-index` to the editors current cursor position"
  ^long [^String s]
  (let [end (String/.length s)]
    (loop [char-index 0
           cnt 0]
      (if (>= char-index end)
        cnt
        (recur (inc char-index)
               (if (= \newline (.charAt s char-index))
                 (inc cnt)
                 cnt))))))

(defn ^:private relative-path
  "Returns `child` path relative to `parent` as a String."
  [parent child]
  (let [parent-path (File/.toPath (File/.getCanonicalFile (io/file parent)))
        child-path (File/.toPath (File/.getCanonicalFile (io/file child)))
        rel (Path/.relativize parent-path child-path)]
    (str rel)))

(defmacro with-git [[git fname] & body]
  `(let [repo# (.build
                (doto (FileRepositoryBuilder.)
                  (FileRepositoryBuilder/.findGitDir (File/.getCanonicalFile
                                                      (io/file ~fname)))))]
     (try
       (let [~git (Git/new repo#)]
         ~@body)
       (finally
         (java.lang.AutoCloseable/.close repo#)))))

(defn get-git-work-tree [path]
  (let [path (io/file path)]
    (with-git
     [git path]
     (let [^Repository repo (.getRepository git)
           work-tree (Repository/.getWorkTree repo)]
       work-tree))))

(defn changed-files
  "Returns a map of the individual status sets plus :all (their union).
   repo-dir can be the repo root (the folder that contains .git)."
  [repo-dir]
  (with-git
   [git repo-dir]
   (let [status (.call (.status git))
         modified    (.getModified status)
         changed     (.getChanged status)
         added       (.getAdded status)
         removed     (.getRemoved status)
         missing     (.getMissing status)
         untracked   (.getUntracked status)
         conflicting (.getConflicting status)
         all (doto (HashSet.)
               (.addAll modified)
               (.addAll changed)
               (.addAll added)
               (.addAll removed)
               (.addAll missing)
               (.addAll untracked)
               (.addAll conflicting))]
     {:modified    (set modified)
      :changed     (set changed)
      :added       (set added)
      :removed     (set removed)
      :missing     (set missing)
      :untracked   (set untracked)
      :conflicting (set conflicting)
      :all         (set all)})))

(comment
  
  (changed-files ".")
  ,)

(defn file-contents [path]
  (slurp (io/file path)))

(defn head-contents
  "Returns the HEAD version of path, or nil if not present in HEAD."
  [path]
  
  (let [path (io/file path)]
    (with-git
     [git path]
     (let [^Repository repo (.getRepository git)
           headTreeId (.resolve repo "HEAD^{tree}")
           work-tree (Repository/.getWorkTree repo)]
       (with-open [tw (doto (TreeWalk. repo)
                        (.addTree headTreeId)
                        (.setRecursive true)
                        (.setFilter (PathFilter/create (relative-path work-tree path))))]
         (when (.next tw)
           (let [loader (.open repo (.getObjectId tw 0))
                 out (ByteArrayOutputStream.)]
             (.copyTo loader out)
             (.toString out "UTF-8"))))))))

(comment

  (head-contents (File/.getCanonicalFile (io/file "src/com/phronemophobic/easel.clj")))
  
  ,)

(defn index-contents
  "Returns the staged (index) version of path, or nil if not in index."
  [path]
  
  (let [path (io/file path)]
    (with-git
     [git path]
     (let [^Repository repo (.getRepository git)
           ^DirCache dc (DirCache/read repo)
           work-tree (Repository/.getWorkTree repo)
           entry (.getEntry dc (relative-path work-tree path))]
       (when entry
         (let [loader (.open repo (.getObjectId entry))
               out (ByteArrayOutputStream.)]
           (.copyTo loader out)
           (.toString out "UTF-8")))))))

(comment
  (index-contents "src/com/phronemophobic/easel.clj")
  ,)


(defn split-lines 
  "Similar to str/split-lines, but does not omit trailing empty lines."
  [s]
  (java.util.Arrays/asList (String/.split s "\r?\n" -1))
  )

(defn unstaged-deltas2 [path]
  (-> (DiffUtils/diff (split-lines (index-contents path))
                      (split-lines (file-contents path)))
      Patch/.getDeltas))

(defn datafy-chunk [chunk]
  {:change-position (Chunk/.getChangePosition chunk)
   :lines (Chunk/.getLines chunk)
   :position (Chunk/.getPosition chunk)
   :last (Chunk/.last chunk)
   :size (Chunk/.size chunk)
   :str (str chunk)})

(defn datafy-delta [delta]
  {:source (datafy-chunk (AbstractDelta/.getSource delta))
   :target (datafy-chunk (AbstractDelta/.getTarget delta))
   :type (str (AbstractDelta/.getType delta))})


(defn add-unhighlighted [editor text line]
  (let [start-cursor (:cursor editor) 
        start-byte (:byte start-cursor)
        
        editor (clobber.text/editor-goto-line editor line)
        end-byte (-> editor :cursor :byte)
        
        extent {:start-byte-offset start-byte
                :end-byte-offset end-byte}
        chunk-text (clobber.util.ui/styled-text (:rope editor)
                                                (:base-style editor)
                                                [(if (= (::mode editor) :clojure)
                                                   (cui/syntax-style editor extent)
                                                   (when (:tree editor)
                                                     (when-let [query (:query editor)]
                                                       (when-let [theme (:theme editor)]
                                                         (clobber.util.ui/syntax-style editor
                                                                                       query
                                                                                       theme
                                                                                       extent)))))]
                                                start-byte
                                                end-byte)]
    [editor (into text chunk-text)]))

(defn add-chunk [editor text highlights highlight-offset chunk s color other-text other-highlight-offset]
  (let [start-cursor (:cursor editor) 
        start-byte (:byte start-cursor)
        start-char (:char start-cursor)
        
        editor (-> editor
                   (clobber.text/editor-goto-line (:position chunk))
                   (clobber.text/editor-forward-char (String/.length s)))

        end-byte (-> editor :cursor :byte)
        
        extent {:start-byte-offset start-byte
                :end-byte-offset end-byte}
        chunk-text (clobber.util.ui/styled-text (:rope editor)
                                                (:base-style editor)
                                                [(if (= (::mode editor) :clojure)
                                                   (cui/syntax-style editor extent)
                                                   (when (:tree editor)
                                                     (when-let [query (:query editor)]
                                                       (when-let [theme (:theme editor)]
                                                         (clobber.util.ui/syntax-style editor
                                                                                       query
                                                                                       theme
                                                                                       extent)))))]
                                                start-byte
                                                end-byte)

        highlights (conj highlights {:index (+ start-char highlight-offset)
                                     :len (String/.length s)
                                     :delta (:delta chunk) 
                                     :color color})
        num-newlines (count-newlines s)]
    [editor
     (into text chunk-text)
     highlights
     (if (pos? num-newlines)
       (conj other-text (str/join (repeat num-newlines \newline)))
       other-text)
     (+ other-highlight-offset num-newlines)]))



(defn highlight-file2 [fname]
  (let [deltas (unstaged-deltas2 fname)
        source-str (index-contents fname)
        target-str (file-contents fname)
        editor-mode (clobber.editor/guess-mode {:file (io/file fname)})
        
        source-editor (-> (clobber.editor/make-editor {:source source-str
                                                       :mode editor-mode})
                          (assoc ::mode editor-mode))
        target-editor (-> (clobber.editor/make-editor {:source target-str
                                                       :mode editor-mode})
                          (assoc ::mode editor-mode))
        
        delete-color [1 0 0 0.2]
        insert-color [0 1 0 0.2]

        [source-text source-highlights target-text target-highlights]
        (loop [source-editor source-editor
               target-editor target-editor
               source-text []
               source-highlights []
               source-highlight-offset 0
               target-text []
               target-highlights []
               target-highlight-offset 0
               deltas (seq deltas)]
          (if deltas
            (let [delta (first deltas)
                  {:keys [type target source]} (datafy-delta delta)
                  
                  [source-editor source-text] (add-unhighlighted source-editor source-text (:position source))
                  [target-editor target-text] (add-unhighlighted target-editor target-text (:position target))

                  ;; add source
                  [source-editor source-text source-highlights target-text target-highlight-offset]
                  (if (seq (:lines source))
                    (let [s (str/join
                             (eduction
                              (map #(str % \newline))
                              (:lines source)))]
                      (add-chunk source-editor source-text source-highlights source-highlight-offset (assoc source :delta delta) s delete-color target-text target-highlight-offset))
                    ;; else
                    [source-editor source-text source-highlights target-text target-highlight-offset])
                  
                  ;; add target
                  [target-editor target-text target-highlights source-text source-highlight-offset]
                  (if (seq (:lines target))
                    (let [s (str/join
                             (eduction
                              (map #(str % \newline))
                              (:lines target)))]
                      (add-chunk target-editor target-text target-highlights target-highlight-offset (assoc target :delta delta) s insert-color source-text source-highlight-offset))
                    ;; else
                    [target-editor target-text target-highlights source-text source-highlight-offset])]
              (recur source-editor target-editor source-text source-highlights source-highlight-offset target-text target-highlights target-highlight-offset (next deltas)))
            ;; else
            (let [;; add whatever is at the bottom
                  [source-editor source-text] (add-unhighlighted source-editor source-text (Rope/.length (:rope source-editor)))
                  [target-editor target-text] (add-unhighlighted target-editor target-text (Rope/.length (:rope target-editor)))]
              [source-text source-highlights target-text target-highlights])))

        map-add-view (fn [para]
                       (map
                        (fn [{:keys [index len color] :as m}]
                          (let [rects (para/get-rects-for-range para 
                                                                index
                                                                (+ index len)
                                                                :max :tight)
                                view (into []
                                           (map (fn [{:keys [x y width height]}]
                                                  (->> (ui/translate
                                                        x y
                                                        (ui/rectangle (max 4 width) height))
                                                       (ui/with-style ::ui/style-fill)
                                                       (ui/with-color color))))
                                           
                                           rects)
                                rect-x (transduce (map :x) min Long/MAX_VALUE rects)
                                rect-y (transduce (map :y) min Long/MAX_VALUE rects)
                                rect-width (- (transduce (map (fn [{:keys [x width]}]
                                                                (+ x width)))
                                                         max
                                                         0
                                                         rects)
                                              rect-x)
                                rect-height (- (transduce (map (fn [{:keys [y height ]}]
                                                                (+ y height)))
                                                         max
                                                         0
                                                         rects)
                                               rect-y)]
                            (assoc m 
                                   :rect/x rect-x
                                   :rect/y rect-y
                                   :rect/width rect-width
                                   :rect/height rect-height
                                   :rects rects
                                   :view view)))))
        
        ;; setting the paragraph width can 
        ;; cause the text to not line up if there are long lines
        ;; too lazy to fix for now.
        source-paragraph (para/paragraph source-text 600 {:paragraph-style/text-style (:base-style source-editor)})
        source-highlights (into []
                                (map-add-view source-paragraph)
                                source-highlights)
        source-diff-view (into [source-paragraph] (map :view) source-highlights)
        
        target-paragraph (para/paragraph target-text 600 {:paragraph-style/text-style (:base-style target-editor)})
        target-highlights (into []
                                (map-add-view target-paragraph)
                                target-highlights)
        target-diff-view (into [target-paragraph] (map :view) target-highlights)]
    {:source-paragraph source-paragraph
     :source-diff-view source-diff-view
     :source-highlights source-highlights
     :target-paragraph target-paragraph
     :target-diff-view target-diff-view
     :target-highlights target-highlights}))



(defeffect ::load-diff [{:keys [$diff-state fname
                                $highlight-index
                                $staged
                                $highlighted-highlight]}]
  (future
    (try
      (let [diff-state (highlight-file2 fname)
            deltas (->> (group-by :delta
                                  (concat
                                   (eduction
                                    (map (fn [m]
                                           (assoc m :side :source)))
                                    (:source-highlights diff-state))
                                   (eduction
                                    (map (fn [m]
                                           (assoc m :side :target)))
                                    (:target-highlights diff-state))))
                        (map (fn [[delta highlights]]
                               (let [minx (transduce (map :rect/x) min Long/MAX_VALUE highlights)
                                     miny (transduce (map :rect/y) min Long/MAX_VALUE highlights)]
                                 {:delta delta
                                  :minx minx
                                  :miny miny
                                  :highlights highlights})))
                        (sort-by (fn [{:keys [minx miny]}]
                                     [miny minx]))
                        (into []))

            diff-state (assoc diff-state :deltas deltas)]
        (dispatch! :set $diff-state diff-state)
        (dispatch! :set $highlight-index nil)
        (dispatch! :set $staged nil)
        (dispatch! :set $highlighted-highlight nil))
      (catch Throwable e
        (tap> e)))))


(defeffect ::scroll-next [this]
  (dispatch! ::scroll-by (assoc this :index-fn inc)))

(defeffect ::scroll-previous [this]
  (dispatch! ::scroll-by (assoc this :index-fn dec)))

(def highlight-pad 4)
(defeffect ::scroll-by [{:keys [$highlight-index
                                $scroll-offset
                                $highlighted-highlight
                                index-fn]
                         :as this}]
  
  (let [deltas (-> this :diff-state :deltas)
        _ (dispatch! :update $highlight-index
                     (fn [n]
                       (min (count deltas)
                            (max -1 
                                 (index-fn (or n -1))))))
        highlight-index (dispatch! :get $highlight-index)]

      (cond
        (= -1 highlight-index)
        (do (dispatch! :set $scroll-offset [0 0])
            (dispatch! :set $highlighted-highlight nil))
        
        (= highlight-index (count deltas))
        (let [[cw ch] (:membrane.stretch/container-size (:context this))
              total-height (ui/height (-> this :diff-state :target-diff-view))]
          (do (dispatch! :set $scroll-offset [0 (- total-height ch)])
              (dispatch! :set $highlighted-highlight nil)))
        
        :else
        (let [{:keys [highlights minx miny] :as delta} (nth deltas highlight-index)
              [cw ch] (:membrane.stretch/container-size (:context this))
              view (into []
                         (map (fn [{:keys [rect/x rect/y rect/width rect/height side]}]
                                
                                (let [x (case side
                                          :source (+ x (* 2 highlight-pad) (ui/width (-> this :diff-state :target-diff-view)))
                                          :target (+ highlight-pad x))]
                                  (->> (ui/translate
                                        x y
                                        (ui/rectangle width height))
                                       (ui/with-style ::ui/style-stroke)
                                       (ui/with-color [0 0 0])))))
                         highlights)]

          (dispatch! :set $highlighted-highlight view)
          (dispatch! :set $scroll-offset [0 (- miny (quot ch 2))])))))

(defeffect ::stage [{:keys [highlight-index
                            $staged]
                     {:keys [deltas]} :diff-state
                     :as this}]
  (when (and (>= highlight-index 0)
             (< highlight-index (count deltas)))
    
    (let [{:keys [highlights]
           :as delta} (nth deltas highlight-index)
          
          view (into []
                     (map (fn [{:keys [rect/x rect/y rect/width rect/height side]}]
                            (let [x (case side
                                      :source (+ x highlight-pad (ui/width (-> this :diff-state :target-diff-view)))
                                      :target (+ x))]
                               (ui/translate
                                x y
                                (ui/rectangle highlight-pad height)))))
                     highlights)]
      (dispatch! :update $staged
                 (fn [staged]
                   (assoc staged
                          (:delta delta) {:view view}))))))

(defeffect ::delete [{:keys [highlight-index
                             $deletes]
                      {:keys [deltas]} :diff-state
                      :as this}]
  (when (and (>= highlight-index 0)
             (< highlight-index (count deltas)))
    (let [{:keys [highlights]
           :as delta} (nth deltas highlight-index)
          
          view (into []
                     (map (fn [{:keys [rect/x rect/y rect/width rect/height side]}]
                            (let [x (case side
                                      :source (+ x highlight-pad (ui/width (-> this :diff-state :target-diff-view)))
                                      :target (+ x))]
                              (ui/translate
                               x y
                               (ui/rectangle highlight-pad height)))))
                     highlights)]
      (dispatch! :update $deletes
                 (fn [deletes]
                   (assoc deletes
                          (:delta delta) {:view view}))))))

(defeffect ::unstage [{:keys [highlight-index
                            $staged]
                     {:keys [deltas]} :diff-state
                     :as this}]
  (when (and (>= highlight-index 0)
             (< highlight-index (count deltas)))
    
    (let [{:keys [delta]} (nth deltas highlight-index)]
      (dispatch! :update $staged dissoc delta))))


(defn put-content-in-index!
  "Update the JGit index entry for `path` to point at a new blob created from `content-bytes`.
   `repo` is an org.eclipse.jgit.lib.Repository.
   `content-bytes` must be a byte-array."
  [^Repository repo ^String path ^bytes content-bytes]
  (let [^ObjectInserter inserter (.newObjectInserter repo)
        blob-id (try
                  (let [id (.insert inserter Constants/OBJ_BLOB content-bytes)]
                    (.flush inserter)
                    id)
                  (finally
                    (.close inserter)))
        ^DirCache cache (.lockDirCache repo)]
    (try
      (let [^DirCacheEditor editor (.editor cache)]
        (.add editor
              (proxy [DirCacheEditor$PathEdit] [path]
                (apply [^DirCacheEntry ent]
                  (.setFileMode ent FileMode/REGULAR_FILE) ; adjust if needed
                  (.setObjectId ent blob-id))))
        (.finish editor)
        (.write cache)
        (.commit cache))
      (finally
        (.unlock cache))))
  nil)


(defn inverse-delta
  "Given an delta, return its inverse."
  [^AbstractDelta delta]
  (let [^Chunk src (.getSource delta)
        ^Chunk target (.getTarget delta)
        delta-type (.getType delta)]
    (cond
      (= delta-type DeltaType/INSERT) (DeleteDelta. target src)
      (= delta-type DeltaType/DELETE) (InsertDelta. target src)
      (= delta-type DeltaType/CHANGE) (ChangeDelta. target src)
      :else (throw (ex-info "Unknown delta type" {:type delta-type :delta delta})))))


(defeffect ::apply-staged [{:keys [$staged staged
                                   $deletes deletes
                                   fname]
                            :as this}]
  ;; if there are deletes, just do those
  (if (seq deletes)
    (let [deltas (keys deletes)
          patch (Patch/new)]
      (run! #(Patch/.addDelta patch (inverse-delta %)) deltas)
      (let [initial-contents (file-contents fname)
            new-contents (str/join \newline (Patch/.applyTo patch (split-lines initial-contents)))]
        (spit (io/file fname) new-contents)
        (dispatch! ::load-diff this)))
    ;; else, add deltas marked as staged to git index
    (let [deltas (keys staged)]
      (when (seq deltas)
        (let [patch (Patch/new)]
          (run! #(Patch/.addDelta patch %) deltas)
          (let [initial-contents (index-contents fname)
                new-contents (str/join \newline (Patch/.applyTo patch (split-lines initial-contents)))]
            (with-git 
             [git fname]
             (let [repo (.getRepository git)
                   work-tree (Repository/.getWorkTree repo)]
               
               (put-content-in-index! repo (relative-path work-tree fname) (.getBytes new-contents "utf-8"))
               (dispatch! ::load-diff this)))))))))






(defui diff-ui [{:keys [diff-state fname highlight-index scroll-offset highlighted-highlight
                        staged
                        deletes] :as this}]
  (let [focus (:focus context)]
    (case diff-state
      
      ::loading (ui/label "loading...")
      nil (present/on-present
           (fn []
             [[:set $diff-state ::loading] 
              [::load-diff this]])
           (ui/label "loading..."))
      
      ;; else
      (let [{:keys [target-diff-view source-diff-view]} diff-state
            [cw ch :as size] (:membrane.stretch/container-size context)
            scroll-offset (or scroll-offset [0 0])

            staged-view (->> (into []
                                   (map :view)
                                   (vals staged))
                             (ui/with-style ::ui/style-fill)
                             (ui/with-color [0 0 0]))
            
            deletes-view (->> (into []
                                    (map :view)
                                    (vals deletes))
                              (ui/with-style ::ui/style-fill)
                              (ui/with-color [1 0 0]))
            
            body
            (basic/scrollview
             {:offset scroll-offset
              :scroll-bounds [(- cw 20)
                              (- ch 20)]
              :$body nil
              :body 
              (ui/no-events
               [staged-view
                deletes-view
                highlighted-highlight
                (ui/horizontal-layout
                 (ui/spacer highlight-pad 0)
                 target-diff-view
                 (ui/spacer highlight-pad 0)
                 source-diff-view)])})
            
            focus-key [::diff-ui $extra]

            focused? (= focus focus-key)
            body (ui/wrap-on
                  :mouse-down
                  (fn [handler mpos]
                    (into [[:set $focus focus-key]]
                          (handler mpos)))
                  body)
            body (if focused?
                   (ui/on
                    :key-press
                    (fn [s]
                      (case s
                        
                        ("n" "N")
                        [[::scroll-next this]]
                        
                        ("p" "P")
                        [[::scroll-previous this]]
                        
                        ("x" "X")
                        [[::apply-staged this]]
                        
                        ("g" "G")
                        [[:set $diff-state ::loading] 
                         [::load-diff this]]
                        
                        ("s" "S")
                        [[::stage this]]
                        
                        ("d" "D")
                        [[::delete this]]
                        
                        
                        ("u" "U")
                        [[::unstage this]]
                        
                        ;; else
                        nil)
                      )
                    body)
                   body)]
        
        body))))


(defeffect ::load-git-info [{:keys [$git-info path]}]
  (future
    (try
      (let [changed (changed-files path)]
        (dispatch! :set $git-info
                   {:git-work-tree-dir (get-git-work-tree path)
                    :untracked (:untracked changed)
                    :modified (:modified changed)
                    :added (:added changed)
                    :staged (:changed changed)})
        (dispatch! :repaint!))
      (catch Throwable t
        (tap> t)))))

(defeffect ::show-diff [{:keys [fname]}]
  (dispatch! :com.phronemophobic.easel/add-component-as-applet #'diff-ui {:fname fname}))

(defn commit [repo-dir {:keys [author message amend?] :as commit-info}]
  (with-git
   [git repo-dir]
   (when (not (seq message))
     (throw (ex-info "Commit must have a message."
                     {:repo-dir repo-dir
                      :commit-info commit-info})))
   (let [commit-command (doto (.commit git)
                          (.setMessage message)
                          (.setAmend (boolean amend?)))
         _ (when author
             (.setAuthor commit-command (:name author) (:email author)))
         
         commit (.call commit-command)]
     commit)))

(defeffect ::commit [{:keys [editor $editor path] :as this}]
  (let [msg (-> editor :rope str)]
    (commit path {:message msg})
    (dispatch! ::load-git-info this)
    (dispatch! :update $editor clobber.text/editor-clear) ))

(defeffect ::stage-file [{:keys [fname]
                          :as this}]
  (with-git
   [git fname]
   (let [^Repository repo (.getRepository git)
         work-tree (Repository/.getWorkTree repo)
         status (.call (doto (Git/.add git)
                         (.addFilepattern (relative-path work-tree fname))))]
     (tap> status))))

(defeffect ::unstage-file [{:keys [fname]}]
  (with-git
   [git fname]
   (let [^Repository repo (.getRepository git)
         work-tree (Repository/.getWorkTree repo)
         status (.call (doto (Git/.reset git)
                         (.addPath (relative-path work-tree fname))))]
     (tap> status)))
  )

(defeffect ::load-editor [{:keys [$editor]}]
  (let [editor (text.ui/make-editor)
        editor (assoc editor
                      :key-bindings
                      (assoc (:key-bindings editor)
                             "S-RET" ::commit))]
    (dispatch! :set $editor editor)))


(defui fidgit-ui [{:keys [git-info path editor] :as this}]
  (case git-info
    ::loading (ui/label "loading...")
    nil (present/on-present
           (fn []
             [[:set $git-info ::loading] 
              [::load-git-info this]
              [::load-editor this]])
           (ui/label "loading..."))
    ;; else
    (let [[cw ch :as size] (:membrane.stretch/container-size context)
          focus (:focus context)
          focus-key [::fidget-ui $extra]
          focused? (= focus focus-key)          

          editor-ui (let [editor-extra (get extra ::editor-extra)
                          editor-focus-key [::fidget-editor $extra]
                          editor-focused? (= editor-focus-key
                                             focus)

                          editor-ui (text.ui/text-editor
                                     {:editor editor
                                      :focused? editor-focused?
                                      :extra editor-extra
                                      :$extra $editor-extra})
                          editor-ui (ui/on
                                     :mouse-down
                                     (fn [_]
                                       [[:set $focus editor-focus-key]])
                                     editor-ui)
                          editor-ui (if editor-focused?
                                      (ui/on
                                       ::commit
                                       (fn [_]
                                         [[::commit this]])
                                       editor-ui)
                                      editor-ui)]
                      editor-ui)


          {:keys [untracked modified staged added]} git-info
          map-file-row (map (fn [fname]
                              (let [p (para/paragraph 
                                       fname
                                       nil
                                       default-paragraph-style)]
                                (assoc p ::fname fname))))

          title-row (fn [title]
                      (para/paragraph 
                       title
                       nil
                       (assoc-in default-paragraph-style
                                 [:paragraph-style/text-style
                                  :text-style/font-style]
                                 {:font-style/weight :bold})))

          rows
          (into []
                cat
                [[(title-row "Untracked")]
                 (eduction map-file-row
                           (map (fn [{::keys [fname] :as m}]
                                  (assoc m ::select-intents [[::stage-file {:fname fname}]
                                                             [::load-git-info this]])))
                           (map (fn [{::keys [fname] :as m}]
                                   (assoc m ::key-intents-fn
                                          (fn [s]
                                            (case s
                                              ("o" "O")
                                              [[::open-file {:fname (io/file
                                                                     (:git-work-tree-dir git-info)
                                                                     fname)}]]                                              
                                              
                                              ;; else
                                              nil)))))
                           untracked)
                 [(title-row "Modified")]
                 (eduction map-file-row
                           (comp
                            (map (fn [{::keys [fname] :as m}]
                                   (assoc m ::select-intents [[::show-diff {:fname (io/file
                                                                                    (:git-work-tree-dir git-info)
                                                                                    fname)}]])))
                            (map (fn [{::keys [fname] :as m}]
                                   (assoc m ::key-intents-fn
                                          (fn [s]
                                            (case s
                                              ("a" "A")
                                              [[::stage-file {:fname fname}]
                                               [::load-git-info this]]
                                              
                                              ("o" "O")
                                              [[::open-file {:fname (io/file
                                                                     (:git-work-tree-dir git-info)
                                                                     fname)}]]
                                              
                                              ("d" "D")
                                              [[::show-unified-diff {:fname (io/file
                                                                             (:git-work-tree-dir git-info)
                                                                             fname)}]]
                                              
                                              ;; else
                                              nil))))))
                           
                           modified)
                 [(title-row "Staged")]
                 (eduction map-file-row 
                           (comp
                            (map (fn [{::keys [fname] :as m}]
                                   (assoc m ::select-intents [[::unstage-file {:fname fname}]
                                                              [::load-git-info this]])))
                            (map (fn [{::keys [fname] :as m}]
                                   (assoc m ::key-intents-fn
                                          (fn [s]
                                            (case s
                                              ("d" "D")
                                              [[::show-staged-unified-diff
                                                {:fname (io/file
                                                         (:git-work-tree-dir git-info)
                                                         fname)}]]
                                              
                                              ("o" "O")
                                              [[::open-file {:fname (io/file
                                                                     (:git-work-tree-dir git-info)
                                                                     fname)}]]                                              
                                              
                                              ;; else
                                              nil))))))
                           staged)
                 [(title-row "Added")]
                 (eduction map-file-row 
                           (map (fn [{::keys [fname] :as m}]
                                  (assoc m ::select-intents [[::unstage-file {:fname fname}]
                                                             [::load-git-info this]])))
                           added)])

          table
          (grid/list-view
           {:row-fn (fn [{:keys [row col
                                 cell-width
                                 cell-height]
                          :as row-info}]
                      (let [body (nth rows row)
                            select-intents (::select-intents body)
                            key-intents-fn (::key-intents-fn body)
                            fname (::fname body)

                            body (ui/padding 1 body)
                            hover? (get extra [::row-hover row])
                            [_w h] (ui/bounds body)
                            w (long (* 0.95 cw))
                            body (if (and focused? hover?)
                                   [(ui/filled-rectangle
                                     [0.9 0.9 0.9] 
                                     w h)
                                    body]
                                   body)
                            
                            body (ui/fixed-bounds [w h]
                                                  body)

                            body (if select-intents
                                   (ui/on
                                    :mouse-down
                                    (fn [_]
                                      select-intents)
                                    body)
                                   body)
                            body (if (and hover? focused? key-intents-fn)
                                   (ui/on
                                    :key-press key-intents-fn
                                    body)
                                   body)

                            body (if fname
                                   (basic/on-hover
                                    {:hover? hover?
                                     :$body nil
                                     :body body})
                                   body)]
                        body))
            :width cw
            :height (- ch (ui/height editor-ui))
            :num-rows (count rows)})
          
          table (ui/wrap-on
                 :mouse-down
                 (fn [handler mpos]
                   (into [[:set $focus focus-key]]
                         (handler mpos)))
                 table)
          table (if focused?
                  (ui/wrap-on
                   :key-press
                   (fn [handler s]
                     (let [intents (handler s)]
                       (if (seq intents)
                         intents
                         (case s
                           ("g" "G")
                           [[:set $git-info ::loading] 
                            [::load-git-info this]]
                           
                           nil))))
                   table)
                  table)]
      (ui/vertical-layout
       editor-ui
       table))))

(defeffect ::open-fidget [{:keys [editor]}]
  (when-let [file (:file editor)]
    (dispatch! :com.phronemophobic.easel/add-component-as-applet
               #'fidgit-ui
               {:git-info nil
                :path file})))

(comment
  
  (dev/add-component-as-applet #'fidgit-ui
                               {:git-info nil
                                :path "."})
  


  
  ,)


(defn unified-diff-data [source target mode]
  (let [source-lines (split-lines source)
        target-lines (split-lines target)
        deltas (-> (DiffUtils/diff source-lines target-lines)
                   Patch/.getDeltas)
        
        source-editor (-> (clobber.editor/make-editor {:source source
                                                       :mode mode})
                          (assoc ::mode mode))
        target-editor (-> (clobber.editor/make-editor {:source target
                                                       :mode mode})
                          (assoc ::mode mode))

        

        context-size 3]
    (loop [deltas (seq deltas)
           ps []]
      (if (seq deltas)
        (let [^AbstractDelta
              delta (first deltas)
              delta-type (.getType delta)
              
              source-chunk (.getSource delta)
              target-chunk (.getTarget delta)
              
              target-editor (clobber.text/editor-goto-line target-editor
                                                           (max 0 (- (Chunk/.getPosition target-chunk)
                                                                     context-size)))
              start-cursor (:cursor target-editor)
              target-editor (clobber.text/editor-goto-line target-editor
                                                           (Chunk/.getPosition target-chunk))
              target-start-cursor (:cursor target-editor)
              target-lines (Chunk/.getLines target-chunk)
              
              target-editor (clobber.text/editor-goto-line target-editor (+ (Chunk/.getPosition target-chunk)
                                                                            (count target-lines)))
              target-end-cursor (:cursor target-editor)
              
              target-editor (clobber.text/editor-goto-line target-editor (+ (Chunk/.getPosition target-chunk)
                                                                            (count target-lines)
                                                                            context-size))
              end-cursor (:cursor target-editor)
              
              text []
              
              ;; add pre context and additions  
              extent {:start-byte-offset (:byte start-cursor)
                      :end-byte-offset (:byte target-end-cursor)}
              text (into text
                         (clobber.util.ui/styled-text (:rope target-editor)
                                                      (:base-style target-editor)
                                                      [(if (= mode :clojure)
                                                         (cui/syntax-style target-editor extent)
                                                         (when (:tree target-editor)
                                                           (when-let [query (:query target-editor)]
                                                             (when-let [theme (:theme target-editor)]
                                                               (clobber.util.ui/syntax-style target-editor
                                                                                             query
                                                                                             theme
                                                                                             extent)))))]
                                                      (:start-byte-offset extent)
                                                      (:end-byte-offset extent)))
              
              source-editor (clobber.text/editor-goto-line source-editor (Chunk/.getPosition source-chunk))
              source-start-cursor (:cursor source-editor)
              source-editor (clobber.text/editor-goto-line source-editor (+ (Chunk/.getPosition source-chunk)
                                                                            (count (Chunk/.getLines source-chunk))))
              source-end-cursor (:cursor source-editor)

              ;; add deletions
              extent {:start-byte-offset (:byte source-start-cursor)
                      :end-byte-offset (:byte source-end-cursor)}
              text (into text
                         (clobber.util.ui/styled-text (:rope source-editor)
                                                      (:base-style source-editor)
                                                      [(if (= mode :clojure)
                                                         (cui/syntax-style source-editor extent)
                                                         (when (:tree source-editor)
                                                           (when-let [query (:query source-editor)]
                                                             (when-let [theme (:theme source-editor)]
                                                               (clobber.util.ui/syntax-style source-editor
                                                                                             query
                                                                                             theme
                                                                                             extent)))))]
                                                      (:start-byte-offset extent)
                                                      (:end-byte-offset extent)))
              
              ;; add post context
              extent {:start-byte-offset (:byte target-end-cursor)
                      :end-byte-offset (:byte end-cursor)}
              text (into text
                         (clobber.util.ui/styled-text (:rope target-editor)
                                                      (:base-style target-editor)
                                                      [(if (= mode :clojure)
                                                         (cui/syntax-style target-editor extent)
                                                         (when (:tree target-editor)
                                                           (when-let [query (:query target-editor)]
                                                             (when-let [theme (:theme target-editor)]
                                                               (clobber.util.ui/syntax-style target-editor
                                                                                             query
                                                                                             theme
                                                                                             extent)))))]
                                                      (:start-byte-offset extent)
                                                      (:end-byte-offset extent)))              
              

              para (para/paragraph
                    text
                    nil
                    default-paragraph-style)
              
              target-highlight {:start-char (- (:char target-start-cursor)
                                               (:char start-cursor))
                                :delta delta
                                :side :target
                                :chunk target-chunk
                                :end-char (- (:char target-end-cursor)
                                             (:char start-cursor))}
              source-highlight {:start-char (:end-char target-highlight)
                                :end-char (+ (:end-char target-highlight)
                                             (- (:char source-end-cursor)
                                                (:char source-start-cursor)))
                                :side :source
                                :chunk source-chunk}

              para (assoc para
                          ::highlights
                          [target-highlight
                           source-highlight])
              
              ps (conj ps para)]
          (recur (next deltas)
                 ps))
        
        ;; else
        ps))))

(defn overlay-highlights [para]
  (let [
        delete-color [1 0 0 0.2]
        insert-color [0 1 0 0.2]

        highlights
        (into []
              (comp
               (filter (fn [{:keys [start-char end-char]}]
                         (> end-char start-char)))
               (map (fn [{:keys [start-char end-char side]}]
                      (let [rects (para/get-rects-for-range para start-char end-char :max :tight)]
                        (->> (into []
                                   (map
                                    (fn [{:keys [x y width height]}]
                                      (ui/translate x y
                                                    (ui/rectangle (max 4 width) height))))
                                   rects)
                             (ui/with-style ::ui/style-fill)
                             (ui/with-color (if (= side :target)
                                              insert-color
                                              delete-color)))))))
              (::highlights para))]
    [para
     highlights]))

(defn unified-diff [source target mode]
  (->> (unified-diff-data source target mode)
       (map overlay-highlights)
       
       (interpose (ui/filled-rectangle [0 0 0]
                                       200 10))
       (apply ui/vertical-layout)
       ))

(comment
  (unified-diff (index-contents "deps.edn")
                (file-contents "deps.edn")
                nil)
  
  ,)

(defeffect ::show-unified-diff [{:keys [fname]}]
  (dispatch! :com.phronemophobic.easel/add-component-as-applet
             (constantly
              (unified-diff (index-contents fname)
                            (file-contents fname)
                            (clobber.editor/guess-mode {:file fname})))
             {}))

(defeffect ::show-staged-unified-diff [{:keys [fname]}]
  (dispatch! :com.phronemophobic.easel/add-component-as-applet
             (constantly
              (unified-diff (head-contents fname)
                            (index-contents fname)
                            (clobber.editor/guess-mode {:file fname})))
             {}))

(defeffect ::open-file [{:keys [fname]}]
  (dispatch! :com.phronemophobic.easel/add-applet
             {:make-applet
              (let [f (requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)]
                #(f % {:file fname}))}))