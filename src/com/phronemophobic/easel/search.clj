(ns com.phronemophobic.easel.search
  (:require [clojure.core.async.flow :as flow]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [membrane.ui :as ui]
            [membrane.basic-components :as basic]
            [membrane.skia.paragraph :as para]
            [membrane.alpha.component.drag-and-drop :as dnd]
            [com.phronemophobic.viscous :as viscous]
            [com.phronemophobic.clobber.modes.clojure.ui :as cui]
            [com.phronemophobic.clobber.modes.text.ui :as text.ui]
            [com.phronemophobic.clobber.modes.text :as text-mode]
            [com.phronemophobic.clobber.util.ui.key-binding :as key-binding]
            [com.phronemophobic.membrandt.icon.ui :as icon.ui]
            [com.phronemophobic.membrandt.impl.grid :as grid]
            [com.phronemophobic.easel.model :as model]
            [membrane.component.present :as present]
            [membrane.component :refer [defui defeffect]])
  (:import (java.util.regex Pattern
                            Matcher
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

(defonce all-flows (atom []))
(defn stop-all-flows! []
  (let [[flows _] (reset-vals! all-flows [])]
    (doseq [flow flows]
      (flow/stop flow))))
(defn ^:private track-flow [flow]
  (swap! all-flows conj flow)
  flow)

(comment
  (stop-all-flows!)
  ,)

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

(defn ^:private monitoring [{:keys [report-chan error-chan]}]
  (prn "========= monitoring start")
  (async/thread
    (loop []
      (let [[val port] (async/alts!! [report-chan error-chan])]
        (if (nil? val)
          (prn "========= monitoring shutdown")
          (do
            (prn (str "======== message from " (if (= port error-chan) :error-chan :report-chan)))
            (clojure.pprint/pprint (select-keys val [::flow/pid ::flow/cid ::flow/ex]))
            (tap> val)
            
            (recur))))))
  nil)

(defn ^:private wrap-producer
  "given a process map. Return a process that allows process to recur when state contains a true ::produce key."
  [{:keys [describe init transition transform]}]
  {:describe describe
   :init
   (fn [m]
     (let [ch (async/chan (async/sliding-buffer 1))]
       (-> (if init (init m) m)
           (update ::flow/in-ports
                   assoc
                   ::kickstart ch)
           (update ::flow/out-ports
                   assoc ::recur ch))))
   :transition
   (fn [state status]
     (let [state (if transition
                   (transition state status)
                   state)]
       (when (and (::produce state)
                  (= status ::flow/resume))
         (let [kickstart-ch (-> state
                                ::flow/in-ports
                                ::kickstart)]
           (async/>!! kickstart-ch true)))
       (when (= state ::flow/stop)
         (let [kickstart-ch (-> state
                                ::flow/in-ports
                                ::kickstart)]
           (async/close! kickstart-ch true)))
       state))
   :transform
   (fn [state in msg]
     (let [[state outs] (transform state in msg)
           
           outs (if (::produce state)
                  (conj (into [] outs)
                        [::recur [true]])
                  outs)]
       [state outs]))})

;; search flow
;; :in channel takes a map like

(def ^:private search-step
  (flow/map->step
   (wrap-producer
    {:describe
     (fn []
       {:params {:pred "pred"
                 :value "Value"
                 :xform "xform"
                 :sentinel "sentinel"}
        :ins {:in "in"}
        :outs {:out "out"}})
     :init
     (fn [params]
       
       (let [xform (or (:xform params)
                       identity)]
         (assoc params
                ::produce true
                :rf (xform (fn [_result val]
                             val))
                :iter (:value params))))
     :transition
     (fn [state transition]
       state)
     :transform
     (fn [{:keys [rf iter] :as state} input msg]
       (if (has-next? iter)
         (let [x (iter-next iter)

               outs (when-let [val (rf nil x)]
                      {:out [val]})]
           [state outs])
         (do
           [(assoc state ::produce false)
            {:out [(:sentinel state)]}])))})))



(defn ^:private search-flow 
  ([pred value]
   (search-flow pred value nil))
  ([pred value opts]
   {:procs
    {:search
     {:args (merge {:pred pred 
                    :value value}
                   (select-keys opts [:branch? :children]))
      :proc (flow/process #'search-step)}
     
     :onto-chan
     {:args {:chan (async/chan 10)}
      :proc
      (flow/process
       (flow/map->step
        {:describe (fn [] {:ins {:in "  "}
                           :params {:chan "Channel to put values onto"}})
         :init (fn [m]
                 (assoc m ::flow/out-ports {:out (:chan m)}))
         :transform (fn [_ _ v]
                      [_ {:out [v]}])}))}
     
     :tap-sink
     {:proc (flow/process
             (flow/map->step
              {:describe (fn [] {:ins {:in "gimme stuff to print!"}})
               :transform (fn [_ _ v]
                            (tap> [:flow v])
                            nil)}))}
     
     :prn-sink
     {:proc (flow/process
             (flow/map->step
              {:describe (fn [] {:ins {:in "gimme stuff to print!"}})
               :transform (fn [_ _ v] (prn :prn v))}))}}
    :conns
    [
     [[:search :out] [:tap-sink :in]]
     ,]
    
    ,}))


(defn ^:private search 
  ([pred value]
   (search pred value nil))
  ([pred value opts]
   (let [flow (-> (flow/create-flow (search-flow pred value opts))
                  track-flow)]
     (-> flow flow/start monitoring)
     (flow/resume flow)
     nil)))

(def ^:private onto-chan-proc
  (flow/map->step
   {:describe (fn [] {:ins {:in "  "}
                      :params {:chan "Channel to put values onto"}})
    :init (fn [m]
            (assoc m ::flow/out-ports {:out (:chan m)}))
    :transition
    (fn [state transition]
      (when (= transition ::flow/stop)
        (let [ch (-> state ::flow/out-ports :out)]
          (async/close! ch)))
      state)
    :transform (fn [state _ v]
                 [state {:out [v]}])}))

(defn ^:private search-sync
  ([pred value]
   (search-sync pred value nil))
  ([pred value opts]
   
   (let [done-ch (async/chan 1)
         sentinel (Object.)
         accumulate-results-ch (async/chan 1 (take-while #(not= % sentinel)))
         results-ch (async/into [] accumulate-results-ch)
         g {:procs
            {:search
             {:args (merge {:pred pred 
                            :value value
                            :sentinel sentinel}
                           (select-keys opts [:branch? :children]))
              :proc (flow/process #'search-step)}
             
             :accumulate-results
             {:args {:chan accumulate-results-ch}
              :proc (flow/process onto-chan-proc)}
             
             
             :tap-sink
             {:proc (flow/process
                     (flow/map->step
                      {:describe (fn [] {:ins {:in "gimme stuff to print!"}})
                       :transform (fn [_ _ v]
                                    (tap> [:flow v])
                                    nil)}))}
             
             :prn-sink
             {:proc (flow/process
                     (flow/map->step
                      {:describe (fn [] {:ins {:in "gimme stuff to print!"}})
                       :transform (fn [_ _ v] (prn :prn v))}))}}
            :conns
            [
             [[:search :out] [:tap-sink :in]]
             [[:search :out] [:accumulate-results :in]]

             ,]
            
            ,}
         flow (-> (flow/create-flow g)
                  track-flow)]
     (-> flow flow/start monitoring)
     (flow/resume flow)
     (let [result (async/<!! results-ch)]
       (flow/stop flow)
       result))))


(comment
  
  (search-sync (constantly true)
          [1 2 3 {:a 42}]
          {:branch? vector?})
  (def result
    (search-sync
     (fn [m]
       (and (map? m)
            (:text-style/font-families m)))
     (dissoc x-104706
             :history))
)
  
  (def icons
    (search-sync
     (fn [o]
       (instance? com.phronemophobic.membrandt.icon.ui.Icon o))
     x-107328
     {:branch?  (fn [o] 
                  (seq (membrane.ui/children o)))
      :children membrane.ui/children}
     ))
  
  (require '[membrane.ui :as ui])
  (dev/add-component-as-applet
   (constantly
    (ui/flex-layout
     icons
     {:direction :column})))
  
  (membrane.ui/children x-107328)

  (stop-all-flows!)
  ,)


(defeffect ::stop-search [{:keys [$ref]}]
  (let [m (dispatch! :get $ref)
        flow (:flow m)]
    (when flow 
      (dispatch! :update $ref dissoc :flow)
      (flow/stop flow))))


(defeffect ::clear-results [{:keys [$ref]}]
  (dispatch! :update $ref dissoc :results :search-id))

;; stop existing search
;; create new flow
;; start flow
(defeffect ::search [{:keys [$ref iter] :as m}]
  (dispatch! ::stop-search m)
  (let [
        ;; query-string (-> query-editor
        ;;                  :rope
        ;;                  str)
        ;; value (binding [*ns* (:eval-ns query-editor)]
        ;;         (eval (read-string query-string)))
        

        done-ch (async/chan 1)
        sentinel (Object.)
        accumulate-results-ch (async/chan 1 (take-while #(not= % sentinel)))
        g {:procs
           {:search
            {:args (merge {;; :xform xform
                           :value iter
                           :sentinel sentinel})
             :proc (flow/process #'search-step)}
            
            :accumulate-results
            {:args {:chan accumulate-results-ch}
             :proc (flow/process onto-chan-proc)}}
           :conns
           [[[:search :out] [:accumulate-results :in]]
            
            ,]
           
           ,}
        flow (-> (flow/create-flow g)
                 track-flow)
        search-id (random-uuid)]
    (dispatch! :update $ref assoc :flow flow :search-id search-id )
    (-> flow flow/start monitoring)
    (flow/resume flow)
    (dispatch! :update $ref assoc :state :running)
    (async/go
     
     (try
       (loop [results []
              to nil]
         (let [[val port] (async/alts! (if to
                                         [to accumulate-results-ch]
                                         [accumulate-results-ch]))]
           (if (= port accumulate-results-ch)
             (if val
               (recur (conj results val)
                      (or to (async/timeout 500)))
               
               (when to
                 (dispatch! :update $ref assoc :results (viscous/wrap results))))
             ;; else timeout
             (do (dispatch! :update $ref assoc :results (viscous/wrap results))
                 (recur results nil)))))
       (catch Exception e
         (tap> e)
         (prn e))
       (finally
         (dispatch! :update $ref assoc :state :stopped)
         (flow/stop flow)))))
  nil)



(deftype APNthWrapped [obj n ^:unsynchronized-mutable ^int _hash]
  
  clojure.lang.IDeref
  (deref [_] (nth obj n))
  
  clojure.lang.IHashEq
  (hasheq [_]
    (if (zero? _hash)
      (let [h (int (clojure.core/hash-ordered-coll [::APNthWrapped obj n]))]
        (set! _hash h)
        h)
      _hash))
  (hashCode [_] 
    (if (zero? _hash)
      (let [h (int (clojure.core/hash-ordered-coll [::APNthWrapped obj n]))]
        (set! _hash h)
        h)
      _hash))
  (equals [this that]
    (if (instance? clojure.lang.IDeref that)
      (identical? (nth obj n) @that)
      false))
  
  viscous/PWrapped
  (-unwrap [_]
    (nth obj n)))

(defn nth-wrap [o n]
  (->APNthWrapped o n (int 0))) 

(defeffect ::search-view-search [{:keys [query-editor] :as this}]
  (try
    (let [query-string (-> query-editor
                           :rope
                           str)
          value (binding [*ns* (:eval-ns query-editor)]
                  (eval (read-string query-string)))]
      (dispatch! ::search (assoc this :iter (->iter value))))
    (catch Throwable t
      (tap> t))))

(defui search-view [{:keys [query-editor results search-id] :as this}]
  (let [focus (:focus context)
        query-editor-focus-id [$query-editor ::focus]
        query-editor-focused? (= focus query-editor-focus-id)
        [cw ch] (:membrane.stretch/container-size context)
        
        *results (when results
                   @results)
        result-count (when results
                       (count *results))

        header
        (ui/vertical-layout
         (ui/horizontal-layout
          
          (ui/on-click
           (fn []
             [[::search-view-search this]])
           (icon.ui/icon {:name "search"}))
          (ui/on-click
           (fn []
             [[::stop-search this]])
           (icon.ui/icon {:name "stop"}))
          (ui/on-click
           (fn []
             [[::clear-results this]])
           (icon.ui/icon {:name "delete"})))
         (ui/on
          ::cui/request-focus
          (fn []
            [[:com.phronemophobic.easel/request-focus query-editor-focus-id]])
           (cui/code-editor {:editor query-editor
                             :focused? query-editor-focused?}))
         (ui/on
          :mouse-down
          (fn [_]
            [[::dnd/drag-start {::dnd/obj {:x results}}]])
          (ui/label 
          (if results
            (str "Results (" result-count ")")
            (if (:flow this)
              "Results ..."
              "Results")))))

        
        results-view
        (when results
          (grid/list-view
           {:row-fn (fn [{:keys [row]}]
                      (let [extra (get extra [::inspector search-id row])]
                        (ui/padding
                         3
                         (viscous/inspector {:obj (nth-wrap *results row)}))))
            :num-rows result-count
            :width cw
            :height (- ch (ui/height header) 20)
            :extra (get extra [:results search-id])}))]
    (ui/vertical-layout
     header
     results-view)))


(defrecord SearchApplet [dispatch! initial-state]
  model/IApplet
  (-start [this {:keys [$ref size] :as info}]
    (assoc this
           :extra {}
           :$extra [$ref '(keypath :extra)]
           :$ref $ref
           :results nil
           :size size
           :query-editor 
           (let [width (first size)
                 height 200]
             (-> (cui/make-editor {:source
                                   (or (:query initial-state)
                                       "")})
                 (cui/editor-set-height height)
                 (assoc :width width
                        :height height)
                 (assoc :eval-ns (the-ns 'com.phronmophobic.inquery))))
           :$query-editor [$ref '(keypath :query-editor)]
           :value (:value initial-state)
           :$value [$ref '(keypath :value)]))
  (-stop [this]
    nil)
  model/IUI
  (-ui [this ui-info]
    (search-view (assoc this
                        :context (:context ui-info)
                        :$context (:$context ui-info))))
  model/IResizable
  (-resize [this size _content-scale]
    (let [[width height] size]
      (-> this
          (assoc :size size)
          (update :query-editor
                  assoc :width width)))))

(defn ^:private search-applet [handler {:keys [query value branch? children] :as m}]
  (-> (->SearchApplet handler m)
      (assoc :label "Search")))

(comment
  
  (com.phronemophobic.easel/add-applet 
   {:make-applet
    (fn [handler]
      (search-applet handler {:query ""}))})
  

  (let [uuid ]
    (binding [*ns* ]
      (def my-uuid uuid)))
  (stop-all-flows!)
  ,)




(defn ^:private file-tree [f]
  (tree-seq 
   #(-> % File/.listFiles seq)
   #(-> % File/.listFiles seq)
   (io/file f)))




(defn ^:private count-newlines
  ([^CharSequence s]
   (count-newlines s 0 (.length s)))
  ([^CharSequence s start]
   (count-newlines s start (.length s)))
  ([^CharSequence s start end]
   (loop [idx start
          new-line-count 0]
     
     (if (< idx end)
       (let [c (.charAt s idx)]
         (case c 
           -1 new-line-count
           \newline (recur (inc idx) (inc new-line-count))
           ;; else
           (recur (inc idx) new-line-count)))
       new-line-count))))

(defn ^:private match-context
  "For a snippet from `start`, `end` of `s`,
  return the trimmed line that contains the snippet."
  [^CharSequence s start end]
  ;; get whole line
  ;; then trim whitespace
  (let [line-start (loop [idx start]
                     (let [next-index (dec idx)]
                       (if (pos? next-index)
                         
                         (let [c (.charAt s next-index)]
                           (case c
                             -1 idx
                             \newline idx
                             ;; else
                             (recur next-index)))
                         idx)))
        line-end (loop [idx end]
                   (let [c (.charAt s idx)]
                     (case c
                       -1 idx
                       \newline idx
                       ;; else
                       (recur (inc idx)))))]
    (str/trim (subs s line-start line-end))))

(defn ^:private grep [f ^Pattern regexp]
  (let [source (slurp f)
        matcher (Pattern/.matcher regexp source)]
    (loop [search-index 0
           results []
           line-number 0]
      (let [result (when (Matcher/.find matcher search-index)
                     (Matcher/.toMatchResult matcher))]
        (if result
          (let [
                start (MatchResult/.start result)
                end (MatchResult/.end result)
                
                line (+ line-number (count-newlines source search-index start))]
            (recur end
                   (conj results {:start start
                                  :match (subs source start end)
                                  :context (match-context source start end)
                                  :end end
                                  :line line})
                   (+ line (count-newlines source start end))))
          results)))))


(def ^:private clojure-file-regexp #".*(\.clj|\.cljs|.cljc)")
(defn ^:private rgrep [f ^Pattern fregexp regexp]
  (let [root f]
    (->> (file-tree f)
         (filter #(File/.isFile %))
         (filter #(re-matches fregexp (-> % File/.getName)))
         (keep (fn [f]
                 (let [matches (grep f regexp)]
                   (when (seq matches)
                     {:root root
                      :file f
                      :matches matches}))))
         (mapcat (fn [{:keys [root file matches]}]
                   (eduction 
                    (map #(-> % (assoc :root root :file file :relative-path (relativize root file))))
                    matches))))))


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

(comment
  (dev/add-component-as-applet #'folder-picker
                               {:folder (io/file ".")
                                :focused? true})
  
  
  
  

  ,)

(defeffect ::rgrep-search [{:keys [search-state $search-state folder regexp-editor file-regexp-editor]}]
  (let [
        ;; root-value (binding [*ns* (:eval-ns folder-picker)]
        ;;              (eval (read-string (-> folder-picker :rope str))))
        
        
        regexp-str (-> regexp-editor :rope str)
        regexp-value (Pattern/compile regexp-str)
        
        file-regexp-str (-> file-regexp-editor :rope str)
        file-regexp-value (Pattern/compile file-regexp-str)]

    (dispatch! ::search (assoc search-state
                               :$ref $search-state
                               :iter (->iter (rgrep folder file-regexp-value regexp-value))))))

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

(defui rgrep-ui [{:keys [folder
                         regexp-editor
                         file-regexp-editor
                         
                         search-state
                         ]
                  :as this}]
  (let [focus (:focus context)
        folder-picker-focus-id [$folder ::focus]
        folder-picker-focused? (= focus folder-picker-focus-id)
        
        regexp-editor-focus-id [$regexp-editor ::focus]
        regexp-editor-focused? (= focus regexp-editor-focus-id)

        file-regexp-editor-focus-id [$file-regexp-editor ::focus]
        file-regexp-editor-focused? (= focus file-regexp-editor-focus-id)

        [cw ch] (:membrane.stretch/container-size context)

        regexp-editor-focus-id [$regexp-editor ::focus]
        regexp-editor-focused? (= focus regexp-editor-focus-id)

        
        results (:results search-state)
        search-id (:search-id search-state)
        *results (when results
                   @results)
        result-count (when results
                       (count *results))
        
        header
        (ui/vertical-layout
         (ui/horizontal-layout
          
          (ui/on-click
           (fn []
             [[::rgrep-search this]])
           (icon.ui/icon {:name "search"}))
          (ui/on-click
           (fn []
             [[::stop-search {:$ref $search-state}]])
           (icon.ui/icon {:name "stop"}))
          (ui/on-click
           (fn []
             [[::clear-results {:$ref $search-state}]])
           (icon.ui/icon {:name "delete"})))
         (ui/label "regex:")
         (let [editor (simple-editor {:editor regexp-editor
                                      :focused? true})]
           (if regexp-editor-focused?
             (ui/on
              ::focus-next
              (fn [_]
                [[:com.phronemophobic.easel/request-focus file-regexp-editor-focus-id]])
              editor)
             (ui/on-mouse-down
              (fn [_]
                [[:com.phronemophobic.easel/request-focus regexp-editor-focus-id]])
              (ui/fill-bordered [0.9 0.9 0.9  ]
                                2
                                (ui/no-events editor)))))
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
                            {:keys [start context match end line file root relative-path]} (nth *results row)
                            body (ui/padding
                                  3
                                  (para/paragraph
                                   [relative-path
                                    " "
                                    (str line)
                                    " "
                                    (truncate context 200)]
                                   cw
                                   default-paragraph-style))
                            hover? (get extra ::hover)
                            body (if hover?
                                   (ui/fill-bordered [0.9 0.9 0.9  ] 0 body)
                                   body)
                            body (ui/on
                                  :mouse-down
                                  (fn [_]
                                    [[::open-editor {:file file
                                                     :line line}]])
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



(defui search-results [{:keys [search-state view value]}]
  (let [[cw ch] (:membrane.stretch/container-size context)
        results (:results search-state)
        search-id (:search-id search-state)
        *results (when results
                   @results)
        result-count (when results
                       (count *results))
        
        header
        (ui/vertical-layout
         (ui/horizontal-layout
          (ui/on-click
           (fn []
             [[::search {:$ref $search-state
                         :iter (->iter @value)}]])
           (icon.ui/icon {:name "search"}))
          (ui/on-click
           (fn []
             [[::stop-search {:$ref $search-state}]])
           (icon.ui/icon {:name "stop"}))
          (ui/on-click
           (fn []
             [[::clear-results {:$ref $search-state}]])
           (icon.ui/icon {:name "delete"})))
         (ui/on
          :mouse-down
          (fn [_]
            [[::dnd/drag-start {::dnd/obj {:x results}}]])
          (ui/label 
           (if results
             (str "Results (" result-count ")")
             (if (:flow search-state)
               "Results ..."
               "Results")))))
        
        
        results-view
        (when results
          (grid/list-view
           {:row-fn (fn [{:keys [row]}]
                      (let [extra (get extra [::inspector search-id row])
                            hover? (get extra ::hover?)
                            body (ui/padding
                                  3
                                  (view {:obj (nth-wrap *results row)
                                         :extra extra
                                         :$extra $extra
                                         :context context
                                         :$context $context})
                                  #_(viscous/inspector {:obj (nth-wrap *results row)}))
                            body (if hover? 
                                   (ui/fill-bordered [0.8 0.8 0.8]
                                                     0
                                                     body)
                                   body)]
                        (basic/on-hover
                         {:hover? hover?
                          :body body
                          })))
            :num-rows result-count
            :width cw
            :height (- ch (ui/height header) 20)
            :extra (get extra [:results search-id])}))]
    (ui/vertical-layout
     header
     results-view)))


(defn ^:private show-search
  ([value]
   (show-search value 
                (fn [m]
                  (let [extra (get m :extra)
                        $extra (get m :$extra)]
                    (viscous/inspector 
                     (assoc m
                            :width (get extra :width 40)
                            :$width [$extra '(keypath :width)]
                            :height (get extra :height 1)
                            :$height [$extra '(keypath :height)]))))))
  ([value view]
   (dev/add-component-as-applet
    #'search-results
    {:view view
     :value (viscous/wrap value)})))


(comment

  (dev/add-component-as-applet
   #'rgrep-ui
   {:folder (io/file ".")
    :regexp-editor (-> (text.ui/make-editor {:source "foo"})
                       (update :key-bindings
                               assoc "TAB" ::focus-next))
    :file-regexp-editor (-> (text.ui/make-editor {:source ".*(\\.clj|\\.cljs|\\.cljc)"})
                            (update :key-bindings
                                    assoc "TAB" ::focus-next))
    
    :search-state nil})
  
  (grep "README.md" #"#")

  (show-rgrep!)

  (show-search
   (rgrep "." 
          #".*(\.clj|\.cljs|.cljc)"
          #"require"))
  
  (doall
   (rgrep "." 
          #".*(\.clj|\.cljs|.cljc)"
          #"require"))
  
  
  
  
  (show-search
   (rgrep "." 
          #".*(\.clj|\.cljs|.cljc)"
          #".")
   {:component #'mycomponent})
  


  
  (com.phronemophobic.easel/add-applet 
   {:make-applet
    (fn [handler]
      (search-applet handler {:query ""}))})  

  (def myrequires (rgrep "/var/tmp" clojure-file-regexp #"require"))
  
  (type (icon.ui/icon {:name "search"}))

  ,)


(defeffect ::open-editor [{:keys [ns line file] :as m}]
  (dispatch! 
   :com.phronemophobic.easel/add-applet
   {:make-applet
    (fn [handler]
      ((requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)
       handler
       m))}))

(defeffect ::show-rgrep [_]
  (dispatch! :com.phronemophobic.easel/add-component-as-applet
             #'rgrep-ui
             {:folder (io/file ".")
              :regexp-editor (-> (text.ui/make-editor {:source "foo"})
                                 (update :key-bindings
                                         assoc "TAB" ::focus-next))
              :file-regexp-editor (-> (text.ui/make-editor {:source ".*(\\.clj|\\.cljs|\\.cljc)"})
                                      (update :key-bindings
                                              assoc "TAB" ::focus-next))
              
              :search-state nil}))

(defn show-rgrep []
  ((requiring-resolve 'dev/add-component-as-applet)
   #'rgrep-ui
   {:folder (io/file ".")
    :regexp-editor (-> (text.ui/make-editor {:source "foo"})
                       (update :key-bindings
                               assoc "TAB" ::focus-next))
    :file-regexp-editor (-> (text.ui/make-editor {:source ".*(\\.clj|\\.cljs|\\.cljc)"})
                            (update :key-bindings
                                    assoc "TAB" ::focus-next))
    
    :search-state nil}))

