(ns dev
  (:require [com.phronemophobic.easel]
            [flow-storm.runtime.debuggers-api :as dbg-api]
            [flow-storm.runtime.indexes.api :as idx-api]
            [flow-storm.runtime.events :as rt-events]
            [fs-tester]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All this should work if we start with the :flow-storm-headless alias ;;
;; Everything under fs-tester* should be instrumented                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We can register a function to receive all the events
(rt-events/set-dispatch-fn
   (fn [[event-key event-args]]
     (when-not (#{:heap-info-update} event-key)
       (println "[Event] " event-key event-args))))

;; let's work only recording on flow-0 for now
(def flow-id 0)

;; by default we will be recording on flow-0, but this can be used to record on a different flow
(dbg-api/switch-record-to-flow flow-id)

(comment
  (easel/run)

  ;; we can toggle recording on/off (will fire an event telling the new status)
  (dbg-api/toggle-recording)

  ;; same but for the multi-thread-timeline
  (dbg-api/toggle-multi-timeline-recording)

  ;; get rid of all recordings
  (dbg-api/clear-runtime-state)

  ;; run some function on an instrumented namespace, when recordings is on
  ;; this should be recorded since it's on an instrumented ns
  (fs-tester/sum 4 5)

  ;; get information about all flows that contain recordings and their recorded threads ids
  (dbg-api/all-flows-threads)

  ;; get information on each flow thread
  (dbg-api/flow-threads-info flow-id)

  ;; let's work with some thread we saw we have recordings on
  (def thread-id 72)

  ;; this should be called with the index of an entry of fn-call type, to retrieve that "fn frame data"
  (idx-api/frame-data flow-id thread-id 0 {:include-path? true, :include-exprs? true, :include-binds? true})

  ;; this can be used for basic stepping
  ;; this will return entries of this kinds :
  ;; {:type :fn-call, :ret-idx 4, :fn-call-idx 0, :fn-name "sum", :fn-args [4 5], :form-id -1340777963, :idx 0, :fn-ns "fs-tester", :parent-idx nil}
  ;; {:type :expr, :coord [3], :result 9, :fn-call-idx 0, :idx 3}
  ;; {:type :fn-return, :coord [], :result 9, :fn-call-idx 0, :idx 4}
  ;; and also a type :fn-unwind for exceptions
  (idx-api/timeline-entry flow-id thread-id 5 :at) ;; could be: :next-out, :next-over, :prev-over, :next, :prev, :at

  ;; if we are dealing with an :expr or :fn-return, we can always look at "it's function" by looking at the :fn-call-idx
  ;; Having the idx of the fn-call entry we can retrieve it, and grab the :form-id from it
  ;; With that form-id we can use :
  (dbg-api/get-form -1340777963)
  ;; which will return like :
  ;; #:form{:id -1340777963,
  ;;      :ns "fs-tester",
  ;;      :form (defn sum [a b] (+ a b)),
  ;;      :def-kind :defn,
  ;;      :file "fs_tester.clj",
  ;;      :line 3}
  ;; Form ids are a hash of the form ast

  ;; With the file, line and :coord fields we should be able to highlight the expression
  ;; The :result field will have the value of the expression to visualize
  ;; And the timeline-entry fn above can be used for the basic stepping

  ;; we can retrieve the timeline to do whatever we want
  (def tl (idx-api/get-timeline thread-id))

  ;; The entries on the timeline are not immutable maps (because of memory footprint reasons), and we can
  ;; access its parts with idx-ap/... functions, or we can make an immutable map out of it by calling

  (-> tl first idx-api/as-immutable)

  )
