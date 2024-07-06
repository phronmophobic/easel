(ns com.phronemophobic.easel.splitpane
  (:require [membrane.skia :as skia]
            [membrane.skia.paragraph :as para]
            [membrane.ui :as ui]
            [membrane.alpha.stretch :as stretch]
            [clojure.zip :as z]
            [com.rpl.specter :as specter]
            [membrane.alpha.component.drag-and-drop :as dnd]
            [com.phronemophobic.membrandt :as ant]

            [membrane.component
             :refer
             [defui defeffect]]))

(def WALK-PANE
  (specter/recursive-path [] p
                          (specter/if-path
                           #(seq (:panes %))
                           (specter/stay-then-continue
                            :panes
                            specter/ALL
                            p)
                           specter/STAY)))

(defn pane-zip [pane]
  (z/zipper #(seq (:panes %))
            :panes
            #(assoc %1 :panes (vec %2))
            pane))

(defn remove-child-pane-by-id
  "Removes any children with :id `id`. Does not recurse."
  [pane id]
  (specter/setval [:panes specter/ALL #(= id (:id %))]
                  specter/NONE
                  pane))

(defn delete-pane-by-pred
  "Finds first descendent pane that returns true for `(pred pane)`

  Deletes and recursively keeps removing nodes as long as the parent node is an only child.

  All nodes must have unique `:id`s."
  [pane pred]
  (let [zpane (pane-zip pane)

        ztarget (loop [zip zpane]
                  (if (z/end? zip)
                    nil
                    (if (pred (z/node zip))
                      zip
                      (recur (z/next zip)))))]
    (if ztarget
      (if-let [zparent (z/up ztarget)]
        (loop [zparent (z/edit zparent
                               #(remove-child-pane-by-id % (:id (z/node ztarget))))]
          (let [zgparent (z/up zparent)]
            (if (and zgparent (empty? (:panes (z/node zparent))))
              ;; if zparent has no siblings. remove and recur
              (recur (z/edit zgparent
                             #(remove-child-pane-by-id % (:id (z/node zparent)))))
              (z/root zparent))))
        ;; else no parent
        nil)
      ;; else not found
      pane)))

(defn delete-pane-by-id
  "Finds first descendent pane with :id `id`.

  Deletes and recursively keeps removing nodes as long as the parent node is an only child.

  All nodes must have unique `:id`s."
  [pane id]
  (delete-pane-by-pred pane #(= id (:id %))))

(defn find-pane-by-pred [pane pred]
  (specter/select-one [WALK-PANE (specter/pred pred)]
                      pane))

(defn find-pane-by-id [pane id]
  (find-pane-by-pred pane #(= id (:id %))))

(defn edit-pane-by-id [pane id f]
  (specter/transform [WALK-PANE #(= id (:id %))]
                     f
                     pane)
  )

(comment
  (delete-pane-by-id  {:panes [{:id 42
                                :panes [{:id 32}]}]}
                      32)

  (delete-pane-by-id  {:panes [{:id 42
                                :panes [{:id 32}
                                        ]}]}
                      32)
  ,)

(defmulti get-size :direction)
(defmulti get-cross-size :direction)
(defmulti get-stretch :direction)

(defmethod get-size :row [m]
  (or (:width m)
      (:flex-layout.stretch/width m)))
(defmethod get-size :default [m]
  (or (:width m)
      (:flex-layout.stretch/width m)))
(defmethod get-size :column [m]
  (or (:height m)
      (:flex-layout.stretch/height m)))

(defn set-size [m direction size]
  (let [k (if (= direction :column)
            :height
            :width)]
   (assoc m k size)))
(defn set-cross-size [m direction size]
  (let [k (if (= direction :column)
            :width
            :height)]
    (assoc m k size)))

(defmethod get-cross-size :row [m]
  (or (:height m)
      (:flex-layout.stretch/height m)))
(defmethod get-cross-size :default [m]
  (or (:height m)
      (:flex-layout.stretch/height m)))
(defmethod get-cross-size :column [m]
  (or (:width m)
      (:flex-layout.stretch/width m)))

(defmethod get-stretch :row [m]
  (:flex.grow/width m 1))
(defmethod get-stretch :default [m]
  (:flex.grow/width m 1))
(defmethod get-stretch :column [m]
  (:flex.grow/height m 1))

(defn stack-layout
  ([direction]
   (stack-layout direction 0))
  ([direction initial-offset]
   (let [offset* (volatile! initial-offset)
         get-size (if (= :column direction)
                    :height
                    :width)
         set-coord (if (= :column direction)
                     #(assoc %1 :y %2 :x 0)
                     #(assoc %1 :x %2 :y 0))]
     (fn [rf]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [offset @offset*]
            (vreset! offset* (+ offset (get-size input)))
            (rf result (set-coord input offset)))))))))

(defn add-child [pane child]
  (update pane :panes
          (fn [panes]
            (conj (or panes [])
                  child))))

(defn layout-pane
  "Returns pane structure with :width key filled in"
  [pane]
  (let [subpanes (:panes pane)]
    (if (seq subpanes)
      (let [size (get-size pane)
            cross-size (get-cross-size pane)
            direction (:direction pane)
            ;; assume no fixed sizes for now
            stretch-total (transduce
                           (map get-stretch)
                           +
                           0
                           subpanes)]
        (assoc pane
               :panes (into []
                            (comp
                             (map (fn [pane]
                                    (-> pane
                                        (set-size direction (* size (/ (get-stretch pane) stretch-total)))
                                        (set-cross-size direction cross-size))))
                             (stack-layout (:direction pane))
                             (map layout-pane))
                            subpanes)))
      ;; leaf node with no children
      pane)))

(defn layout-pane-nested
  "Returns pane structure with :width key filled in. Leaves space for a top bar"
  [pane top-bar-height]
  (let [subpanes (:panes pane)]
    (if (seq subpanes)
      (let [size (get-size pane)
            cross-size (get-cross-size pane)

            column? (= :column (:direction pane))
            [size cross-size] (if column?
                                [(max 0 (- size top-bar-height))
                                 cross-size]
                                [size
                                 (max 0 (- cross-size top-bar-height))])

            direction (:direction pane)
            ;; assume no fixed sizes for now
            stretch-total (transduce
                           (map get-stretch)
                           +
                           0
                           subpanes)]
        (assoc pane
               :panes (into []
                            (comp
                             (map (fn [pane]
                                    (-> pane
                                        (set-size direction (* size (/ (get-stretch pane) stretch-total)))
                                        (set-cross-size direction cross-size))))
                             (if column?
                               (comp (stack-layout (:direction pane) top-bar-height)
                                     (map (fn [pane]
                                            (assoc pane :x 0))))
                               (comp (stack-layout (:direction pane))
                                     (map (fn [pane]
                                            (assoc pane :y top-bar-height)))))
                             (map #(layout-pane-nested % top-bar-height)))
                            subpanes)))
      ;; leaf node with no children
      pane)))

(defn pane->view [pane]
  (loop [view (transient [])
         q (list [pane 0 0])]
    (if (seq q)
      (let [[pane x y] (first q)
            subpanes (:panes pane)]
        (if (seq subpanes)
          (recur view
                 (into (next q)
                       (eduction
                        (map (fn [subpane]
                               [subpane (+ x (:x pane 0)) (+ y (:y pane 0))]))
                        subpanes)))

          (recur (conj! view (ui/translate (+ x (:x pane 0))
                                           (+ y (:y pane 0))
                                           (:view pane)))
                 (next q))))
      

      (persistent! view))))

(defn flatten-pane
  "Returns all panes and subpanes with `:x` and `:y` in the coordinate frame of the root pane."
  [pane]
  (loop [view (transient [])
         q (list [pane 0 0])]
    (if (seq q)
      (let [[pane x y] (first q)
            subpanes (:panes pane)]
        (if (seq subpanes)
          (recur view
                 (into (next q)
                       (eduction
                        (map (fn [subpane]
                               [subpane (+ x (:x pane 0)) (+ y (:y pane 0))]))
                        subpanes)))

          (recur (conj! view
                        (assoc pane
                               :x (+ x (:x pane 0))
                               :y (+ y (:y pane 0))))
                 (next q))))
      

      (persistent! view))))

(defn flatten-pane-nested
  "Returns all panes and subpanes with `:x` and `:y` in the coordinate frame of the root pane.
  As opposed to `flatten-pane` also includes branches."
  [pane]
  (loop [view (transient [])
         q (list [pane 0 0])]
    (if (seq q)
      (let [[pane x y] (first q)
            subpanes (:panes pane)
            pane-x (+ x (:x pane 0))
            pane-y (+ y (:y pane 0))]
        (if (seq subpanes)
          (recur (conj! view
                        (assoc pane :x pane-x :y pane-y))
                 (into (next q)
                       (eduction
                        (map (fn [subpane]
                               [subpane pane-x pane-y]))
                        subpanes)))

          (recur (conj! view
                        (assoc pane
                               :x pane-x
                               :y pane-y))
                 (next q))))
      

      (persistent! view))))





(defeffect ::delete-pane [{:keys [$root id]}]
  (dispatch! :update $root
             #(delete-pane-by-id % id)))

(defn toggle-direction [pane]
  (let [direction (:direction pane)]
    (assoc pane :direction (if (= direction :column)
                             :row
                             :column))))
(defeffect ::update-pane [{:keys [$root id f]}]
  (dispatch! :update
             $root
             (fn [root]
               (specter/transform [WALK-PANE #(= id (:id %))]
                                  f
                                  root))))

(defui pane-test [{:keys [pane]}]
  (ui/vertical-layout
   (ant/button {:text "add pane"
                :on-click
                (fn []
                  [[:update $pane add-child {:id (random-uuid)}]])})
   (into []
         (map (fn [{:keys [x y width height id direction]}]
                (ui/translate (inc x) (inc y)
                              [
                               (ui/flex-layout
                                [(ui/on-click
                                  (fn []
                                    [[::update-pane {:$root $pane
                                                     :id id
                                                     :f toggle-direction}]])
                                  (para/paragraph
                                   (if (= direction :column)
                                     "↕️"
                                     "↔️")))
                                 (ui/on-click
                                  (fn []
                                    [[::update-pane {:$root $pane
                                                     :id id
                                                     :f #(-> %
                                                             (add-child {:id (random-uuid)})
                                                             (add-child {:id (random-uuid)}))}]])
                                  (para/paragraph
                                   "+"))
                                 (ui/on-click
                                  (fn []
                                    [[::delete-pane {:$root $pane
                                                     :id id}]])
                                  (para/paragraph
                                   "X"))]
                                {:direction (if (= direction :column)
                                              :column
                                              :row)})
                               (ui/filled-rectangle [0.4 0.4 0.4 0.4]
                                                    (- width 2)
                                                    (- height 2))])))
         (-> pane
             layout-pane
             flatten-pane))))


(comment
  (def app-state (atom {:pane {:width 300
                               :height 400
                               :id (random-uuid)}}))
  (reset! app-state
          {:pane {:width 300
                  :height 400
                  :id (random-uuid)}}
          )

  (skia/run
    (membrane.component/make-app #'pane-test app-state))

  (layout-pane
   {:panes [{} {} {:panes [{}]}]
    :width 100})

  ,)






