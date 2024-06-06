;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.util
  "Utilities for computing lattice layouts."
  (:require [conexp.base :refer :all]
            [conexp.math.algebra :refer :all]
            [conexp.fca.lattices :refer :all]
            [conexp.fca.protoconcepts :refer :all]
            [conexp.fca.posets :refer :all]
            [conexp.layouts.base :refer :all]
            [conexp.util.graph :as graph])
  (:import [conexp.fca.posets Poset]
           [conexp.fca.lattices Lattice]
           [conexp.fca.protoconcepts Protoconcepts]))

;;;

(defn enclosing-rectangle
  "Returns left lower and right upper edge of the minimal rectangle
  containing all points. The coordinates are given in a vector of the
  form [x_min y_min x_max y_max]."
  [points]
  (when (empty? points)
    (illegal-argument (str "Cannot scale empty sequence of points.")))
  (let [[x0 y0] (first points)]
    (loop [x_min  x0,
           y_min  y0,
           x_max  x0,
           y_max  y0,
           points (next points)]
      (if points
        (let [[x y] (first points)]
          (recur (min x x_min)
                 (min y y_min)
                 (max x x_max)
                 (max y y_max)
                 (next points)))
        [x_min y_min x_max y_max]))))

(defn- scale-points-to-rectangle
  "Scales the collection of points such that they fit in the
  rectangle given by [x1 y1] and [x2 y2]."
  [[x1 y1] [x2 y2] points]
  (let [[x_min y_min x_max y_max] (enclosing-rectangle points),
        [a_x, b_x] (if (= x_min x_max)
                     [0, (/ (+ x1 x2) 2)]
                     (let [slope (/ (- x1 x2) (- x_min x_max))]
                       [slope, (- x1 (* slope x_min))])),
        [a_y, b_y] (if (= y_min y_max)
                     [0, (/ (+ y1 y2) 2)]
                     (let [slope (/ (- y1 y2) (- y_min y_max))]
                       [slope, (- y1 (* slope y_min))]))]
    (map (fn [[x y]]
           [(+ (* a_x x) b_x), (+ (* a_y y) b_y)])
         points)))

(defn scale-layout
  "Scales given layout to rectangle [x1 y1], [x2 y2]."
  [[x1 y1] [x2 y2] layout]
  (let [points (seq (positions layout))]
    (make-layout-nc (poset layout)
                    (zipmap (map first points)
                            (scale-points-to-rectangle [x1 y1] [x2 y2]
                                                       (map second points)))
                    (connections layout)
                    (upper-labels layout)
                    (lower-labels layout)
                    (valuations layout))))

;;;

(defn- lattice->graph
  "Converts given lattice to it's corresponding graph with loops
  removed."
  [lattice]
  (-> (graph/make-directed-graph
        (base-set lattice)
        (memoize
          (fn [x]
            (let [order (order lattice)]
              (filter #(order [x %]) (base-set lattice))))))
      graph/remove-loops))

(defn layers
  "Returns the layers of the given lattice, that is sequence of points
  with equal depth, starting with the lowest layer."
  [lattice]
  (reverse (graph/dependency-list (lattice->graph lattice))))

(defn- edges-by-border
  "Computes edges of a lattice by a border algorithm as given by José
  L. Balcázar and Cristina Tîrnǎucǎ."
  [lattice]
  (let [sup (sup lattice)
        L   (reverse (topological-sort (order lattice) (base-set lattice))),
        B   #{}
        H   #{}]
    (loop [L L, B B, H H]
      (if (not-empty L)
        (let [x     (first L),
              cover (partial-min (order lattice)
                                 (map #(sup x %) B))]
          (recur (rest L)
                 (reduce! (fn [set y]
                            (disj! set y))
                          (conj B x)
                          cover)
                 (reduce! (fn [set z]
                            (conj! set [x z]))
                          H
                          cover)))
        H))))

(defmulti edges
  "Returns a sequence of pairs of vertices of poset (or lattice) which 
  are directly neighboured in poset."
  (fn [poset] (type poset)))

(defn- poset-edges
  [poset]
  (into #{} 
        (filter #(directly-neighboured? poset (first %) (second %)) 
                (for [x (base-set poset)
                      y (base-set poset)
                      :when ((order poset) x y)]
                  [x y]))))

(defmethod edges Poset
  [poset]
  (poset-edges poset))

(defmethod edges Lattice
  [lattice]
  (edges-by-border lattice))

(defmethod edges Protoconcepts
  [protoconcepts]
  (poset-edges protoconcepts))

(defn top-down-elements-in-layout
  "Returns the elements in layout ordered top down."
  [layout]
  (let [graph (graph/make-directed-graph
                (keys (positions layout))
                (memoize (fn [x]
                           (map second (filter (fn [[a b]] (= a x))
                                               (connections layout))))))]
    (apply concat (graph/dependency-list graph))))

;;; grid adjustment

(defn- fit-point-to-grid
  "Moves given point [x y] to the next point on the grid given by the
  origin [x_origin y_origin] and the paddings x_pad and y_pad."
  [[x_origin y_origin] x_pad y_pad [x y]]
  [(if (> x_pad 0)
     (+ x_origin (* x_pad (int (+ 0.5 (/ (- x x_origin) x_pad)))))
     (+ x_origin (* x_pad (int (+ 0.5 (- x x_origin)))))),
   (+ y_origin (* y_pad (int (+ 0.5 (/ (- y y_origin) y_pad)))))])

(defn fit-layout-to-grid
  "Specifies a grid by a origin point and paddings x_pad and y_pad
  between two adjacent grid lines in x and y direction
  respectively. Returns the layout resulting from adjusting the given
  layout on this layout."
  [layout origin x_pad y_pad]
  (let [fit-point (partial fit-point-to-grid origin x_pad y_pad)]
    (update-positions layout
                      (persistent!
                       (reduce (fn [map [name [x y]]]
                                 (assoc! map name (fit-point [x y])))
                               (transient {})
                               (positions layout))))))

(defn discretize-layout
  "Adjusts the given layout to fit on a grid of x_cells cells in the x
  coordinate and y_cells cells in the y coordinate."
  [layout x_cells y_cells]
  (assert (< 0 x_cells))
  (assert (< 0 y_cells))
  (let [[x_min y_min x_max y_max] (enclosing-rectangle (vals (positions layout))),
        origin [x_min y_min],
        x_pad  (/ (- x_max x_min) x_cells),
        y_pad  (/ (- y_max y_min) y_cells)]
    (fit-layout-to-grid layout origin x_pad y_pad)))

(defn discretize-layout-values
  "Adjusts the given layout to fit on a grid of x_cells cells in the x
  coordinate and y_cells cells in the y coordinate using only discrete 
  values."
  [layout x_cells y_cells]
  (let [[x_min y_min x_max y_max] 
          (enclosing-rectangle (vals (positions layout)))
        origin [x_min y_min]
        x_pad (if (> x_cells 0)
                 (/ (- x_max x_min) x_cells)
                 1)
        y_pad (if (> y_cells 0)
                (/ (- y_max y_min) y_cells)
                1)
        discrete-layout 
          (fit-layout-to-grid layout origin x_pad y_pad)
        x_fac  (/ 1 x_pad)
        y_fac  (/ 1 y_pad)]
    (update-positions discrete-layout
                      (persistent!
                       (reduce (fn [map [name [x y]]]
                                 (assoc! map name [(int (+ 1 (* x x_fac))) 
                                                   (int (+ 1 (* y y_fac)))]))
                               (transient {})
                               (positions layout))))))

;;;

(defn compute-below-above
  "Computes maps mapping endpoints of edges to all elements above and
  below it. Returns [below,above]."
  [edges]
  (loop [above (transient {}),
         below (transient {}),
         edges edges]
    (if (empty? edges)
      [(persistent! below), (persistent! above)]
      (let [[a b]   (first edges),
            above   (assoc! above a
                            (conj (get above a #{}) a b)),
            above   (assoc! above b
                            (conj (get above b #{}) b)),
            below   (assoc! below b
                            (conj (get below b #{}) b a)),
            below   (assoc! below a
                            (conj (get below a #{}) a)),
            above-b (above b),
            below-a (below a),
            above   (reduce (fn [above x]
                              (assoc! above x
                                      (into (above x) above-b)))
                            above
                            below-a),
            above   (assoc! above a
                            (into (above a) (above b))),
            below   (reduce (fn [below x]
                              (assoc! below x
                                      (into (below x) below-a)))
                            below
                            above-b),
            below   (assoc! below b
                            (into (below b) (below a)))]
        (recur above below (rest edges))))))

;; Layouts for Posets, using Dedekind MacNeille completion

(defn- order-embedding
  "Return an order embedding using the Dedekind MacNeille completion."
  [poset]
  (into {} 
        (map #(vector [(order-ideal poset #{%}) 
                       (order-filter poset #{%})]
                      %) 
             (base-set poset))))

(defn- poset-layout->lattice-layout
  "The poset of the  given layout is transformed into a lattice, using the Dedekind MacNeille completion."
  [layout]
  (let [poset (poset layout)]
    (if (= Lattice (type poset))
      layout
      (let [lattice (concept-lattice (poset-context poset)) ;; Dedekind MacNeille completion
            embedding (order-embedding poset)
            new-base-set (map #(get embedding % %) (base-set lattice))
            max-y-position (apply max (map second (vals (positions layout))))]
        (make-layout (merge 
                      (into {} (map #(vector % [0 (inc max-y-position)])
                                    new-base-set))
                      (positions layout))
                     (map #(vector 
                            (get embedding (first %) (first %))
                            (get embedding (second %) (second %)))
                          (edges lattice)))))))

(defn- lattice-layout->poset-layout
  "The lattice of the given layout is transformed into a poset. Only the given nodes remain in the layout."
  [layout nodes]
  (let [new-positions (into {} (filter #(contains? nodes (key %))
                                       (positions layout)))
        new-connections (into [] (filter #(and (contains? nodes (first %)) 
                                               (contains? nodes (second %)))
                                         (connections layout)))]
    (make-layout new-positions new-connections)))

(defn layout-fn-on-poset
  "The poset of the given layout is transformed into a lattice, using 
  the Dedekind MacNeille completion. After computing the new layout 
  with the given layout-fn, the layout is re-transformed to the previous
  layout by deleting all nodes added in the Dedekind MacNeille completion."
  ([layout-fn layout]
   (let [lattice-layout (poset-layout->lattice-layout layout),
         new-layout (layout-fn lattice-layout)]
     (lattice-layout->poset-layout new-layout (nodes layout))))
  ([layout-fn layout args]
   (let [lattice-layout (poset-layout->lattice-layout layout),
         new-layout (layout-fn lattice-layout args)]
     (lattice-layout->poset-layout new-layout (nodes layout)))))

;;;

nil
