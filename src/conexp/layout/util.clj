;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layout.util
  (:use conexp.util
	[conexp.layout.base :only (make-layout, positions, connections)]
	[conexp.fca.lattices :only (base-set, directly-neighboured?, order)])
  (:use [clojure.contrib.graph :only (directed-graph, dependency-list)]))

(update-ns-meta! conexp.layout.util
  :doc "Utilities for computing lattice layouts.")


;;;

(defn edges-of-points
  "Returns left lower and right upper edge of the minimal rectangle
  containing all points. The coordinates are given in a vector of the
  form [x_min y_min x_max y_max]."
  [points]
  (if (empty? points)
    (illegal-argument (str "Cannot scale empty sequence of points.")))
  (let [[x0 y0] (first points),
	[x_min y_min x_max y_max] (loop [x_min x0
					 y_min y0
					 x_max x0
					 y_max y0
					 points (rest points)]
				    (if (empty? points)
				      [x_min y_min x_max y_max]
				      (let [[x y] (first points)]
					(recur (min x x_min)
					       (min y y_min)
					       (max x x_max)
					       (max y y_max)
					       (rest points)))))]
    [x_min y_min x_max y_max]))

(defn- scale-points-to-rectangle
  "Scales the collection of points such that they fit in the
  rectangle given by [x1 y1] and [x2 y2]."
  [[x1 y1] [x2 y2] points]
  (let [[x_min y_min x_max y_max] (edges-of-points points),
	a_x (/ (- x1 x2) (- x_min x_max 0.1)), ; -0.1 for (= x_min x_max)
	b_x (- x1 (* a_x x_min)),
	a_y (/ (- y1 y2) (- y_min y_max 0.1)),
	b_y (- y1 (* a_y y_min))]
    (map (fn [[x y]]
	   [(+ (* a_x x) b_x), (+ (* a_y y) b_y)])
	 points)))

(defn scale-layout
  "Scales given layout to rectangle [x1 y1], [x2 y2]. Layout is given
  as a map of points to coordinates and a sequence of connection pairs."
  [[x1 y1] [x2 y2] layout]
  (let [points (seq (positions layout))]
    (make-layout (apply hash-map
			(interleave (map first points)
				    (scale-points-to-rectangle [x1 y1] [x2 y2]
							       (map second points))))
		 (connections layout))))

(defn edges
  "Returns a sequence of pairs of vertices of lattice which are
  directly neighbored in lattice."
  [lattice]
  (for [x (base-set lattice),
	y (base-set lattice),
	:when (directly-neighboured? lattice x y)]
    [x y]))

(defn top-down-elements-in-layout
  "Returns the elements in layout ordered top down."
  [layout]
  (let [graph (struct-map directed-graph
		:nodes (keys (positions layout))
		:neighbors (memoize (fn [x]
				      (map second (filter (fn [[a b]] (= a x))
							  (connections layout))))))]
    (apply concat (dependency-list graph))))

;;;

nil
