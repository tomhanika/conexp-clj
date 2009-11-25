(ns conexp.layout.util
  (:use conexp.base
	conexp.fca.lattices
	[clojure.contrib.graph :exclude (transitive-closure)]))

(defn scale-points-to-rectangle
  "Scales the collection of points such that they fit in the
  rectangle given by [x1 y1] and [x2 y2]."
  [[x1 y1] [x2 y2] points]
  (if (empty? points)
    (illegal-argument (str "Cannot scale empty sequence of points.")))
  (if (or (= x1 x2)
	  (= y1 y2))
    (illegal-argument (str "Cannot scale to rectangle with size 0.")))
  (let [[x0 y0] (first points)
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
					       (rest points)))))
	a_x (/ (- x1 x2) (- x_min x_max 0.1)) ; -0.1 for (= x_min x_max)
	b_x (- x1 (* a_x x_min))
	a_y (/ (- y1 y2) (- y_min y_max 0.1))
	b_y (- y1 (* a_y y_min))]
    (map (fn [[x y]]
	   [(+ (* a_x x) b_x) (+ (* a_y y) b_y)])
	 points)))

(defn scale-layout
  "Scales given layout to rectangle (x1 y1), (x2 y2). Layout is given
  as a map of points to coordinates and a sequence of connection pairs."
  [[x1 y1] [x2 y2] [points-to-coordinates point-connections]]
  (let [points (seq points-to-coordinates)]
    [(apply hash-map (interleave (map first points)
				 (scale-points-to-rectangle [x1 y1] [x2 y2]
							    (map second points))))
     point-connections]))

(defn lattice->graph
  "Converts given lattice to it's corresponding graph with loops
  removed."
  [lattice]
  (remove-loops
   (struct-map directed-graph
     :nodes (base-set lattice)
     :neighbors (memoize
		 (fn [x]
		   (let [order (order lattice)]
		     (filter #(order [% x]) (base-set lattice))))))))

nil
