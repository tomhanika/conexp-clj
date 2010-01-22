(ns conexp.layout.util
  (:use conexp.base
	conexp.fca.lattices
	[clojure.contrib.graph :exclude (transitive-closure)]))

(update-ns-meta! conexp.layout.util
  :doc "Utilities for computing lattice layouts.")


;;;

(defn scale-points-to-rectangle
  "Scales the collection of points such that they fit in the
  rectangle given by [x1 y1] and [x2 y2]."
  [[x1 y1] [x2 y2] points]
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
					       (rest points))))),
	a_x (/ (- x1 x2) (- x_min x_max 0.1)), ; -0.1 for (= x_min x_max)
	b_x (- x1 (* a_x x_min)),
	a_y (/ (- y1 y2) (- y_min y_max 0.1)),
	b_y (- y1 (* a_y y_min))]
    (map (fn [[x y]]
	   [(+ (* a_x x) b_x), (+ (* a_y y) b_y)])
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

(defn edges
  "Returns a sequence of pairs of vertices of lattice which are
  directly neighbored in lattice."
  [lattice]
  (for [x (base-set lattice),
	y (base-set lattice),
	:when (directly-neighboured? lattice x y)]
    [x y]))


;;; inf-irreducible additive layout

(defn- vector-plus
  "Implements pointwise plus for vectors."
  [vec1 vec2]
  (vec (map + vec1 vec2)))

(defn- place-inf-irr-by-coatoms
  "Places all other inf-irreducible elements of lattice by placement
  of coatoms."
  [lattice placement]
  (let [inf-irrs (lattice-inf-irreducibles lattice),

	;; determine minimal distance
	;; is this a good idea? -> lattice gets "noisy" downwards
	offset (fn []
		 [(- (rand) 0.5), -1]),

	pos (memo-fn pos [v]
	      (if (contains? placement v)
		(get placement v)
		(reduce (fn [[x y] w]
			  (if (or (= v w)
				  (not ((order lattice) [v w])))
			    [x y]
			    (vector-plus [x y] (pos w))))
			(offset)
			inf-irrs)))]
    (hash-by-function pos inf-irrs)))

(defn layout-by-placement
  "Computes additive layout of lattice by given positions of the keys
  of placement. The values of placement should be the positions of the
  corresponding keys. Top element will be at [0,0], if not explicitly given."
  [lattice placement]
  (let [pos (fn pos [v]
	      (get placement v
		   (reduce (fn [p w]
			     (if ((order lattice) [v w])
			       (vector-plus p (placement w))
			       p))
			   [0 0]
			   (keys placement)))),
	overall-placement (hash-by-function pos (base-set lattice))]
    [ overall-placement, (edges lattice) ]))

;;;

nil
