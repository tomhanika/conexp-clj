(ns conexp.layout.force
  (:use conexp.base
        conexp.fca.lattices
	conexp.layout.util
	[conexp.math.util :only (pos-infinity)]
	conexp.math.optimize
	clojure.contrib.pprint))

;;; Force layout as described by C. Zschalig

;; Helpers

(defn square
  "Squares."
  [x]
  (* x x))

(defn- line-length-squared
  "Returns the square of the length of the line between [x_1, y_1] and [x_2, y_2]."
  [[x_1, y_1] [x_2, y_2]]
  (+ (square (- x_1 x_2))
     (square (- y_1 y_2))))

;; Repulsive Energy

(defn- node-line-distance
  "Returns the distance from node to the line between [x_1, y_1] and [x_2, y_2]."
  [[x, y] [[x_1, y_1] [x_2, y_2]]]
  ;; dump school math follows
  (if (and (= x_1 x_2) (= y_1 y_2))
    (Math/sqrt (line-length-squared [x, y] [x_1, y_1]))
    (let [;; position of projection of [x y] onto the line, single coordinate
	  r (/ (+ (* (- x x_1)
		     (- x_2 x_1))
		  (* (- y y_1)
		     (- y_2 y_1)))
	       (+ (square (- x_2 x_1))
		  (square (- y_2 y_1))))]
      (Math/sqrt (line-length-squared [x, y]
				      (cond
				       (<= r 0) [x_1, y_1], ; node is behind [x_1, y_1]
				       (>= r 1) [x_2, y_2], ; node is ahead [x_2, y_2]
				       :else [(+ x_1 (* r (- x_2 x_1))), (+ y_1 (* r (- y_2 y_1)))]))))))

(defn- repulsive-energy
  "Computes the repulsive energy of the given layout."
  [[node-positions node-connections]]
  (try
   (reduce (fn [sum v]
	     (+ sum
		(reduce (fn [sum [x y]]
			  (+ sum
			     (if (or (= x v) (= y v))
			       0
			       (/ 1 (node-line-distance (node-positions v)
							[(node-positions x), (node-positions y)])))))
			0
			node-connections)))
	   0
	   (keys node-positions))
   (catch ArithmeticException e
     pos-infinity)))

;; Attractive Energy

(defn- attractive-energy
  "Computes the attractive energy of the given layout."
  [[node-positions node-connections]]
  (reduce (fn [sum [x y]]
	    (+ sum
	       (line-length-squared (node-positions x) (node-positions y))))
	  0
	  node-connections))

;; Gravitative Energy

(defn- phi
  "Returns the angle between a inf-irreducible node [x_1,y_1]
  and its upper neighbor [x_2, y_2]."
  [[x_1,y_1] [x_2,y_2]]
  (let [result (Math/atan2 (- y_2 y_1) (- x_2 x_1))]
    (min (max 0 result) Math/PI)))

(defn- gravitative-energy
  "Returns the gravitative energy of the given layout. inf-irrs are
  the infimum irreducible elements, upper-neighbors is a hash of
  infimum-irreducible elements to their upper neighbour, both given
  some perfomances sake."
  [[node-positions node-connections] inf-irrs upper-neighbours]
  (let [phi_0 (/ Math/PI (+ 1 (count inf-irrs))),
	E_0 (+ (- phi_0) (- (* (Math/sin phi_0) (Math/cos phi_0)))),
	E_1 (+ E_0 Math/PI)]
    (try
     (reduce (fn [sum n]
	       (+ sum
		  (let [phi_n (phi (node-positions n)
				   (node-positions (upper-neighbours n)))]
		    (cond
		     (<= 0 phi_n phi_0) (+ phi_n
					   (/ (square (Math/sin phi_0))
					      (Math/tan phi_n))
					   E_0),
		     (<= (- Math/PI phi_0) phi_n Math/PI) (+ (- phi_n)
							     (/ (- (square (Math/sin phi_0)))
								(Math/tan phi_n))
							     E_1),
		     :else 0))))
	     0
	     inf-irrs)
     (catch ArithmeticException e
       pos-infinity))))

;; Overall Energy

(def *repulsive-energy-amount* 1)
(def *attractive-energy-amount* 1)
(def *gravitative-energy-amount* 1)

(defn- layout-energy
  "Returns the overall energy of the given layout."
  [layout inf-irrs upper-neighbours]
  (+ (* *repulsive-energy-amount* (repulsive-energy layout))
     (* *attractive-energy-amount* (attractive-energy layout))
     (* *gravitative-energy-amount* (gravitative-energy layout
							inf-irrs
							upper-neighbours))))

(defn- energy-by-inf-irr-positions
  "Returns a function calculating the energy of an attribute additive
  layout of lattice given by the positions of the infimum irreducible
  elements. seq-of-inf-irrs gives the order of the infimum irreducible
  elements."
  [lattice seq-of-inf-irrs]
  (let [upper-neighbours (hash-by-function (fn [n]
					     (first (lattice-upper-neighbours lattice n)))
					   seq-of-inf-irrs)]
    (fn [& point-coordinates]
      (let [points (partition 2 point-coordinates),
	    inf-irr-placement (apply hash-map
				     (interleave seq-of-inf-irrs
						 points))]
	(layout-energy (layout-by-placement lattice inf-irr-placement)
		       seq-of-inf-irrs
		       upper-neighbours)))))

;; Force Layout

(defn force-layout
  "Improves given layout with force layout."
  [lattice layout]
  (let [;; get positions of inf-irreducibles from layout as starting point
	inf-irrs (seq (lattice-inf-irreducibles lattice)),
	node-positions (first layout),
	inf-irr-points (map node-positions inf-irrs),

	;; move top element to [0,0], needed by layout-by-placement
	[top_x, top_y] (node-positions (lattice-one lattice)),
	inf-irr-points (map (fn [[x y]]
			      [(- x top_x), (- y top_y)])
			    inf-irr-points),

	;; minimize layout energy with above placement as initial value
	[new-points, value] (minimize (energy-by-inf-irr-positions lattice inf-irrs)
				      (apply concat inf-irr-points)),

	;; make hash
	point-hash (apply hash-map (interleave inf-irrs
					       (partition 2 new-points)))]

    (pprint (apply hash-map (interleave inf-irrs inf-irr-points)))
    (pprint point-hash)
    (pprint value)

    ;; compute layout given by the result
    (layout-by-placement lattice point-hash)))

;;;

(comment "For Testing"

(require 'conexp :reload-all)
(def lat (conexp/concept-lattice (conexp/rand-context #{1 2 3 4 5} 0.4)))
(defn simple-layered-force-layout [lat]
  (force-layout lat (conexp/simple-layered-layout lat)))
(conexp/draw-lattice lat simple-layered-force-layout)

)
;;;

nil
