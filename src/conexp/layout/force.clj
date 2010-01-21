(ns conexp.layout.force
  (:use conexp.fca.lattices
	conexp.layout.util
	[conexp.math.util :only (pos-infinity)]
	conexp.math.optimize))

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
  ;; distance is area of parallelogramm divided by the length of [[x_1, y_2], [x_2, y_2]]
  (/ (square (- (* (- y_2 y_1)
		   (- x x_1))
		(* (- x_2 x_1)
		   (- y y_1))))
     (Math/sqrt (+ (square (- x_2 x_1))
		   (square (- y_2 y_1))))))

(defn- repulsive-energy
  "Computes the repulsive energy of the given layout."
  [[node-positions node-connections]]
  (reduce (fn [sum v]
	    (+ sum
	       (reduce (fn [sum [x y]]
			 (if (or (= x v) (= y v))
			   sum
			   (let [distance (node-line-distance (node-positions v)
							      [(node-positions x), (node-positions y)])]
			     (if (zero? distance)
			       pos-infinity
			       (+ sum
				  (/ 1 distance))))))
		       0
		       node-connections)))
	  0
	  (keys node-positions)))

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

(defn- gravitative-energy [[node-positions node-connections]]
  0)

;; Overall Energy

(defn- layout-energy
  "Returns the overall energy of the given layout. The coefficients r,
  a and g give the amount of repulsive, attractive and gravitative
  energy, respectively."
  [r a g layout]
  (+ (* r (repulsive-energy layout))
     (* a (attractive-energy layout))
     (* g (gravitative-energy layout))))

(defn- energy-by-inf-irr-positions
  "Returns a function calculating the energy of an attribute additive
  layout of lattice given by the positions of the infimum irreducible
  elements. seq-of-inf-irrs gives the order of the infimum irreducible
  elements."
  [lattice seq-of-inf-irrs]
  (fn [& point-coordinates]
    (let [points (partition 2 point-coordinates),
	  inf-irr-placement (apply hash-map
				   (interleave seq-of-inf-irrs
					       points))]
      (layout-energy 1			; repulsive component
		     1			; attractive component
		     1			; gravitative component
		     (layout-by-inf-irr-placement lattice
						  inf-irr-placement)))))

;; Force Layout

(defn force-layout
  "Improves given layout with force layout."
  [lattice layout]
  (let [;; get positions of inf-irreducibles from layout as starting point
	inf-irrs (seq (lattice-inf-irreducibles lattice)),
	node-positions (first layout),
	inf-irr-points (map node-positions inf-irrs),

	;; minimize layout energy with above placement as initial value
	[new-points, value] (minimize (energy-by-inf-irr-positions lattice inf-irrs)
				      (apply concat inf-irr-points)),

	;; move top element to [0,0], needed by layout-by-inf-irr-placement
	[top_x, top_y] (node-positions (lattice-one lattice)),
	point-hash (apply hash-map
			  (interleave inf-irrs
				      (map (fn [[x y]]
					     [(- x top_x), (- y top_y)])
					   (partition 2 new-points))))]

    ;; compute layout given by the result
    (layout-by-inf-irr-placement lattice point-hash)))

;;;

nil
