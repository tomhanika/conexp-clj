(ns conexp.layout.force
  (:use conexp.base
        conexp.fca.lattices
	conexp.layout.util
	[conexp.math.util :only (with-doubles, partial-derivatives)]
	conexp.math.optimize
	clojure.contrib.pprint))

;;; Force layout as described by C. Zschalig

;; Helpers

(defn square
  "Squares."
  [x]
  (with-doubles [x]
    (* x x)))

(defn- line-length-squared
  "Returns the square of the length of the line between [x_1, y_1] and [x_2, y_2]."
  [[x_1, y_1] [x_2, y_2]]
  (with-doubles [x_1, y_1, x_2, y_2]
    (+ (square (- x_1 x_2))
       (square (- y_1 y_2)))))

(defn- distance
  "Returns distance of two points."
  [[x_1 y_1] [x_2 y_2]]
  (with-doubles [x_1 y_1 x_2 y_2]
    (Math/sqrt (+ (square (- x_1 x_2)) (square (- y_1 y_2))))))

(defmacro sum
  "Sums up all values of bindings obtained with expr. See for."
  [bindings expr]
  `(reduce +
	   (double 0)
	   (for ~bindings
	     (double ~expr))))

;; Repulsive Energy and Force

(defn- node-line-distance
  "Returns the distance from node to the line between [x_1, y_1] and [x_2, y_2]."
  [[x, y] [[x_1, y_1] [x_2, y_2]]]
  (with-doubles [x, y, x_1, y_1, x_2, y_2]
    (if (and (= x_1 x_2) (= y_1 y_2))
      (distance [x, y] [x_1, y_1])
      (let [;; position of projection of [x y] onto the line, single coordinate
	    r (/ (+ (* (- x x_1)
		       (- x_2 x_1))
		    (* (- y y_1)
		       (- y_2 y_1)))
		 (+ (square (- x_2 x_1))
		    (square (- y_2 y_1)))),
	    r (min (max r 0) 1)]
	(distance [x, y] [(+ x_1 (* r (- x_2 x_1))),
			  (+ y_1 (* r (- y_2 y_1)))])))))

(defn- repulsive-energy
  "Computes the repulsive energy of the given layout."
  [[node-positions node-connections]]
  (try
   (let [edges (map (fn [[x y]]
		      [x y (get node-positions x) (get node-positions y)])
		    node-connections)]
     (sum [[v pos-v] node-positions,
	   [x y pos-x pos-y] edges
	   :when (not (#{x, y} v))]
	 (/ 1.0
	    (double (node-line-distance pos-v [pos-x, pos-y])))))
   (catch Exception e
     Double/MAX_VALUE)))

(defn- repulsive-force
  "Computes the n-th component of the vector of repulsive forces
  acting on the inf-irreducible elements of layout. The vector
  itself has 2n entries."
  [layout inf-irrs n]
  (throw (UnsupportedOperationException. "Not yet implemented.")))

;; Attractive Energy and Force

(defn- attractive-energy
  "Computes the attractive energy of the given layout."
  [[node-positions node-connections]]
  (sum [[x y] node-connections]
       (line-length-squared (node-positions x) (node-positions y))))

(defn- attractive-force
  "Computes the n-th component of the vector of attractive forces
  acting on the inf-irreducible elements of layout. The vector
  itself has 2n entries."
  [layout inf-irrs n]
  (throw (UnsupportedOperationException. "Not yet implemented.")))

;; Gravitative Energy and Force

(defn- phi
  "Returns the angle between a inf-irreducible node [x_1,y_1]
  and its upper neighbor [x_2, y_2]."
  [[x_1,y_1] [x_2,y_2]]
  (with-doubles [x_1, y_1, x_2, y_2]
    (let [result (Math/atan2 (- y_2 y_1) (- x_2 x_1))]
      (min (max 0 result) Math/PI))))

(defn- gravitative-energy
  "Returns the gravitative energy of the given layout."
  [[node-positions node-connections] {inf-irrs :inf-irrs,
				      upper-neighbours :upper-neighbours-of-inf-irrs}]
  (let [phi_0 (double (/ Math/PI (+ 1 (count inf-irrs)))),
	E_0   (double (+ (- phi_0) (- (* (Math/sin phi_0) (Math/cos phi_0))))),
	E_1   (double (+ E_0 Math/PI))]
    (try
     (sum [n inf-irrs]
	  (let [phi_n (double (phi (node-positions n)
				   (node-positions (upper-neighbours n))))]
	    (cond
	     (<= 0 phi_n phi_0)
	     (+ phi_n
		(/ (square (Math/sin phi_0))
		   (Math/tan phi_n))
		E_0),

	     (<= (- Math/PI phi_0) phi_n Math/PI)
	     (+ (- phi_n)
		(/ (- (square (Math/sin phi_0)))
		   (Math/tan phi_n))
		E_1),

	     :else 0)))
     (catch Exception e
       Double/MAX_VALUE))))

(defn- gravitative-force
  "Computes the n-th component of the vector of gravitative forces
  acting on the inf-irreducible elements of layout. The vector itself
  has 2n entries."
  [layout inf-irrs n]
  (throw (UnsupportedOperationException. "Not yet implemented.")))

;; Overall Energy and Force

(def *repulsive-amount* 1)
(def *attractive-amount* 1)
(def *gravitative-amount* 1)

(defn layout-energy
  "Returns the overall energy of the given layout."
  [layout additional-information]
  (+ (* *repulsive-amount* (repulsive-energy layout))
     (* *attractive-amount* (attractive-energy layout))
     (* *gravitative-amount* (gravitative-energy layout additional-information))))

(defn- layout-force
  "Computes overall force _component_ of index n in the inf-irreducible elements."
  [layout inf-irrs n additional-information]
  (throw (UnsupportedOperationException. "Not yet implemented.")))

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
		       {:inf-irrs seq-of-inf-irrs,
			:upper-neighbours-of-inf-irrs upper-neighbours})))))

(defn- force-by-inf-irr-positions
  "Returns a function representing the forces on the infimum
  irreducible elements of lattice. The function returned takes as
  input and index representing the coordinate to derive at and returns
  itself a function from a placement of the inf-irreducible elements
  to the force component given by the index. seq-of-inf-irrs gives the
  order of the inf-irreducible elements."
  [lattice seq-of-inf-irrs]
  (throw (UnsupportedOperationException. "Not yet implemented.")))

;; Force Layout

(defn force-layout
  "Improves given layout with force layout."
  [layout]
  (let [;; compute lattice from layout; this will be changed in the future
	lattice (lattice-from-layout layout),

	;; get positions of inf-irreducibles from layout as starting point
	inf-irrs (seq (lattice-inf-irreducibles lattice)),
	node-positions (first layout),
	inf-irr-points (map node-positions inf-irrs),

	;; move top element to [0,0], needed by layout-by-placement
	[top_x, top_y] (node-positions (lattice-one lattice)),
	inf-irr-points (map (fn [[x y]]
			      [(- x top_x), (- y top_y)])
			    inf-irr-points),

	;; minimize layout energy with above placement as initial value
	energy         (energy-by-inf-irr-positions lattice inf-irrs),
	[new-points, value] (minimize energy
				      ;;(partial-derivatives energy 0.06)
				      (apply concat inf-irr-points)),

	;; make hash
	point-hash (apply hash-map (interleave inf-irrs
					       (partition 2 new-points)))]

    ;; (pprint (apply hash-map (interleave inf-irrs inf-irr-points)))
    ;; (pprint point-hash)
    ;; (pprint value)

    ;; compute layout given by the result
    (layout-by-placement lattice point-hash)))

;;;

(require 'conexp)
(defn simple-layered-force-layout [lat]
  (force-layout (conexp/simple-layered-layout lat)))

(comment "For Testing"

(def lat (conexp/concept-lattice (conexp/rand-context #{1 2 3 4 5} 0.4)))
(conexp/draw-lattice lat simple-layered-force-layout)

)

;;;

nil
