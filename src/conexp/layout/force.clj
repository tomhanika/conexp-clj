(ns conexp.layout.force
  (:use conexp.layout.util
	conexp.math.optimize))

;;; Force layout as described by C. Zschalig

;; Repulsive Energy

(defn node-line-distance
  "Returns the distance from node to the line between x and y."
  [node [x y]]
  )

(defn repulsive-energy [[node-positions node-connections]]
  0)

;; Attractive Energy

(defn attractive-energy [[node-positions node-connections]]
  0)

;; Gravitative Energy

(defn gravitative-energy [[node-positions node-connections]]
  0)

;; Overall Energy

(defn layout-energy
  "Returns the overall energy of the given layout. The coefficients r,
  a and g give the amount of repulsive, attractive and gravitative
  energy, respectively."
  [r a g layout]
  (+ (* r (repulsive-energy layout))
     (* a (attractive-energy layout))
     (* g (gravitative-energy layout))))

(defn energy-by-inf-irr-positions
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
  ;; layout is initial parameter for optimization
  [lattice layout]
  ;; get placement of inf-irreducibles in layout
  ;; minimize layout energy with above placement as initial value
  ;; compute layout given by the result
  )

;;;

nil
