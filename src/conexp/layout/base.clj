(ns conexp.layout.base
  (:use conexp.base
	conexp.layout.util
	conexp.fca.lattices
	[clojure.contrib.graph :only (dependency-list)]
	[clojure.contrib.ns-utils :only (immigrate)]))

(update-ns-meta! conexp.layout.base
  :doc "Basic namespace for lattice layouts.")


;;; Simple Layered Layout

(defn- layers
  "Returns the layers of the given lattice, that is sequence of points
  with equal heights."
  [lattice]
  (dependency-list (lattice->graph lattice)))

(defn- layer-coordinates
  "Assigns coordinates to a given layer such that it is centerer
  around 0 at height given by number."
  [number layer]
  (let [start (double (- (/ (- (count layer) 1) 2)))]
    (interleave layer
		(map #(vector % number)
		     (iterate inc start)))))

(defn simple-layered-layout
  "Simple layered layout for lattice visualization."
  [lattice]
  (let [positions (apply hash-map
			 (apply concat
				(map layer-coordinates
				     (iterate inc 0)
				     (layers lattice))))]
    (scale-layout [0.0 0.0] [200.0 200.0]
		  [positions,
		   (for [x (base-set lattice),
			 y (base-set lattice),
			 :when (and (not= x y)
				    (directly-neighboured? lattice x y))]
		     [x y])])))


;;;

nil
