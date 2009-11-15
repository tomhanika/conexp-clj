(ns conexp.layout.util
  (:use conexp.graphics.base
	conexp.fca.lattices
	clojure.contrib.graph))

(defn lattice->graph [lattice]
  (remove-loops
   (struct-map directed-graph
     :nodes (base-set lattice)
     :neighbors (memoize 
		 (fn [x]
		   (let [order (order lattice)]
		     (filter #(order [x %]) (base-set lattice))))))))

(defn layers [lattice]
  (dependency-list (lattice->graph lattice)))

(defn scale-points-to-rectangle
  "Scales the collection of points such that they fit int the
  rectangle given by [x1 y1] and [x2 y2]."
  [[x1 y1] [x2 y2] points]
  'to-be-done)  

nil
