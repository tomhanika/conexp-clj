(ns conexp.layout.base
  (:use conexp.base
	[conexp.fca.lattices :only (make-lattice)]))

(update-ns-meta! conexp.layout.base
  :doc "Basic definition of layout datatype")

;;;

(deftype Layout [positions connections information])

(defn make-layout
  "Creates layout datatype from given positions hash-map, mapping node
  names to coordinate pairs, and connections, a set of pairs of node
  names denoting edges in the layout."
  [positions connections]
  (Layout positions (set connections) (ref {})))

(defn positions
  "Return positions map of layout."
  [layout]
  (:positions layout))

(defn connections
  "Returns set of connections of layout."
  [layout]
  (:connections layout))

(defn- information
  "Returns stored additional information of layout."
  [layout]
  (:information layout))

(defn update-positions
  "Updates position map in layout to be new-positions. Keys of both
  hash-maps must be the same, otherwise everything will be a mess."
  [layout new-positions]
  (Layout new-positions (connections layout) (information layout)))

(defmethod print-method ::Layout [layout out]
  (.write out
	  (str "Layout on "
	       (count (positions layout)) " nodes with "
	       (count (connections layout)) " edge(s).")))

(defn nodes
  "Returns all nodes of a given layout."
  [layout]
  (set (keys (positions layout))))
  

;;; Layout Auxiliary Functions

(defmacro- def-layout-fn
  "Defines a function name on layout. If this function has been called
  on this layout before, returns the stored value. Otherwise computes
  a value and stores it."
  [name doc-string [layout & args] & body]
  `(defn ~name ~doc-string [~layout ~@args]
     (let [result# (get @(information ~layout) (keyword '~name))]
       (if (not (nil? result#))
	 result#
	 (dosync
	  (let [new-result# (do ~@body)]
	    (alter (information ~layout) assoc ~keyword new-result#)
	    new-result#))))))

(def-layout-fn upper-neighbours
  "Returns hash-map mapping node names to sets of their upper neighbours."
  [layout]
  (let [uppers (loop [uppers {},
		      connections (seq (connections layout))]
		 (if (empty? connections)
		   uppers
		   (let [[a b] (first connections)]
		     (recur (update-in uppers [a] conj b)
			    (rest connections)))))]
    uppers))

(def-layout-fn upper-neighbours-of-inf-irreducibles
  "Returns hash-map mapping the infimum irreducible elements to their
  upper neighbours."
  [layout]
  (loop [inf-uppers {}
	 all-uppers (seq (upper-neighbours layout))]
    (if (empty? all-uppers)
      inf-uppers
      (let [[x upper-x] (first all-uppers)]
	(recur (if (= 1 (count upper-x))
		 (conj inf-uppers [x (first upper-x)])
		 inf-uppers)
	       (rest all-uppers))))))

(def-layout-fn inf-irreducibles
  "Returns a sequence of infimum irreducible elements of layout."
  [layout]
  (keys (upper-neighbours-of-inf-irreducibles layout)))

(def-layout-fn order
  "Returns underlying order relation of layout."
  [layout]
  (union (transitive-closure (connections layout))
	 (set-of [x x] [x (nodes layout)])))

(defn- lattice-from-layout
  "Computes lattice from a given layout."
  [layout]
  (make-lattice (nodes layout) (order layout)))

(def-layout-fn lattice
  "Returns lattice represented by layout."
  [layout]
  (lattice-from-layout layout))


;;;

nil
