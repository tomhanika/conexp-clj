;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layout.base
  (:use conexp.base
	[conexp.fca.lattices :only (make-lattice, standard-context)]
	clojure.contrib.pprint))

(ns-doc "Basic definition of layout datatype")

;;;

(deftype Layout [positions connections information]
  Object
  (equals [this other]
    (generic-equals [this other] Layout [positions connections information]))
  (hashCode [this]
    (hash-combine-hash Layout positions connections)))

(defn make-layout
  "Creates layout datatype from given positions hash-map, mapping node
  names to coordinate pairs, and connections, a set of pairs of node
  names denoting edges in the layout."
  [positions connections]
  (Layout. positions (set connections) (ref {})))

(defn positions
  "Return positions map of layout."
  [^Layout layout]
  (.positions layout))

(defn connections
  "Returns set of connections of layout."
  [^Layout layout]
  (.connections layout))

(defn- information
  "Returns stored additional information of layout."
  [^Layout layout]
  (.information layout))

(defn update-positions
  "Updates position map in layout to be new-positions. Keys of both
  hash-maps must be the same, otherwise everything will be a mess."
  [layout new-positions]
  (Layout. new-positions (connections layout) (information layout)))

(defmethod print-method Layout [layout, ^java.io.Writer out]
  (.write out
	  ^String (with-out-str
                    (println "Layout")
                    (println "Positions")
                    (pprint (positions layout))
                    (println "Connections")
                    (pprint (connections layout)))))

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
	    (alter (information ~layout) assoc (keyword '~name) new-result#)
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

(def-layout-fn lower-neighbours
  "Returns hash-map mapping node names to sets of their upper neighbours."
  [layout]
  (let [lowers (loop [lowers {},
		      connections (seq (connections layout))]
		 (if (empty? connections)
		   lowers
		   (let [[a b] (first connections)]
		     (recur (update-in lowers [b] conj a)
			    (rest connections)))))]
    lowers))

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
  "Returns underlying order relation of layout. This operation may be
  very costly."
  [layout]
  (reflexive-transitive-closure (nodes layout) (connections layout)))

(def-layout-fn lattice
  "Returns lattice represented by layout."
  [layout]
  (make-lattice (nodes layout) (order layout)))

(def-layout-fn context
  "Returns a context whose lattice is represented by this layout."
  [layout]
  (standard-context (lattice layout)))

(defn- concept-lattice-layout?
  "Tests whether layout comes from a concept lattice."
  [layout]
  (and (forall [node (nodes layout)]
	 (and (vector? node)
	      (= 2 (count node))
	      (set? (first node))
	      (set? (second node))))
       (forall [node-1 (nodes layout),
		node-2 (nodes layout)]
	 (=> ((order layout) [node-1 node-2])
	     (and (subset? (first node-1) (first node-2))
		  (superset? (second node-1) (second node-2)))))))

(defn- set-to-label
  "Converts set of elements to a label."
  [set]
  (apply str (interpose ", " set)))

(def-layout-fn annotation
  "Returns the annotation of this layout as hash-map of nodes to
  pairs, where the first entry is the upper label and second one is
  the lower label."
  [layout]
  (if-not (concept-lattice-layout? layout)
    (hashmap-by-function (fn [x] [x ""]) (nodes layout))
    (let [uppers (upper-neighbours layout),
	  lowers (lower-neighbours layout)]
      (into {} (for [node (nodes layout)]
                 [(set-to-label
                   (apply difference (second node) (map second (uppers node))))
                  (set-to-label
                   (apply difference (first node) (map first (lowers node))))])))))

;;;

nil
