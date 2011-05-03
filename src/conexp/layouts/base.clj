;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.base
  (:use conexp.base
        conexp.fca.lattices
        clojure.pprint))

(ns-doc "Basic definition of layout datatype")

;;;

(deftype Layout [lattice                ;the underlying lattice
                 positions              ;fn mapping nodes to $\RR^2$
                 annotation-label       ;fn mapping nodes and #{'lower,'upper}
                                        ;to their labels
                 annotation-position    ;fn mapping nodes and #{'lower,'upper}
                                        ;to the position of the corresponding
                                        ;label
                 information]           ;ref for technicals

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HERE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  Object
  (equals [this other]
    (generic-equals [this other] Layout [positions connections]))
  (hashCode [this]
    (hash-combine-hash Layout positions connections)))

(defn layout?
  "Returns true iff thing is a layout."
  [thing]
  (instance? Layout thing))

(defn make-layout-nc
  "Creates layout datatype from given positions hash-map, mapping node
  names to coordinate pairs, and connections, a set of pairs of node
  names denoting edges in the layout. Does not do any error checking."
  ([lattice positions connections]
     (Layout. positions (set connections) (ref {:lattice lattice})))
  ([positions connections]
     (Layout. positions (set connections) (ref {}))))

(defn- verify-positions-connections
  [positions connections]
  (when-not (map? positions)
    (illegal-argument "Positions must be a map."))
  (when-not (every? #(and (vector? %)
                          (= 2 (count %)))
                    (vals positions))
    (illegal-argument "Points must be positioned with pairs."))
  (when-not (and (coll? connections)
                 (every? #(and (vector? %)
                          (= 2 (count %)))
                    connections))
    (illegal-argument "Connections must be given as a collection of pairs."))
  (when-not (subset? (set-of x | pair connections, x pair)
                     (set (keys positions)))
    (illegal-argument "Connections must be given between positioned points."))
  ;; checking for cycles, copied from «upper-neighbours»
  (let [uppers  (loop [uppers {},
                       connections connections]
                  (if (empty? connections)
                    (map-by-fn (fn [x] (set (uppers x)))
                               (keys uppers))
                    (let [[a b] (first connections)]
                      (recur (update-in uppers [a] conj b)
                             (rest connections))))),
        cycles? (fn cycles? [node]
                  (let [equals-some-seen (fn equals-some-seen [current seen]
                                            (let [next (uppers current)]
                                              (cond
                                               (empty? next) false,
                                               (not-empty (intersection seen next)) true,
                                               :else
                                               (let [seen (into seen next)]
                                                 (some #(equals-some-seen % seen) next)))))]
                    (equals-some-seen node #{})))]
    (when (exists [x (keys positions)] (cycles? x))
      (illegal-argument "Given set of edges is cyclic.")))
  nil)

(defn- verify-lattice-positions-connections
  [lattice positions connections]
  (verify-positions-connections
   positions connections)
  (when-not (= (base-set lattice)
               (set (keys positions)))
    (illegal-argument "Positioned points must be the elements of the given lattice."))
  (when-not (forall [x (base-set lattice),
                     y (base-set lattice)]
              (<=> (contains? connections [x y])
                   (directly-neighboured? lattice x y)))
    (illegal-argument "The given connections must represent the edges of the given lattice."))
  nil)

(defn make-layout
  "Creates layout datatype from given positions hash-map, mapping node
  names to coordinate pairs, and connections, a set of pairs of node
  names denoting edges in the layout."
  ([lattice positions connections]
     (verify-lattice-positions-connections
      lattice positions connections)
     (make-layout-nc lattice positions connections))
  ([positions connections]
     (verify-positions-connections
      positions connections)
     (make-layout-nc positions connections)))

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

(defmethod print-method Layout
  [layout, ^java.io.Writer out]
  (let [^String str (with-out-str
                      (println "Layout")
                      (println "Positions")
                      (pprint (positions layout))
                      (println "Connections")
                      (pprint (connections layout)))]
    (.write out str)))

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
     (let [result# (get @(information ~layout) ~(keyword name))]
       (if (not (nil? result#))
         result#
         (let [new-result# (do ~@body)]
           (dosync
            (alter (information ~layout) assoc ~(keyword name) new-result#)
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
  (loop [inf-uppers (transient {}),
         all-uppers (seq (upper-neighbours layout))]
    (if (empty? all-uppers)
      (persistent! inf-uppers)
      (let [[x upper-x] (first all-uppers)]
        (recur (if (= 1 (count upper-x))
                 (assoc! inf-uppers x (first upper-x))
                 inf-uppers)
               (rest all-uppers))))))

(def-layout-fn inf-irreducibles
  "Returns the set of infimum irreducible elements of layout."
  [layout]
  (set-of v [[v uppers] (upper-neighbours layout),
             :when (singleton? uppers)]))

(def-layout-fn sup-irreducibles
  "Returns the set of supremum irreducible elements of layout."
  [layout]
  (set-of v [[v lowers] (lower-neighbours layout),
             :when (singleton? lowers)]))

(def-layout-fn full-order-relation
  "Returns underlying order relation of layout. This operation may be
  very costly."
  [layout]
  (reflexive-transitive-closure (nodes layout) (connections layout)))

(def-layout-fn lattice
  "Returns lattice represented by layout."
  [layout]
  (let [uppers (upper-neighbours layout),
        order  (memo-fn order [x y]
                 (or (= x y)
                     (exists [z (uppers x)]
                       (order z y))))]
    (make-lattice-nc (nodes layout) order)))

(def-layout-fn context
  "Returns a context whose lattice is represented by this layout."
  [layout]
  (standard-context (lattice layout)))

(defn concept-lattice-layout?
  "Tests whether layout comes from a concept lattice.

  Note: This implementation is not correct, as it only tests whether
  the layout repects the subset relation in the first component and
  the superset relation in the second component of every node."
  [layout]
  (let [lattice (lattice layout)]
    (and (forall [x (base-set lattice)]
           (and (vector? x)
                (= 2 (count x))
                (set? (first x))
                (set? (second x))))
         (forall [[x y] (connections layout)]
           (and (subset? (first x) (first y))
                (superset? (second x) (second y)))))))

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
    (map-by-fn (fn [x] [x ""]) (nodes layout))
    (let [uppers (upper-neighbours layout),
          lowers (lower-neighbours layout)]
      (map-by-fn (fn [node]
                   [(set-to-label
                     (apply difference (second node) (map second (uppers node))))
                    (set-to-label
                     (apply difference (first node) (map first (lowers node))))])
                 (nodes layout)))))

;;;

nil
