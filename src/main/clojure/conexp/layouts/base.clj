;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.base
  "Basic definition of layout datatype"
  (:use conexp.base
        conexp.math.algebra
        conexp.fca.lattices
        conexp.fca.posets
        conexp.fca.closure-systems
        clojure.pprint))

;;;

(deftype Layout [poset                  ;the underlying ordered set
                 positions              ;map mapping nodes to $\RR^2$
                 connections            ;connections as set of pairs
                 upper-labels           ;map mapping nodes to vectors of labels and coordinates/nil
                 lower-labels           ;same
                 valuations             ;valuations of the poset elements
                 information]           ;ref for technicals
  Object
  (equals [this other]
    (generic-equals [this other] Layout [poset positions connections upper-labels lower-labels]))
  (hashCode [this]
    (hash-combine-hash Layout poset positions connections upper-labels lower-labels)))

(defn layout?
  "Returns true iff thing is a layout."
  [thing]
  (instance? Layout thing))

;;; helper functions

(defn- poset-from-layout-data
  "Returns poset or lattice represented by layout."
  [positions connections]
  ;; error checking
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
  (let [uppers  (loop [uppers      {},
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
      (illegal-argument "Given set of edges is cyclic."))
    ;; actual construction
    (let [poset-base-set (set (keys positions)),
          poset-order (memo-fn order [x y]
                               (or (= x y)
                                   (exists [z (uppers x)]
                                           (order z y))))]
      (try (make-lattice poset-base-set
                         poset-order)
           (catch IllegalArgumentException _ ;; no lattice order -> create a poset instead of a lattice
             (make-poset-nc poset-base-set
                            poset-order))))))

;;; plain construction

(defn make-layout-nc
  "Creates layout datatype from given information. The arguments thereby have the following meaning:

   - poset is the underlying poset of the to be constructed layout
   - positions is a hash-map, mapping node names to coordinate pairs,
   - connections is a set of pairs of node names denoting edges in the layout,
   - upper-labels is a map mapping nodes to pairs of upper labels and coordinates or nil,
   - lower-labels is like upper-labels for lower-labels.
   - valuations is a map mapping nodes to their value for some valuation

  This functions does only a limited amount of error checking."
  ([poset positions connections upper-labels lower-labels valuations]
     (Layout. poset positions connections upper-labels lower-labels valuations (ref {})))
  ([poset positions connections upper-labels lower-labels]
     (Layout. poset positions connections upper-labels lower-labels (ref {}) (ref {})))
  ([poset positions connections]
     (make-layout-nc poset positions connections nil nil))
  ([positions connections upper-label lower-label]
     (make-layout-nc (poset-from-layout-data positions connections)
                     positions
                     connections
                     upper-label lower-label))
  ([positions connections]
     (make-layout-nc (poset-from-layout-data positions connections)
                     positions
                     connections)))

(defn update-positions
  "Updates position map in layout to be new-positions. Keys of both
  hash-maps must be the same."
  [^Layout layout, new-positions]
  (assert (= (set (keys new-positions))
             (set (keys (.positions layout))))
          "Nodes must stay the same when updating positions of an already existing layout.")
  (Layout. (.poset layout)
           new-positions
           (.connections layout)
           (.upper-labels layout)
           (.lower-labels layout)
           (.valuations layout)
           (.information layout)))

(defn update-valuations
  "Updates valuation map in layout."
  [^Layout layout, val-fn]
  (let [theposet (.poset layout)
        elements (base-set theposet)]
    (Layout. (.poset layout)
           (.positions layout)
           (.connections layout)
           (.upper-labels layout)
           (.lower-labels layout)
           (reduce (fn [e x] (assoc e x (val-fn x))) {} elements)
           (.information layout))))

(defn update-valuations-error
  "Write \"err\" to each valuation in layout."
  [^Layout layout]
  (let [error-fn (fn [_] "err")]
    (update-valuations layout error-fn)))

;;; argument verification

(defn- verify-poset-positions-connections
  [poset positions connections]
  (when-not (= (base-set poset)
               (set (keys positions)))
    (illegal-argument "Positioned points must be the elements of the given poset."))
  (when-not (forall [x (base-set poset),
                     y (base-set poset)]
              (<=> (contains? connections [x y])
                   (directly-neighboured? poset x y)))
    (illegal-argument "The given connections must represent the edges of the given poset.")))

(defn- check-labels
  [positions labels direction]
  (when-not (map? labels)
    (illegal-argument "Labels must be given as map."))
  (when-not (= (set (keys labels))
               (set (keys positions)))
    (illegal-argument "Nodes in layout and given labeled nodes are different."))
  (when-not (forall [x (vals labels)]
              (and (vector? x)
                   (= 2 (count x))
                   (or (nil? (second x))
                       (and (vector? (second x))
                            (number? (first (second x)))
                            (number? (second (second x)))))))
    (illegal-argument "Nodes must be labeled with pairs, of which the second entry must either "
                      "be nil or a pair of numbers."))
  (when-not (forall [[x [_ pos-x]] positions]
              (if-let [[_ pos-lab-x] (second (labels x))]
                (direction pos-lab-x pos-x)
                true))
    (illegal-argument "Labels must be above the labeled node (for upper-labels) "
                      "or below the labeled node (for lower-labels).")))

(defn- verify-labels
  [positions upper-labels lower-labels]
  (check-labels positions upper-labels >=)
  (check-labels positions lower-labels <=))

;;; checked construction

(defn make-layout
  "Creates layout datatype from given positions hash-map, mapping node
  names to coordinate pairs, and connections, a set of pairs of node
  names denoting edges in the layout."
  ([poset positions connections upper-label lower-label valuations]
     (let [connections (set connections),
           upper-label (if (fn? upper-label)
                         (map-by-fn upper-label (base-set poset))
                         upper-label),
           lower-label (if (fn? lower-label)
                         (map-by-fn lower-label (base-set poset))
                         lower-label)
           valuations  (if (fn? valuations)
                         (map-by-fn valuations  (base-set poset))
                         valuations)]
       (verify-poset-positions-connections poset positions connections)
       (verify-labels positions upper-label lower-label)
       (make-layout-nc poset positions connections upper-label lower-label valuations)))
  ([poset positions connections upper-label lower-label]
     (let [connections (set connections),
           upper-label (if (fn? upper-label)
                         (map-by-fn upper-label (base-set poset))
                         upper-label),
           lower-label (if (fn? lower-label)
                         (map-by-fn lower-label (base-set poset))
                         lower-label)]
       (verify-poset-positions-connections poset positions connections)
       (verify-labels positions upper-label lower-label)
       (make-layout-nc poset positions connections upper-label lower-label)))
  ([positions connections upper-label lower-label]
     (make-layout (poset-from-layout-data positions connections)
                  positions
                  connections
                  upper-label
                  lower-label))
  ([poset positions connections]
     (let [connections (set connections)]
       (verify-poset-positions-connections poset positions connections)
       (make-layout-nc poset positions connections)))
  ([positions connections]
     (make-layout (poset-from-layout-data positions connections)
                  positions
                  connections)))

;;; basic functions

(defn poset
  "Returns the poset underlying the given layout."
  [^Layout layout]
  (.poset layout))

(defn positions
  "Return positions map of layout."
  [^Layout layout]
  (.positions layout))

(defn connections
  "Returns set of connections of layout."
  [^Layout layout]
  (.connections layout))

(defn valuations
  "Returns stored additional valuations of layout."
  [^Layout layout]
  (.valuations layout))

(defn- information
  "Returns stored additional information of layout."
  [^Layout layout]
  (.information layout))

(defn upper-labels
  "Returns the upper labels of a given layout."
  [^Layout layout]
  (.upper-labels layout))

(defn lower-labels
  "Returns the lower labels of a given layout."
  [^Layout layout]
  (.lower-labels layout))

;;;

(defmethod print-method Layout
  [layout, ^java.io.Writer out]
  (let [^String str (with-out-str
                      (println "Layout")
                      (println "Positions")
                      (pprint (positions layout))
                      (println "Connections")
                      (pprint (connections layout))
                      (println "Valuations")
                      (pprint (valuations layout)))]
    (.write out str)))

(defn nodes
  "Returns all nodes of a given layout."
  [layout]
  (set (keys (positions layout))))

(defn upper-label
  "Returns the upper label of x in layout, if it exists. Otherwise returns nil."
  [layout x]
  (when-let [labels (upper-labels layout)]
    (first (labels x))))

(defn lower-label
  "Returns the lower label of x in layout, if it exists. Otherwise returns nil."
  [layout x]
  (when-let [labels (lower-labels layout)]
    (first (labels x))))

(defn upper-label-position
  "Returns the position of the upper label of x in layout, if existent. Otherwise returns nil."
  [layout x]
  (when-let [labels (upper-labels layout)]
    (second (labels x))))

(defn lower-label-position
  "Returns the position of the lower label of x in layout, if existent. Otherwise returns nil."
  [layout x]
  (when-let [labels (lower-labels layout)]
    (second (labels x))))

(defn valuation
  "Returns the valuation of x in layout, if it exists. Otherwise returns nil."
  [layout x]
  (when-let [vals (valuations layout)]
    (first (vals x))))

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
  (assert (has-lattice-order? (poset layout)) 
          "The given layout does not contain a lattice.")
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
  (assert (has-lattice-order? (poset layout)) 
          "The given layout does not contain a lattice.")
  (set-of v [[v uppers] (upper-neighbours layout),
             :when (singleton? uppers)]))

(def-layout-fn sup-irreducibles
  "Returns the set of supremum irreducible elements of layout."
  [layout]
  (assert (has-lattice-order? (poset layout))
          "The given layout does not contain a lattice.")
  (set-of v [[v lowers] (lower-neighbours layout),
             :when (singleton? lowers)]))

(def-layout-fn full-order-relation
  "Returns underlying order relation of layout. This operation may be
  very costly."
  [layout]
  (reflexive-transitive-closure (nodes layout) (connections layout)))

(def-layout-fn context
  "Returns a context whose lattice is represented by this layout."
  [layout]
  (poset-context (poset layout)))

(def-layout-fn concept-lattice-layout?
  "Tests whether layout comes from a concept lattice.

  Note: This implementation is not correct, as it only tests whether
  the layout repects the subset relation in the first component and
  the superset relation in the second component of every node."
  [layout]
  (let [poset (poset layout)]
    (and (forall [x (base-set poset)]
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

(defn concept-lattice-annotation
  "Returns the shortend annotation for a concept lattice."
  [layout]
  (assert (concept-lattice-layout? layout)
          "Layout must be that of a concept lattice.")
  (let [uppers (upper-neighbours layout),
        lowers (lower-neighbours layout)]
    (map-by-fn (fn [node]
                 [(apply difference (second node) (map second (uppers node)))
                  (apply difference (first node) (map first (lowers node)))])
               (nodes layout))))

(def-layout-fn annotation
  "Returns the annotation of this layout as hash-map of nodes to
  pairs, where the first entry is the upper label and second one is
  the lower label."
  [layout]
  (cond
   (and (upper-labels layout)
        (lower-labels layout))
   (map-by-fn (fn [x]
                [(first ((upper-labels layout) x)),
                 (first ((lower-labels layout) x))])
              (nodes layout)),
   ;;
   (concept-lattice-layout? layout)
   (let [ann (concept-lattice-annotation layout)]
     (map-by-fn (fn [node]
                  [(set-to-label (first (ann node))),
                   (set-to-label (second (ann node)))])
                (keys ann))),
   ;;
   :else
   (map-by-fn (fn [x] [x ""]) (nodes layout))))

;;;

nil
