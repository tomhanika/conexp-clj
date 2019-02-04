(ns conexp.fca.graph
  (:require [ubergraph.core :as uber]
            [loom.graph :as lg]
            [conexp.fca.lattices :as lat]
            [conexp.util.graph :exclude [transitive-closure] :refer :all])
  (:use conexp.base)
  (:use loom.io))


;;; graph <-> lattice

(defn lattice->graph
  "Converts a lattice to a directed graph.
  For concepts u,v, there will be an edge u->v iff v <= u.
  (This implies that the only loops will be u->u for all u.)"
  [lat]
  (make-digraph-from-condition (lat/base-set lat) (lat/order lat)))

(defn graph->lattice-nc
  "Converts a directed graph to a lattice.
  Note: This method does no checks, so the lattice may be invalid is the graph
  does not represent one. Use with care."
  [g]
  (lat/make-lattice-nc (lg/nodes g) (fn [u v] (or (= u v) (lg/has-edge? g u v)))))

(defn graph->lattice
  "Converts a directed graph to a lattice.
  Note: This function will test the resulting lattice for being one,
  which may take some time. If you don't want this, use
  make-lattice-nc."
  [g]
  (lat/make-lattice (lg/nodes g) (fn [u v] (or (= u v) (lg/has-edge? g u v)))))


;;; (co)comparability

(defn comparability
  "Given a set and a relation, generates a graph of comparable elements.
  For elements u,v, there will be an edge u<->v iff (u,v) or (v,u) in relation.
  Note: If the relation is reflexive, u<->u for all u in the set."
  ([lattice] (comparability
               (conexp.fca.lattices/base-set lattice)
               (conexp.fca.lattices/order lattice)))
  ([base-set relation]
   (make-graph-from-condition base-set relation)))

(defn co-comparability
  "Given a set and a relation, generates a graph of incomparable elements.
  For elements u,v, there will be an edge u<->v iff neither (u,v) nor (v,u) in
  relation.
  Note: If the relation not reflexive, u<->u for all u in the set."
  ([lattice] (co-comparability
               (conexp.fca.lattices/base-set lattice)
               (conexp.fca.lattices/order lattice)))
  ([base-set relation]
   (make-graph-from-condition base-set #(and (not (relation %1 %2))
                                             (not (relation %2 %1))))))

(defn strict
  "Make a strict ordering < of an ordering <=."
  [<=]
  (fn
    <
    ([x y] (and (<= x y) (not= x y)))
    ([vec] (< (first vec) (second vec)))))

(defn non-strict
  "Make a non-strict ordering <= of a strict ordering <."
  [<]
  (fn
    <=
    ([x y] (or (< x y) (= x y)))
    ([vec] (< (first vec) (second vec)))))

