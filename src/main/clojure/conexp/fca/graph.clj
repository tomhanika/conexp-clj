(ns conexp.fca.graph
  (:require [ubergraph.core :as uber]
            [loom.graph :as lg]
            [conexp.fca.lattices :as lat])
  (:use conexp.base)
  (:use loom.io))


;;; graph <-> lattice

(defn lattice->graph
  "Converts a lattice to a directed graph.
  For concepts u,v, there will be an edge u->v iff v <= u.
  (This implies that the only loops will be u->u for all u.)"
  [lat]
  (let
    [<= (lat/order lat)
     base-set (lat/base-set lat)]
    (uber/add-directed-edges*
      (uber/digraph)
      (mapcat
        (fn [x] (map
                  (fn [y] [x y])
                  (filter
                    (fn [y] (<= x y))
                    base-set)))
        base-set))))

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
  For elements u,v, there will be an edge u<->v iff (u,v) or (v,u) in R.
  Note: If the ordering is reflexive, u<->u for all u in the set."
  [base-set relation]
  (uber/add-undirected-edges*
    (uber/graph)
    (mapcat
      (fn [x] (map
                (fn [y] [x y])
                (filter #(relation x %) base-set)))
      base-set)))

(defn co-comparability
  "Given a set and a relation, generates a graph of incomparable elements.
  For elements u,v, there will be an edge u<->v iff neither (u,v) nor (v,u) in R.
  Note: If the ordering not reflexive, u<->u for all u in the set."
  [base-set relation]
  (uber/add-undirected-edges*
    (uber/graph)
    (mapcat
      (fn [x] (map
                (fn [y] [x y])
                (filter
                  (fn [y] (and (not (relation x y))
                               (not (relation y x))))
                  base-set)))
      base-set)))

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

