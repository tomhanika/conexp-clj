(ns conexp.fca.graph
  (:require [loom.graph :as lg]
            [conexp.fca.lattices :as lat]
            [conexp.util.graph :refer :all]
            [conexp.base :exclude [transitive-closure] :refer :all]))


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

(defn implication-classes
  "Given a transitive digraph g, computes the implication classes.

  See Golumbic 1976,
  \"The Complexity of Comparability Graph Recognition and Coloring\".

  The implication classes are returned as a graph on the edges of g,
  where two edges are connected iff they are in the same implication class.

  These are classes of edges for which the directions in a transitive
  orientation depend on each other.
  Example for the following graph:
  b   c
  \\ / \\
    a - d
  The implication classes are {b--a, c--a, d--a} and {c--d},
  because e.g. the direction b->a would imply c->a and d->a
  (Otherwise, if e.g. b->a and a->c were applied, due to transitivity, b->c
  would also be comparable, but this edge is not in the original graph)."
  [g]
  (transitive-closure
    (make-graph-from-condition
      (lg/edges g)
      (fn [e1 e2]
        (let [a (lg/src e1)
              b (lg/dest e1)
              a' (lg/src e2)
              b' (lg/dest e2)]
          (or (and (= a a') (not (lg/has-edge? g b b')))
              (and (= b b') (not (lg/has-edge? g a a')))))))))

(defn- valid-collection-of-implication-classes?
  "Tests whether for a given graph g, a given set of implication classes allows
  the construction of a transitive orientation."
  [g impl-classes]
  (if (empty? (lg/nodes impl-classes))
    true
    (let [an-edge-of-g (first (lg/nodes impl-classes))
          the-reversed-edge (first 
                              (filter 
                                #(and (= (lg/dest %) (lg/src an-edge-of-g))
                                      (= (lg/src %) (lg/dest an-edge-of-g))) 
                                (lg/edges g)))]
      (if (lg/has-edge? impl-classes an-edge-of-g the-reversed-edge)
        false                                               ; is only comparability-graph if classes of an-edge-of-g and the-reversed-edge are disjoint
        (let [A1 (lg/successors impl-classes an-edge-of-g)  ; includes an-edge-of-g
              A1-inv (lg/successors impl-classes the-reversed-edge)
              new-impl-classes (lg/remove-nodes* (lg/remove-nodes* impl-classes A1) A1-inv)]
          (assert (not= new-impl-classes impl-classes))     ; at least an-edge-of-g and the-reversed-edge should have been removed
          (recur g new-impl-classes))))))

(defn comparability-graph?
  "Tests whether a given graph g is a comparability graph.

  Returns true iff there exists a digraph d s.t. g is the comparability graph
  of d."
  [g]
  (valid-collection-of-implication-classes?
    g (implication-classes g)))


(defn- decompose
  "Recursively decomposes graphs as described in Golumbic 1976,
  \"The Complexity of Comparability Graph Recognition and Coloring\"."
  ([g decomp]
   (decompose g decomp #(first (lg/edges %)) (implication-classes g)))
  ([g decomp edge-selection impl]
   (if (empty? (lg/edges g))
     decomp
     (let [e (edge-selection g)
           B (lg/successors impl e)
           B-inv (map reverse B)
           B-overline (union B B-inv)
           g-next (lg/remove-edges* g B-overline)]
       (recur g-next (conj decomp [B B-overline]) edge-selection impl)))))

(defn- graph-decomposition
  "Decomposes graphs as described in Golumbic 1976,
  \"The Complexity of Comparability Graph Recognition and Coloring\"."
  ([g]
   (decompose g []))
  ([g scheme]
   (decompose g []
              (fn [gr]
                (first (concat (filter #(lg/has-edge? gr (lg/src %) (lg/dest %))
                                       scheme)
                               (lg/edges gr)))))))

(defn transitive-orientation
  "Generates a transitive orientation for an undirected graph.

  `g` must be an undirected graph, the method returns a directed transitive
  copy of `g`.

  Optionally, a decomposition scheme may be given. This is a vector of edges
  describing the order in which to take the edges.

  See Golumbic 1976,
  \"The Complexity of Comparability Graph Recognition and Coloring\"."
  ([g scheme]
   (lg/add-edges*
     (lg/digraph)
     (mapcat first (graph-decomposition g scheme))))
  ([g]
   (lg/add-edges*
     (lg/digraph)
     (mapcat first (graph-decomposition g)))))

;;; consistency graph

(defn consistency-digraph-nc
  "Same as consistency-digraph, but does not check if the graph is transitive.
  Use with care!

  Given, a transitive graph g, computes the consistency-digraph.

  The nodes of the consistency-digraph are oriented incomparable pairs of nodes
  from g. From each node n=[a b] (a,b are nodes in g), edges go to all nodes
  corresponding to edges in g that would be comparable if [a b] would be
  introduced in g.

  See Definition 2.2 in https://doi.org/10.1006/jagm.1998.0974"
  [some-g]
  (let [g         (lg/build-graph (lg/digraph) some-g)
        incompat-nodes
        (set (map (fn [e] [(lg/src e) (lg/dest e)])
                  (lg/edges (co-comparability (nodes g) #(lg/has-edge? g %1 %2)))))]
    (make-directed-graph incompat-nodes
                         (fn [x0y0] (let [x0 (x0y0 0)
                                          y0 (x0y0 1)
                                          x (distinct (cons x0 (lg/predecessors* g x0)))
                                          y (distinct (cons y0 (lg/successors* g y0)))]
                                      (for [xi x
                                            yj y :when (contains? incompat-nodes [xi yj])]
                                        [xi yj]))))))

(defn consistency-digraph
  "Given, a transitive graph g, computes the consistency-digraph.

  The nodes of the consistency-digraph are oriented incomparable pairs of nodes
  from g. From each node n=[a b] (a,b are nodes in g), edges go to all nodes
  corresponding to edges in g that would be comparable if [a b] would be
  introduced in g.

  See Definition 2.2 in https://doi.org/10.1006/jagm.1998.0974"
  [g]
  (assert (= g (transitive-closure g)) "graph must be transitive!")
  (consistency-digraph-nc g))


;;; Incompatibility Graph

(defn incompatibility-graph
  "See Definition 3.2 in https://doi.org/10.1006/jagm.1998.0974"
  ([P <=]
   (incompatibility-graph (make-digraph-from-condition P <=)))
  ([pg]
   (let [g (consistency-digraph pg)
         V* (filter #(subset? (set (lg/predecessors* g %)) #{%}) (nodes g))] ; edge-nodes with no predecessor
     (make-graph-from-condition
       V*
       (fn [a b] (some #(some #{(reverse %)} (lg/successors* g b))
                       (lg/successors* g a)))))))


;;;

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

