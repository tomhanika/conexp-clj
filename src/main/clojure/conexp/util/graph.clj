;; Copyright ⓒ Jeffrey Straszheim. All rights reserved. The use and
;; distribution terms for this software are covered by the Eclipse Public
;; License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;; be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other,
;; from this software.
;;
;; graph
;;
;; Basic Graph Theory Algorithms
;;
;; straszheimjeffrey (gmail)
;; Created 23 June 2009
;;
;; with modifications by D. Borchmann for conexp-clj
;; and more modifications for use with loom

(ns
  #^{:author "Jeffrey Straszheim",
     :doc    "Basic graph theory algorithms"}
  conexp.util.graph
  (:require [loom.graph :as lg]
            [loom.attr :as la])
  (:use [clojure.set :only (union)])
  (:import [org.dimdraw Transitive]))


(defn make-directed-graph
  "Constructs a directed graph from a set of nodes and a function that maps a
   node to its neighbors."
  [nodes neighbor-fn]
  (lg/add-edges*
    (la/add-attrs-to-all (lg/add-nodes* (lg/digraph) nodes))
    (mapcat (fn [x] (map (fn [y] [x y]) (neighbor-fn x))) nodes)))

(defn make-graph-from-condition
  "Constructs an undirected graph from a set of nodes and a condition that tests
   if two nodes shall get an edge.
   Edges are undirected, so there will be an edge u<->v iff the condition holds
   for either (u,v) or (v,u) or both."
  [nodes condition]
  (lg/add-edges*
    (apply lg/add-nodes (lg/graph) nodes)
    (mapcat
      (fn [x] (map
                (fn [y] [x y])
                (filter #(condition x %) nodes)))
      nodes)))

(defn make-digraph-from-condition
  "Constructs a directed graph from a set of nodes and a condition that tests
   if two nodes shall get an edge."
  [nodes condition]
  (lg/add-edges*
    (apply lg/add-nodes (lg/digraph) nodes)
    (mapcat
      (fn [x] (map
                (fn [y] [x y])
                (filter #(condition x %) nodes)))
      nodes)))

(defn nodes
  "all nodes of the graph"
  [g]
  (lg/nodes g))

(defn neighbor-fn
  "A function that maps from nodes to their neighbors"
  [g]
  (fn [n] (lg/successors* g n)))

(defn get-neighbors
  "Get the neighbors of a node."
  [g n]
  (lg/successors* g n))

;; Graph Modification

(defn reverse-graph
  "Given a directed graph, return another directed graph with the
order of the edges reversed."
  [g]
  (let [op (fn [rna idx]
             (let [ns (get-neighbors g idx)
                   am (fn [m val]
                        (assoc m val (conj (get m val #{}) idx)))]
               (reduce am rna ns)))
        rn (reduce op {} (nodes g))]
    (make-directed-graph (nodes g) rn)))

(defn add-loops
  "For each node n, add the edge n->n if not already present."
  [g]
  (make-directed-graph
    (nodes g)
    (into {} (map (fn [n]
                    [n (conj (set (get-neighbors g n)) n)]) (nodes g)))))

(defn remove-loops
  "For each node n, remove any edges n->n."
  [g]
  (make-directed-graph
    (nodes g)
    (into {} (map (fn [n]
                    [n (disj (set (get-neighbors g n)) n)]) (nodes g)))))


;; Graph Walk

(defn lazy-walk
  "Return a lazy sequence of the nodes of a graph starting a node n. Optionally,
provide a set of visited notes (v) and a collection of nodes to
visit (ns)."
  ([g n]
   (lazy-walk g [n] #{}))
  ([g ns v]
   (lazy-seq (let [s (seq (drop-while v ns))
                   n (first s)
                   ns (rest s)]
               (when s
                 (cons n (lazy-walk g (concat (get-neighbors g n) ns) (conj v n))))))))

(defn transitive-closure
  "Returns the transitive closure of a graph. The neighbors are lazily computed.

  This implementation uses the (Floyd-)Warshall algorithm which is based on
  dynamic programming to compute the closure.

  Note: some version of this algorithm return all edges a->a
  regardless of whether such loops exist in the original graph. This
  version does not. Loops will be included only if produced by
  cycles in the graph. If you have code that depends on such
  behavior, call (-> g transitive-closure add-loops)"
  [g]
  (if (= 0 (count (lg/nodes g)))
    g
    (let [len   (count (lg/nodes g))
          nodes (range len)
          dict  (zipmap (range) (lg/nodes g))
          graph (into-array (for [u (range (count dict))]
                              (into-array Boolean/TYPE
                                          (for [v (range (count dict))]
                                            (lg/has-edge? g (dict u) (dict v))))))
          pairs (reduce concat (for [u nodes]
                                 (for [v nodes] [u v])))
          ;; graph (reduce (fn [graph [k i]]
          ;;                 (if (nth (nth graph i) k)
          ;;                   (reduce #(if (nth (nth %1 k) %2)
          ;;                              (assoc-in %1 [i %2] true)
          ;;                              %1)
          ;;                           graph nodes)
          ;;                   graph))
          ;; graph pairs)
          graph (. Transitive hull graph)]
      (reduce (fn [g u]
                (reduce (fn [g v]
                          (if (nth (nth graph u) v)
                            (lg/add-edges g [(dict u) (dict v)])
                            g))
                        g (range len)))
              g (range len)))))

;; Strongly Connected Components

(defn- post-ordered-visit
  "Starting at node n, perform a post-ordered walk."
  [g n [visited acc :as state]]
  (if (visited n)
    state
    (let [[v2 acc2] (reduce (fn [st nd] (post-ordered-visit g nd st))
                            [(conj visited n) acc]
                            (get-neighbors g n))]
      [v2 (conj acc2 n)])))

(defn post-ordered-nodes
  "Return a sequence of indexes of a post-ordered walk of the graph."
  [g]
  (fnext (reduce #(post-ordered-visit g %2 %1)
                 [#{} []]
                 (nodes g))))

(defn scc
  "Returns, as a sequence of sets, the strongly connected components of g."
  [g]
  (let [po (reverse (post-ordered-nodes g))
        rev (reverse-graph g)
        step (fn [stack visited acc]
               (if (empty? stack)
                 acc
                 (let [[nv comp] (post-ordered-visit rev
                                                     (first stack)
                                                     [visited #{}])
                       ns (doall (remove nv stack))]        ;doall prevents StackOverflow
                   (recur ns nv (conj acc comp)))))]
    (step po #{} [])))

(defn component-graph
  "Given a graph, perhaps with cycles, return a reduced graph that is acyclic.
Each node in the new graph will be a set of nodes from the old.
These sets are the strongly connected components. Each edge will
be the union of the corresponding edges of the prior graph."
  ([g]
   (component-graph g (scc g)))
  ([g sccs]
   (let [find-node-set (fn [n]
                         (some #(if (% n) % nil) sccs))
         find-neighbors (fn [ns]
                          (let [nbs1 (map (partial get-neighbors g) ns)
                                nbs2 (map set nbs1)
                                nbs3 (apply union nbs2)]
                            (set (map find-node-set nbs3))))
         nm (into {} (map (fn [ns] [ns (find-neighbors ns)]) sccs))]
     (make-directed-graph (set sccs) nm))))

(defn recursive-component?
  "Is the component (recieved from scc) self recursive?"
  [g ns]
  (or (> (count ns) 1)
      (let [n (first ns)]
        (some #(= % n) (get-neighbors g n)))))

(defn self-recursive-sets
  "Returns, as a sequence of sets, the components of a graph that are
self-recursive."
  [g]
  (filter (partial recursive-component? g) (scc g)))

;; Dependency Lists

(defn fixed-point
  "Repeatedly apply fun to data until (equal old-data new-data)
returns true. If max iterations occur, it will throw an
exception. Set max to nil for unlimited iterations."
  [data fun max equal]
  (let [step (fn step [data idx]
               (when (and idx (= 0 idx))
                 (throw (Exception. "Fixed point overflow")))
               (let [new-data (fun data)]
                 (if (equal data new-data)
                   new-data
                   (recur new-data (and idx (dec idx))))))]
    (step data max)))

(defn- fold-into-sets
  [priorities]
  (let [max (inc (apply max 0 (vals priorities)))
        step (fn [acc [n dep]]
               (assoc acc dep (conj (acc dep) n)))]
    (reduce step
            (vec (replicate max #{}))
            priorities)))

(defn dependency-list
  "Similar to a topological sort, this returns a vector of sets. The
set of nodes at index 0 are independent. The set at index 1 depend
on index 0; those at 2 depend on 0 and 1, and so on. Those withing
a set have no mutual dependencies. Assume the input graph (which
much be acyclic) has an edge a->b when a depends on b."
  [g]
  (let [step (fn [d]
               (let [update (fn [n]
                              (inc (apply max -1 (map d (get-neighbors g n)))))]
                 (into {} (map (fn [[k v]] [k (update k)]) d))))
        counts (fixed-point (zipmap (nodes g) (repeat 0))
                            step
                            (inc (count (nodes g)))
                            =)]
    (fold-into-sets counts)))

(defn stratification-list
  "Similar to dependency-list (see doc), except two graphs are
provided. The first is as dependency-list. The second (which may
have cycles) provides a partial-dependency relation. If node a
depends on node b (meaning an edge a->b exists) in the second
graph, node a must be equal or later in the sequence."
  [g1 g2]
  (assert (= (-> g1 nodes set) (-> g2 nodes set)))
  (let [step (fn [d]
               (let [update (fn [n]
                              (max (inc (apply max -1
                                               (map d (get-neighbors g1 n))))
                                   (apply max -1 (map d (get-neighbors g2 n)))))]
                 (into {} (map (fn [[k v]] [k (update k)]) d))))
        counts (fixed-point (zipmap (nodes g1) (repeat 0))
                            step
                            (inc (count (nodes g1)))
                            =)]
    (fold-into-sets counts)))

;; Set Operations


(defn transitive-edge-union
  ([base]
   (if (satisfies? loom.graph/Digraph base)
     (transitive-closure base)
     (transitive-closure (apply lg/add-nodes (lg/digraph) base))))
  ([base edges1]
   (if (instance? clojure.lang.Sequential edges1)
     (transitive-closure
       (lg/add-edges* (transitive-edge-union base)
                        edges1))
     (transitive-closure
       (lg/add-edges* (transitive-edge-union base)
                        (mapcat
                          (fn [x] (map
                                    (fn [y] [x y])
                                    (filter #(edges1 x %) base)))
                          base)))))
  ([base edges1 & more-edges]
   (if (instance? clojure.lang.Sequential edges1)
     (transitive-closure
       (lg/add-edges* (apply transitive-edge-union base more-edges)
                             edges1))
     (transitive-closure
       (lg/add-edges* (apply transitive-edge-union base more-edges)
                        (mapcat
                          (fn [x] (map
                                    (fn [y] [x y])
                                    (filter #(edges1 x %) base)))
                          base))))))

;; End of file

