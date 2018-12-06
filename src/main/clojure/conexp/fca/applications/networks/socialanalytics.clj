;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.applications.networks.socialanalytics
  "Provides some functionallity for Socialanalytics."
  (:require [conexp.fca.contexts :refer [objects
                                         attributes
                                         object-derivation
                                         attribute-derivation
                                         context?]]
            [clojure.set :refer [intersection
                                 union]]
            [conexp.fca.applications.networks.structure
             :refer
             :all]))

;;; Average-shortest-path

;; The following two marcros are adapted from
;; http://clojure-goes-fast.com/blog/java-arrays-and-unchecked-math/.

(defmacro two-dimensional-aget [a i j]
  `(aget ^"[I" (aget ~a ~i) ~j))

(defmacro two-dimensional-aset [a i j v]
  `(aset ^"[I" (aget ~a ~i) ~j ~v))

(defn- floyd-step
  "Do one overwriting in the Floyd-Warshall-Algorithm."
  [^"[[I" matrix, ^Integer k, ^Integer i, ^Integer j]
  (assert (<= i j) "No computation under the diagonalelements possible.")
  (let [A_ij (two-dimensional-aget matrix i (- j i))
        A_ik (if (<= i k)
               (two-dimensional-aget matrix i (- k i))
               (two-dimensional-aget matrix k (- i k)))
        A_kj (if (<= k j)
               (two-dimensional-aget matrix k (- j k))
               (two-dimensional-aget matrix j (- k j)))
        ^Integer newvalue (cond
                            (and (= A_ij 0) (or (= A_ik 0) (=  A_kj 0)))
                            0
                            ;;
                            (= A_ij 0)
                            (+ A_ik A_kj)
                            ;;
                            (or (= A_ik 0) (=  A_kj 0))
                            A_ij
                            ;;
                            :else
                            (min A_ij (+ A_ik A_kj)))]
    (two-dimensional-aset matrix i (- j i) newvalue)))

(defn distance-matrix
  "Computes the distance-matrix for a given `context' and a function `fn'.
  `fn' should map a context to the upper half of the adjacency matrix of the
  corresponding undirected graph.  To compute the path lengths, the
  Floyd-Warshall-Algorithm is used with one modification: because the graph is
  undirected, just the upper triangle (including diagonal elements) of the
  adjacency matrix has to be stored."
  ^"[[I" [context fn]
  (assert (context? context) "Fist argument must be a formal context")
  (let [^"[[I" matrix (fn context)
        n (count matrix)]
    (do (dorun
          (for [k (range 0 n) i (range 0 n) j (range i n)]
            (floyd-step matrix k i j)))
      matrix)))

(defn average-shortest-path
  "Takes the upper half of a distance-matrix `matrix' and computes the average
  shortest path length of the corresponding undirected graph.  To compute the
  path-lengths, the Floyd-Warshall-Algorithm is used with one modification:
  because the graph is undirected, just the upper triangle (including
  diagonal-elements) of the adjacency-matrix has to be stored.  Paths from a
  vertex to itself are discarded.  If there are no edges and therefore no paths
  in the graph, nil is returned."
  [matrix]
  (let [distances (remove zero? (mapcat #(drop 1 %) matrix))]
    (if (empty? distances)
      nil
      (/ (reduce + distances) (count distances)))))

(defn context-graph-average-shortest-path
  "Computes for a `context' the average shortest path length of the graph that
  has as vertices the objects and attributes of the context and in which the
  edges are defined through the incidence-relation."
  [context]
  (assert (context? context) "Argument must be a formal context.")
  (average-shortest-path (distance-matrix context
                                          context-graph-adjacency-matrix)))

(defn object-projection-average-shortest-path
  "Computes fo a `context' the average shortest path length of the graph that
  has as vertices the objects of the context and in which two objects share an
  edge if they share an attribute."
  [context]
  (assert (context? context) "Argument must be a formal context.")
  (average-shortest-path (distance-matrix context
                                          object-projection-adjacency-matrix)))

(defn attribute-projection-average-shortest-path
  "Computes for a `context' the average shortest path length of the graph that
  has as vertices the attributes of the context and in which two attributes
  share an edge if they share an object."
  [context]
  (assert (context? context) "Argument must be a formal context.")
  (average-shortest-path (distance-matrix context
                                          attribute-projection-adjacency-matrix)))


;;; Average-shortest-path-via-breadth-first search
;;; Efficent for graphs with a low density.

(defn average-shortest-path-via-bfs
  "Computes for a given undirected `graph' the average shortest path length
  via breadth-first search. If the graph has no paths, nil is returned.
  Paths from a vertex to itself are discarded.
  If there are no paths in the graph, nil is returned.

  This function is efficient for graphs with a low density."
  [graph]
  (let [[sum-of-path-lengths amount-of-paths] (reduce (fn [[sum counter] x] [(+ sum x) (inc counter)])
                                                      [0 0]
                                                      (mapcat
                                                        #(vals (dissoc (breadth-first-search graph %) %))
                                                        (keys graph)))]
    (if (= 0 amount-of-paths)
      nil
      (/ sum-of-path-lengths amount-of-paths))))

(defn context-graph-average-shortest-path-via-bfs
  "Computes for a `context' the average shortest path length
  of the undirected graph which has as vertices the objects
  and attributes of the context and in which the edges are
  defined through the incidence relation.

  This function uses breadth-first search and therefore is efficent
  for contexts with a low density."
  [context]
  (assert (context? context) "Argument must be a formal context.")
  (average-shortest-path-via-bfs (context-graph context)))

(defn object-projection-average-shortest-path-via-bfs
  "Computes for a `context' the average shortest path length
  of the graph which has the objects as vertices and in which
  two objects share an edge if they share an attribute.

 This function uses breadth-first search and therefore is efficent
  for contexts with a low density."
  [context]
  (assert (context? context) "Argument must be a formal context.")
  (average-shortest-path-via-bfs (object-projection context)))

(defn attribute-projection-average-shortest-path-via-bfs
  "Computes for a `context' the average shortest path length
  of the graph which has the attributes as vertices and in which
  two attributes share an edge if they share an object.

  This function uses breadth-first search and therefore is efficent
  for contexts with a low density."
  [context]
  (assert (context? context) "Argument must be a formal context.")
  (average-shortest-path-via-bfs (attribute-projection context)))

;;; Vertex-degrees

(defn vertex-degrees
  "For a given `context' and a function `fn' that maps contexts to graphs
  represented by adjacency maps, the seq of vertex degrees of (projection
  context) is returned."
  [context fn]
  (assert (context? context) "First argument must be a formal context.")
  (map count (vals (fn context))))

(defn context-graph-vertex-degrees
  "For a given `context', the seq of vertex degrees is returned of the graph that
  has as vertices the objects and attributes of the context and in which the
  edges are defined through the incidence relation."
  [context]
  ;; Note that this function does not use the above vertex-degrees function for
  ;; arbitary projections.  The reason for this is, that the special
  ;; construction of this specific graph allows to directly compute the list of
  ;; the vertex degrees from the context.
  (assert (context? context) "Argument must be a formal context.")
  (concat (map #(count (object-derivation context #{%}))
               (objects context))
          (map #(count (attribute-derivation context #{%}))
               (attributes context))))

(defn object-projection-vertex-degrees
  "For a given `context', this function returns the vertex degrees of the graph
  which has as vertices the objects of the context and in which two objects
  share an edge if they share an attribute."
  [context]
  (vertex-degrees context object-projection))

(defn attribute-projection-vertex-degrees
  "For a given `context', this function returns the vertex degrees of the graph
  which has as vertices the attributes of the context and in which two
  attributes share an edge if they share an object."
  [context]
  (vertex-degrees context attribute-projection))


;;; K-cores

(defn k-cores-elimination
  "Returns for a given `graph' the maximial subgraph
  in which every vertice has at least `k' edges."
  [graph k]
  (assert (and (integer? k) (>= k 0)) "K must be a non-negative integer!")
  (let [keys-to-remove (set (filter #(< (count (graph %)) k) (keys graph)))]
    (if (empty? keys-to-remove)
      graph
      (let [graph-with-removed-vertices (apply dissoc graph keys-to-remove)
            graph-with-removed-vertices-and-edges
            (reduce
              (fn [hmap key]
                (update hmap key #(set (remove keys-to-remove %))))
              graph-with-removed-vertices
              (keys graph-with-removed-vertices))]
        (k-cores-elimination graph-with-removed-vertices-and-edges k)))))

(defn k-cores
  "Returns for a given `graph' the `k'-cores.
  These are the connected components of the maximal
  subgraph, in which all vertices have at least `k' edges."
  [graph k]
  (assert (and (integer? k) (>= k 0)) "K must be a non-negative integer!")
  (map
    #(set (keys %))
    (connected-components (k-cores-elimination graph k))))

(defn context-graph-k-cores
  "Returns fo a given `context' the `k'-cores of the graph
  which has as vertices the objects and attributes and in which
  the edges are defined through the incidence-relation."
  [context k]
  (assert (context? context) "First argument must be a formal context!")
  (k-cores (context-graph context) k))

(defn object-projection-k-cores
  "Returns for a given `context' the `k'-cores of the graph
  which has as vertices the objects and in which two vertices
  share an edge if they share an attribute."
  [context k]
  (assert (context? context) "First argument must be a formal context!")
  (k-cores (object-projection context) k))

(defn attribute-projection-k-cores
  "Returns for a given `context' the `k'-cores of the graph
  which has as vertices the attributes and in which two vertices
  share an edge if they share an object."
  [context k]
  (assert (context? context) "First argument must be a formal context!")
  (k-cores (attribute-projection context) k))


;;;Clustering Coefficients

(defn local-clustering-coefficient
  "Returns for a given `graph' and a given `node'
  the local clustering coefficient.
  If the node has at most one neighbour, 0 is returned.
  The graph should be represented by a hash-map with nodes as keys
  and sets of neighbours as values.
  See https://en.wikipedia.org/wiki/Clustering_coefficient
  for more information."
  [graph node]
  (let [neighbours (disj (graph node) node)
        n (count neighbours)
        count-neighbours
        ;; This function takes a node n1 that should be a
        ;; neighbour of node and counts how many of the other
        ;; neighbours of node share an edge with n1.
        (fn [node1]
          (let [neighbours-of-node1 (graph node1)]
            (count (intersection (disj neighbours-of-node1 node1)
                                 neighbours))))]
    (if (<= n 1)
      0
      (/ (reduce (fn [a node1]
                   (+ a (count-neighbours node1)))
                 0
                 neighbours),
       (* n (- n 1))))))

(defn clustering-coefficient
  "Returns for a given graph the average local
  clustering coefficient.
  See https://en.wikipedia.org/wiki/Clustering_coefficient for
  informations. Graphs should be represented by maps with
  nodes as keys and sets of neighbours as values."
  [graph]
  (assert (not (empty? graph)) "Graph has no nodes!")
  (/ (reduce + (map
                 #(local-clustering-coefficient graph %)
                 (keys graph)))
     (count (keys graph))))

(defn object-projection-clustering-coefficient
  "Returns for a given `context' the average local
  clusteringc oefficient of the graph which has as
  vertices the objects of the context and in which
  two objects share an edge if they share an attribute."
  [context]
  (assert (context? context) "Argument must be a formal context!")
  (clustering-coefficient (object-projection context)))

(defn attribute-projection-clustering-coefficient
  "Returns for a given `context' the average local
  clustering coefficient of the graph which has as
  vertices the attributes of the graph and in which
  two attributes share an edge if they share an object."
  [context]
  (assert (context? context) "Argument must be a formal context!")
  (clustering-coefficient (attribute-projection context)))

(defn two-mode-local-clustering-coefficient
  "Computes for a `node' of a bipartite `graph'
  the local clustering coefficient in the following
  manner:
  If for a node u N(U) is the neighbourhood, we define
  for all v in N(N(u) with v!=u the overlapping
  cc(u,v):= |N(u) intersection N(v)| / |N(u)|
  and the local clustering coefficient of u is then the
  average of all values cc(u,v) over all v in N(N(u))
  with v!=u.
  
  This is a modification of the coefficient in
  https://arxiv.org/pdf/cond-mat/0611631.pdf, page 12.

  A Graph should be represented by a map with the nodes as keys and the
  sets of neighbours as values."
  [graph node]
  (let [n (count (graph node))
        considerd-nodes (disj (apply union
                                     (vals (select-keys graph
                                                        (graph node))))
                              node)
        compute-overlapping (fn [node1]
                              (count (intersection (graph node)
                                                   (graph node1))))]
    (if (empty? considerd-nodes)
      0
      (/ (reduce (fn [val current-node]
                   (+ val (compute-overlapping current-node)))
                 0
                 considerd-nodes)
         (* n (count considerd-nodes))))))

(defn two-mode-clustering-coefficient
  "Computes for a given bipartite `graph' the average
  local clustering coefficient.

  A Graph should be represented by a map with the nodes as keys and
  the sets of neighbours as values."
  [graph]
  (assert (not (empty? graph)) "Graph has no nodes!")
  (let [nodes (keys graph)]
    (/ (reduce (fn [val current-node]
                 (+ val
                    (two-mode-local-clustering-coefficient graph
                                                           current-node)))
                 0
                 nodes)
       (count nodes))))

(defn context-graph-clustering-coefficient
  "Computes for a given `context' the average local 
  clustering coefficient of the bipartite context graph."
  [context]
  (assert (context? context) "Argument must be a formal context!")
  (two-mode-clustering-coefficient (context-graph context)))


;;; Betweenes-centrality

(defn- betweenes-centrality-step
  "Do one step in the brandes-algorithm to compute the betweenes-centrality
  of the graph `graph'

  See ``A Faster Algorithm for Betweenes Centrality`` by Ulrik Brandes for
  more information."
  [graph node values]
  (let [nodes (keys graph)]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY node)
           stack []
           P (zipmap nodes (repeat (list)))
           sigma (assoc (zipmap nodes (repeat 0))
                        node
                        1)
           distance (assoc (zipmap nodes (repeat -1))
                           node
                           0)]
      (if (empty? queue)
        (loop [stack stack
               gamma (zipmap nodes (repeat 0))
               values values]
          (if (empty? stack)
            values
            (let [w (peek stack)
                  new-gamma (reduce (fn [hmap node]
                                      (assoc hmap node
                                             (+ (hmap node)
                                                (* (double (/ (sigma node) (sigma w))) (+ 1 (gamma w))))))
                                    gamma
                                    (P w))]
              (recur (pop stack)
                     new-gamma
                     (if (= node w)
                       values
                       (assoc values w (+ (values w) (new-gamma w))))))))
        (let [v (peek queue)
             [queue-new P-new sigma-new distance-new]
             (loop [queue (pop queue)
                    distance distance
                    P P
                    sigma sigma
                    neighbours (graph v)]
               (if (empty? neighbours)
                     [queue P sigma distance]
                     (let [w (first neighbours)]
                       (cond (< (distance w) 0)
                             (recur (conj queue w)
                                    (assoc distance
                                           w
                                           (+ 1
                                              (distance v)))
                                    (assoc P w (conj (P w) v))
                                    (assoc sigma w (+ (sigma w) (sigma v)))
                                    (disj neighbours w))
                             ;;;
                             (= (distance w) (inc (distance v)))
                             (recur queue
                                    distance
                                    (assoc P w (conj (P w) v))
                                    (assoc sigma w (+ (sigma w) (sigma v)))
                                    (disj neighbours w))
                             ;;;
                             :else (recur queue
                                          distance
                                          P
                                          sigma
                                          (disj neighbours w))))))]
          (recur queue-new
                 (conj stack v)
                 P-new
                 sigma-new
                 distance-new))))))

(defn betweenes-centrality
  "Computes for a given `graph', represented as
  adjacency-map, the betweenes-cenrality of all nodes
  by using brandes algorithm.

  See ``A Faster Algorithm for Betweenes Centrality`` by
  Ulrik Brandes for more information."
  [graph]
  (let [init-values (zipmap (keys graph) (repeat 0))]
  (reduce (fn [values node]
            (betweenes-centrality-step graph node values))
          init-values
          (keys graph))))

(defn betweenes-centrality-normalized
  "Computes for a given `graph', represented as
  adjacency-map, the betweenes-centrality of all nodes
  by using brandes algorithm and normalizes the result.

   See ``A Faster algorithm for Betweenes Centrality`` by
  Ulrik Brandes for more information."
  [graph]
  (if (empty? graph)
    {}
    (let [centrality (betweenes-centrality graph)
          map-max (apply max (vals centrality))
          map-min (apply min (vals centrality))
          diff (- map-max map-min)]
      (cond (= 0 map-max)
            centrality
            ;;;
            (= 0 diff)
            (zipmap (keys centrality) (repeat 0))
            ;;;
            :else
            (reduce (fn [hmap node]
                      (assoc hmap
                             node
                             (/ (- (hmap node) map-min) diff)))
                    centrality
                    (keys centrality))))))


(defn context-graph-betweenes-centrality
  "Computes for a given `context' the betweenes-centrality of the
  context-graph"
  [context]
  (assert (context? context) "Argument must be a formal context.")
  (betweenes-centrality (context-graph context)))

(defn object-projection-betweenes-centrality
  "Computes for a given `context' the betweenes-centrality of
  the object-projection of the context-graph"
  [context]
  (assert (context? context) "Argument must be a formal context.")
  (betweenes-centrality (object-projection context)))

(defn attribute-projection-betweenes-centrality
  "Computes for a given `context' the betweenes-centrality of
  the attribute-projection of the context-graph"
  [context]
  (assert (context? context) "Argument must be a formal context.")
  (betweenes-centrality (attribute-projection context)))

(defn context-graph-betweenes-centrality-normalized
  "Computes for a given `context' the normalized betweenes-centrality of the
  context-graph"
  [context]
  (assert (context? context) "Argument must be a formal context.")
  (betweenes-centrality-normalized (context-graph context)))

(defn object-projection-betweenes-centrality-normalized
  "Computes for a given `context' the normalized betweenes-centrality of
  the object-projection of the context-graph"
  [context]
  (assert (context? context) "Argument must be a formal context.")
  (betweenes-centrality-normalized (object-projection context)))

(defn attribute-projection-betweenes-centrality-normalized
  "Computes for a given `context' the normalized betweenes-centrality of
  the attribute-projection of the context-graph"
  [context]
  (assert (context? context) "Argument must be a formal context.")
  (betweenes-centrality-normalized (attribute-projection context)))


;;;

nil
