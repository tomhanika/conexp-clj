;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.applications.socialanalytics
  "Provides some functionallity for Socialanalytics."
  (:require [conexp.fca.contexts :refer [objects
                                         attributes
                                         object-derivation
                                         attribute-derivation
                                         context?
                                         incident?]]
            [clojure.set :refer [intersection
                                 union]]))


;;; Functions to compute adjacency-matricies

(defn context-graph-adjacency-matrix
  "Computes the adjacency matrix of the context graph of `context'.
  The context graph has the objects and attributes of `context' as vertices and
  two verticies are connected if the pair is contained in the incidence
  relation.  As the edges of the graph have no direction, just the upper entrys
  a_ij with i ≤ j have to be stored."
  ^"[[I" [context]
  (let [objects (vec (objects context))
        attributes (vec (attributes context))
        m (count objects)
        n (count attributes)
        compute-row-for-object (fn [object attributes i]
                                 ;; Computes for an `object' the row in the
                                 ;; adjacency-matrix: Decides with which of the
                                 ;; `attributes' the object has an edge with.
                                 ;; Starts the row with `i' zeros.
                                 (concat (take i (repeat 0))
                                         (map
                                          (fn [attribute]
                                            (if (incident? context object attribute)
                                              1
                                              0))
                                          attributes)))]
    (into-array
     (map int-array (concat
                     ;; Concat the rows corresponding to an object...
                     (map #(compute-row-for-object (objects %) attributes (- m %))
                          (range 0 m))
                     ;; ... with the rows correpsonding to attributes.
                     (map #(take (- n %) (repeat 0))
                          (range 0 n)))))))

(defn- general-adjacency-matrix
  "This is a helper function to compute the adjacency matrices of object- and
  attribute-projection.

  Computes the upper half of the adjacency matrix of the undirected graph with
  the node set `node-set' and in which two nodes n₁, n₂ share an edge if the
  intersection of (derivation `context' n₁) and (derivation `context' n₂) is not
  empty."
  ^"[[I" [context derivation node-set]
  (let [derivations (mapv #(derivation context #{%}) node-set)
        n (count derivations)
        compute-row (fn [dev devs]
                      (int-array (map #(if (empty? (intersection dev %)) 0 1)
                                      devs)))]
    (into-array
     (map #(compute-row (nth derivations %) (drop % derivations))
          (range 0 n)))))

(defn object-projection-adjacency-matrix
  "Computes the adjacency matrix for the graph that has the objects of a `context'
  as vertices and in which two objects share an edge if they share an attribute.
  The edges of the graph have no direction, therefore just the upper entrys a_ij
  with i<=j have to be stored."
  ^"[[I" [context]
  (general-adjacency-matrix context object-derivation (objects context)))

(defn attribute-projection-adjacency-matrix
  "Computes the adjacency matrix for the graph that has the attributes of a
  `context' as vertices and in which two attributes share an edge if they share
  an object.  The edges of the graph have no direction, therefore just the upper
  entrys a_ij with i<=j have to be stored"
  ^"[[I" [context]
  (general-adjacency-matrix context attribute-derivation (attributes context)))


;;; Functions to compute adjacency-maps.

;; The following functions take a context as argument and return graphs
;; represented by adjacency-maps in the form {node1 set-of-neighbours, node2
;; set-of-neighbours...}.

(defn context-graph
  "Computes for a given `context' the adjacency map of the graph that has as
  vertices the objects and attributes and in which two verticies are connected
  if the pair is contained in the incidence relation.  As it is possible for an
  object and an attribute to have the same name, the vertices belonging to
  objects are renamed from object₁, object₂ to obj-object₁, obj-object₂,…, and
  vertices belonging to attributes are renamed from attribute₁, attribute₂ to
  atr-attribute₁,atr-attribute₂,…"
  [context]
  (let [object-nodes
        ;; Computes the successors for all vertices which correspond to
        ;; objects.
        (reduce
          (fn [hmap obj]
            (assoc hmap (str 'obj- obj)
                   (set (map #(str 'atr- %)
                             (object-derivation context #{obj})))))
          {}
          (objects context))
        attribute-nodes
        ;; Computes the successors for all vertices which correspond to
        ;; attributes.
        (reduce
          (fn [hmap atr]
            (assoc hmap (str 'atr- atr)
                   (set (map #(str 'obj- %)
                             (attribute-derivation context #{atr})))))
          {}
          (attributes context))]
    (merge object-nodes attribute-nodes)))

(defn general-projection
  "This is a helper function for object-projection and attriube-projection to
  avoid unnecessary duplicate code.

  Computes the graph with the nodes `node-set' and in which two nodes n₁ and n₂
  share an edge if there is an c in `connection-set' with n₁, n₂ ∈ (derivation
  context #{c})."
  [context derivation node-set connection-set]
  (let [init-vertices
        ;; We initialize all elements of the `node-set' as verticies without
        ;; edges.
        (reduce
          (fn [hmap node]
            (assoc hmap node #{}))
          {}
          node-set)
        add-edges
        ;; This function takes a map `hmap' and a `set' and adds all elements of
        ;; set to all those keys of hmap, whose are elements of set themselves.]
        (fn [hmap set]
          (reduce
            (fn [currenthmap element]
              (update-in currenthmap [element] union set))
            hmap
            set))]
    ;; Iterate now through all elements `connection' in connection-set to find
    ;; the edges n1<->n2 for all n1, n2 in (derivation context #{connection}).
    (reduce
      (fn [hmap connection]
        (add-edges hmap (derivation context #{connection})))
      init-vertices
      connection-set)))

(defn object-projection
  "Computes for a `context' the adjacency map of the graph that has as vertices
  the objects of `context' and in which two objects share an edge if they share
  an attribute."
  [context]
  (general-projection context
                      attribute-derivation
                      (objects context)
                      (attributes context)))

(defn attribute-projection
  "Computes for a `context' the adjacency map of the graph that has as vertices
  the attributes of `context' and in which two attributes share an edge if they
  share an object."
  [context]
  (general-projection context
                      object-derivation
                      (attributes context)
                      (objects context)))


;;; Breadth-first-search

(defn breadth-first-search
  "For a `graph', given as adjacency-map, this function returns a map
  of all reachable nodes from node as keys and the distances to node
  as values."
  [graph node]
  (assert (contains? graph node) "Second argument must be a node of the graph!")
  (let [do-bfs
        (fn [visited queue]
          (if (empty? queue)
            visited
            (let [[current-node current-depth] (peek queue)
                  new-nodes (map
                              #(vector % (+ 1 current-depth))
                              (remove visited (graph current-node)))]
              (recur (apply conj visited new-nodes) (apply conj (pop queue) new-nodes)))))]
    (do-bfs {node 0} (conj clojure.lang.PersistentQueue/EMPTY [node 0]))))

(defn connected-components
  "Returns for a given `graph', represented as an adjacency-map,
  a list of the connected components."
  [graph]
  (if (empty? graph)
    '()
    (let [component (select-keys graph (keys (breadth-first-search graph (first (keys graph)))))]
      (cons component (connected-components (apply dissoc graph (keys component)))))))


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

;;;k-cores

(defn- k-cores-elimination
"For a given `graph', returns the maximial subgraph,
  in which every vertice has at least `k' edges."
[graph k]
(assert (and (integer? k) (>= k 0)) "K must be a non-negative integer!")
(let [keys-to-remove (set (filter #(< (count (graph %)) k) (keys graph)))]
  (if (empty? keys-to-remove)
    graph
    (let [graph-with-removed-verticies (apply dissoc graph keys-to-remove)
          graph-with-removed-verticies-and-edges
          (reduce
            (fn [hmap key]
              (update hmap key #(set (remove keys-to-remove %))))
            graph-with-removed-verticies
            (keys graph-with-removed-verticies))]
      (k-cores-elimination graph-with-removed-verticies-and-edges k)))))

(defn k-cores
  "Returns for a given `graph' the `k'-cores.
  These are the connected components of the maximal
  subgraph, in which all veticies have at least `k' edges."
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

;;;

nil
