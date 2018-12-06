;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.applications.networks.structure
  "Provides the functionality to transform contexts to graphs, represented by maps or
  adjacency-matrices. This namespace also includes some basic
  graph operations like breadth-first-search."
  (:require [conexp.fca.contexts :refer [objects
                                         attributes
                                         object-derivation
                                         attribute-derivation
                                         incident?]]
            [clojure.set :refer [union
                                 intersection]]))


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
  of all reachable nodes from `node' as keys and the distances to `node'
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
              (recur (apply conj visited new-nodes)
                     (apply conj (pop queue) new-nodes)))))]
    (do-bfs {node 0}
            (conj clojure.lang.PersistentQueue/EMPTY [node 0]))))

(defn connected-components
  "Returns for a given `graph', represented as an adjacency-map,
  a list of the connected components."
  [graph]
  (if (empty? graph)
    '()
    (let [component (select-keys graph
                                 (keys (breadth-first-search graph
                                                             (first (keys graph)))))]
      (cons component
            (connected-components (apply dissoc graph (keys component)))))))