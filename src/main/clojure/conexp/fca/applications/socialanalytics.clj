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

;;;;Functions to compute adjacency-matricies

(defn combined-projection-adjacency-matrix
  "Computes the adjacency-matrix of the graph which has the objects and 
  attributes as vertices and the edges defined via the incidence-relation.
  The edges of the graph have no direction, therefore just the upper entrys
  a_ij with i<=j have to be stored."
  ^"[[I" [context]
  (assert (context? context) "Argument must be a formal context!")
  (let [objects (vec (objects context))
        attributes (vec (attributes context))
        m (count objects)
        n (count attributes)
        compute-row-for-object (fn [object attributes i]
                                 ;; Computes for an `object' the row
                                 ;; in the adjacency-matrix: Decides with which
                                 ;; of the `attributes' the object has an edge with.
                                 ;; Starts the row with `i' zeros.
                                 (concat (take i (repeat 0))
                                         (map
                                           (fn [attribute]
                                             (if (incident? context object attribute)
                                               1
                                               0))
                                           attributes)))]
    (into-array (map int-array (concat
                                 ;; Concat the rows corresponding to an object...
                                 (map #(compute-row-for-object (objects %)
                                                               attributes
                                                               (- m %))
                                      (range 0 m))
                                 ;; ... with the rows correpsonding to attributes.
                                 (map #(take (- n %) (repeat 0))
                                      (range 0 n)))))))

(defn- general-adjacency-matrix
  "This is a helper function to compute the adjacency matricies of the object- and 
  the attribute-projection.

  Computes the upper half of the adjacency matrix of the undirected graph
  with the nodeset `nodeset' and in which two nodes n1, n2 share an edge if
  the intersection of (derivation `context' n1) and (derivation `context' n2)
  is not empty."
  ^"[[I" [context derivation nodeset]
  (let [derivations (mapv
                      #(derivation context #{%})
                      nodeset)
        n (count derivations)
        compute-row  (fn [dev devs] (mapv
                                      #(let [sharedthings
                                             (intersection dev %)]
                                         (if (empty? sharedthings) 0 1))
                                      devs))]
    (into-array
      (map int-array (map
                       #(compute-row
                          (nth derivations %)
                          (drop % derivations))
                       (range 0 n))))))

(defn object-projection-adjacency-matrix
  "Computes the adjacency-matrix for the graph, which has
  the objects of a `context' as vertices and in which two
  objects share an edge if they share an attribute.
  The edges of the graph have no direction, therefore just
  the upper entrys a_ij with i<=j have to be stored."
  ^"[[I" [context]
  (general-adjacency-matrix context object-derivation (objects context)))

(defn attribute-projection-adjacency-matrix
  "Computes the adjacency-matrix for the graph, which has
  the attributes of a `context' as vertices and in which two attributes
  share an edge if they share an object.
  The edges of the graph have no direction, therefore just the upper
  entrys a_ij with i<=j have to be stored"
  ^"[[I"[context]
  (general-adjacency-matrix context attribute-derivation (attributes context)))

;;; Functions to compute adjacency-maps.
;;; The following functions take a context as argument and return
;;; graphs, represented by adjacency-maps in the form
;;; {node1 set-of-neighbours, node2 set-of-neighbours...}.

(defn combined-projection
  "Computes for a given `context' the adjacency-map
  of the graph, which has as vertices the objects
  and attributes and in which the edges are defined
  through the incidence-relation.
  As it is possible for an object and an attribute
  to have the same name, the vertices belonging to objects
  are renamed from object1, object2 to
  obj-object1, obj-object2,... and vertices belonging to
  attributes are renamed from attribute1,attribute2 to
  atr-attribute1,atr-attribute2..."
  [context]
  (let [object-nodes
        ;; Computes the successors
        ;; for all vertices, which
        ;; correspond to objects.
        (reduce
          (fn [hmap obj]
            (assoc hmap (str 'obj- obj) 
                   (set
                     (map
                       #(str 'atr- %)
                       (object-derivation context #{obj})))))
          {}
          (objects context))
        attribute-nodes
        ;; Computes the successors
        ;; for all vertices, which
        ;; correspond to attributes.
        (reduce
          (fn [hmap atr]
            (assoc hmap (str 'atr- atr)
                   (set
                     (map
                       #(str 'obj- %)
                       (attribute-derivation context #{atr})))))
          {}
          (attributes context))]
    (merge object-nodes attribute-nodes)))

(defn general-projection
  "This is a helper function for object-projection and
  attriube-projection to avoid unnecessary doubled code.
  
  Computes the graph with the nodes `node-set' and in 
  which two nodes n1 and n2 share an edge if there is an c
  in `connection-set' with n1,n2 in
  (derivation context #{c})."
  [context derivation node-set connection-set]
  (let [init-vertices
        ;; We initialize all nodes
        ;; of the context as verticies
        ;; without edges.
        (reduce
          (fn [hmap node]
            (assoc hmap node #{}))
          {}
          node-set)
        add-edges
        ;; This function takes a map `hmap'
        ;; and a `set' and adds all elements
        ;; of set to all those keys of hmap,
        ;; whose are elements of set
        ;; themselves.]
        (fn [hmap set]
          (reduce
            (fn [currenthmap element]
              (update-in currenthmap
                         [element]
                         union
                         set))
            hmap
            set))]
    ;; Iterate now through all elements `connection'
    ;; in connection-set to find the edges n1<->n2
    ;; for all n1, n2 in (derivation context connection).
    (reduce
      (fn [hmap connection]
        (add-edges hmap (derivation context #{connection})))
      init-vertices
      connection-set)))

(defn object-projection
  "Computes for a `context' the adjacency-map
  of the graph which has as vertices
  the objects of a context and in which
  two objects share an edge if they share an
  attribute."
  [context]
  (general-projection context
                      attribute-derivation
                      (objects context)
                      (attributes context)))

(defn attribute-projection
  "Computes for a `context' the adjacency-map
  of the graph which has as vertices
  the attributes of a context and in which
  two attributes share an edge if they share an
  object."
  [context]
  (general-projection context
                      object-derivation
                      (attributes context)
                      (objects context)))

;;; Average-shortest-path

;;The following two marcros are adapted from
;;http://clojure-goes-fast.com/blog/java-arrays-and-unchecked-math/.
(defmacro two-dimensional-aget [a i j]
  `(aget ^"[I" (aget ~a ~i) ~j))

(defmacro two-dimensional-aset [a i j v]
  `(aset ^"[I" (aget ~a ~i) ~j ~v))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- floyd-step
  "This is a helper-function for average-shortest-path:
  Do one overwriting in the floyd-algorithm, see
  https://de.wikipedia.org/wiki/Algorithmus_von_Floyd_und_Warshall or
  https://en.wikipedia.org/wiki/Floyd-Warshall_algorithm for Details."
  [^"[[I" matrix ^Integer k ^Integer i ^Integer j]
  (assert (<= i j) "No computation under the diagonalelements possible!")
  (let [A_ij (two-dimensional-aget matrix i (- j i))
        A_ik (if (<= i k)
               (two-dimensional-aget matrix i (- k i))
               (two-dimensional-aget matrix k (- i k)))
        A_kj (if (<= k j)
               (two-dimensional-aget matrix k (- j k))
               (two-dimensional-aget matrix j (- k j)))
        ^Integer newvalue (cond
                            (and (= A_ij 0) (or (= A_ik 0) (=  A_kj 0))) 0
                            (= A_ij 0) (+ A_ik A_kj)
                            (or (= A_ik 0) (=  A_kj 0)) A_ij
                            :else (min A_ij (+ A_ik A_kj)))]
    (two-dimensional-aset matrix i (- j i) newvalue)))

(defn average-shortest-path
  "Computes the average-shortest path for a given `context' and a `projection'.
  The projection f should map a context to the upper half of the adjacency-matrix
  of the corresponding undirected graph.
  To compute the path-lenghts, the floyd-algorithm:
  https://de.wikipedia.org/wiki/Algorithmus_von_Floyd_und_Warshall,
  https://en.wikipedia.org/wiki/Floyd-Warshall_algorithm is used
  with one modification: Because the graph is undirected, just the upper triangle
  (including diagonal-elements) of the adjacency-matrix
  has to be stored.
  Paths from a vertex to itself are discarded. 
  If there are no edges and therefore no paths
  in the graph, nil is returned."
  [context projection]
  (assert (context? context) "Argument is not a formal context!")
  (let [^"[[I" matrix (projection context)
        n (count matrix)
        paths (do
                (dorun
                  (for [k (range 0 n) i (range 0 n) j (range i n)]
                    (floyd-step matrix k i j)))
                (remove zero?
                        (mapcat #(drop 1 %)
                                matrix)))]
    (if (empty? paths)
      nil
      (/ (reduce + paths) (count paths)))))

(defn combined-projection-average-shortest-path
  "Computes for a `context' the average-shortest-path of the graph,
   which has as vertices the objects and attributes of the context
   and in which the edges are defined through the incidence-relation."
  [context]
  (average-shortest-path context combined-projection-adjacency-matrix))

(defn object-projection-average-shortest-path
  "Computes fo a `context' the average-shortest-path of the graph,
   which has as vertices the objects of the context and in which
   two objects share an edge if they share an attribute."
  [context]
  (average-shortest-path context object-projection-adjacency-matrix))

(defn attribute-projection-average-shortest-path
  "Computes for a `context' the average-shortest-path of the graph,
   which has as vertices the attributes of the context and in which
   two attributes share an edge if they share an object."
  [context]
  (average-shortest-path context attribute-projection-adjacency-matrix))

;;;vertex-degrees

(defn vertex-degrees
  "For a given `context' and a `projection', which maps
  contexts to graphs, represented by adjacency-maps,
  the seq of vertex-degrees of (projection context) is returned."
  [context projection]
  (assert (context? context) "First argument must be a formal context!")
  (map
    count
    (vals (projection context))))

(defn combined-projection-vertex-degrees
  "For a given `context', the seq of vertex-degrees
  of the graph, which has as vertices the objects
  and attributes of the context and in which the edges
  are defined through the incidence-relation, is returned."
  [context]
  ;; Note that this function does not use
  ;; the above vertex-degrees function
  ;; for airbitary projections.
  ;; The reason therefore is,
  ;; that the special construction of
  ;; this specific graph allows to directly
  ;; compute the list of the vertex-degrees
  ;; from the context.
  (assert (context? context) "Argument must be a formal context!")
  (concat
    (map
      #(count (object-derivation context #{%}))
      (objects context))
    (map
      #(count (attribute-derivation context #{%}))
      (attributes context))))

(defn object-projection-vertex-degrees
  "For a given `context', this function returns
  the vertex-degrees of the graph, which has
  as vertices the objects of the context
  and in which two objects share an edge if they share
  an attribute."
  [context]
  (vertex-degrees context object-projection))

(defn attribute-projection-vertex-degrees
  "For a given `context', this function returns
  the vertex-degrees of the graph, which has
  as vertices the attributes of the context and
  in which two attributes share an edge if they share
  an object."
  [context]
  (vertex-degrees context attribute-projection))

;;;
nil
