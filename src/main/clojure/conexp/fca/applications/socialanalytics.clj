;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.applications.socialanalytics
  "Provides some functionallity for Socialanalytics."
  (:require [conexp.fca.contexts :refer :all]
            [clojure.set :refer [intersection
                                 union]]))

;;;;Functions to compute adjacency-matricies

(defn adjacency-matrix-for-object-and-attribute-projection
  "Computes the adjacency-matrix of the graph which has the objects and 
  attributes as verticies and the edges defined via the incidence-relation.
  The edges of the graph have no direction, therefore just the upper entrys
  a_ij with i<=j have to be stored."
  [context]
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
                                 (vec (concat (take i (repeat 0))
                                              (map
                                                (fn [attribute]
                                                  (if (incident? context object attribute)
                                                    1
                                                    0))
                                                attributes))))]
    (vec (concat
           ;; Concat the rows corresponding to an object...
           (map #(compute-row-for-object (objects %)
                                         attributes
                                         (- m %))
                (range 0 m))
           ;; ... with the rows correpsonding to attributes.
           (map #(vec (take (- n %) (repeat 0)))
                (range 0 n))))))

(defn- compute-row-for-object-or-attribute-matrix
  "Tis is a helper-function to initialize the adjacency-matrix
  for the object- and attribute-projection.
  This function takes the derivation `object-or-attribute-derivation' of
  an object or attribute x1 and a seq `object-or-attribute-derivations'
  of derivations of objects or attributes to decide which objects
  or attributes of the seq share an edge with x1."
  [object-or-attribute-derivation object-or-attribute-derivations]
  (mapv
    #(let [sharedthings
           (intersection object-or-attribute-derivation %)]
       (if (empty? sharedthings) 0 1))
    object-or-attribute-derivations))

(defn adjacency-matrix-for-object-projection
  "Computes the adjacency-matrix for the graph, which has
  the objects of a `context' as verticies and in which two
  objects share an edge if they share an attribute.
  The edges of the graph have no direction, therefore just
  the upper entrys a_ij with i<=j have to be stored."
  [context]
  (let [object-derivations-of-context (mapv
                                        #(object-derivation context #{%})
                                        (objects context))
        n (count object-derivations-of-context)]
    (mapv
      #(compute-row-for-object-or-attribute-matrix
         (nth object-derivations-of-context %)
         (drop % object-derivations-of-context))
      (range 0 n))))

(defn adjacency-matrix-for-attribute-projection
  "Computes the adjacency-matrix for the graph, which has
  the attributes of a `context' as verticies and in which two attributes
  share an edge if they share an object.
  The edges of the graph have no direction, therefore just the upper
  entrys a_ij with i<=j have to be stored"
  [context]
  (let [attribute-derivations-of-context (mapv
                                           #(attribute-derivation context #{%})
                                           (attributes context))
        n (count attribute-derivations-of-context)]
    (mapv
      #(compute-row-for-object-or-attribute-matrix
         (nth attribute-derivations-of-context %)
         (drop % attribute-derivations-of-context))
      (range 0 n))))

;;; Functions to compute adjacency-maps.
;;; The following functions take a context as argument and return
;;; graphs, represented by adjacency-maps in the form
;;; {node1 set-of-neighbours, node2 set-of-neighbours...}.

(defn object-and-attribute-projection
  "Computes for a given `context' the adjacency-map
  of the graph, which has as verticies the objects
  and attributes and in which the edges are defined
  through the incidence-relation.
  As it is possible for an object and an attribute
  to have the same name, the verticies belonging to objects
  are renamed from object1, object2 to
  obj-object1, obj-object2,... and verticies belonging to
  attributes are renamed from attribute1,attribute2 to
  atr-attribute1,atr-attribute2..."
  [context]
  (let [object-nodes
        ;; Computes the successors
        ;; for all verticies, which
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
        ;; for all verticies, which
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

(defn- add-edges
  "This is a helper-function to compute
  the objects-projection and the attributes-projection
  of a context.

  It takes a map `hmap' and a `set' and adds all elements
  of set to all those keys of hmap, whose are elements of
  set themselves."
  [hmap set]
  (reduce
    (fn [currenthmap element]
      (update-in currenthmap
                 [element]
                 union
                 set))
    hmap
    set))

(defn object-projection
  "Computes for a `context' the adjacency-map
  of the graph which has as verticies
  the objects of a context and in which
  two objects share an edge if they share an
  attribute."
  [context]
  (let [init-verticies
        ;; We initialize all objects
        ;; of the context as verticies
        ;; without edges.
        (reduce
          (fn [hmap object]
            (assoc hmap object #{}))
          {}
          (objects context))]
    ;; Iterate through all attributes `attribute' to
    ;; add the edges o1<->02 for all objects
    ;; o1,o2 that have the `attribute'.
    (reduce
      (fn [hmap attribute]
        (add-edges hmap (attribute-derivation context #{attribute})))
      init-verticies
      (attributes context))))

(defn attribute-projection
  "Computes for a `context' the adjacency-map
  of the graph which has as verticies
  the attributes of a context and in which
  two attributes share an edge if they share an
  object."
  [context]
  (let [init-verticies
        ;; We initialize all attributes
        ;; of the context as verticies
        ;; without edges.
        (reduce
          (fn [hmap attribute]
            (assoc hmap attribute #{}))
          {}
          (attributes context))]
    ;; Iterate through all objects `object' to
    ;; add the edges a1<->a2 for all attributes
    ;; a1,a2 that this `object' has.
    (reduce
      (fn [hmap object]
        (add-edges hmap (object-derivation context #{object})))
      init-verticies
      (objects context))))

;;; Average-shortest-path

(defn- floyd-step
  "This is a helper-function for average-shortest-path:
  Do one overwriting in the floyd-algorithm, see
  https://de.wikipedia.org/wiki/Algorithmus_von_Floyd_und_Warshall or
  https://en.wikipedia.org/wiki/Floyd-Warshall_algorithm for Details."
  [matrix k i j]
  (assert (<= i j) "No computation under the diagonalelements possible!")
  (let [A_ij (nth (nth matrix i) (- j i))
        A_ik (if (<= i k)
               (nth (nth matrix i) (- k i))
               (nth (nth matrix k) (- i k)))
        A_kj (if (<= k j)
               (nth (nth matrix k) (- j k))
               (nth (nth matrix j) (- k j)))
        newvalue (cond
                   (and (= A_ij 0) (or (= A_ik 0) (=  A_kj 0))) 0
                   (= A_ij 0) (+ A_ik A_kj)
                   (or (= A_ik 0) (=  A_kj 0)) A_ij
                   :else (min A_ij (+ A_ik A_kj)))]
    (assoc matrix
           i
           (assoc (nth matrix i) (- j i) newvalue))))

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
  Paths from a vertice to itself are discarded. 
  If there are no edges and therefore no paths
  in the graph, nil is returned."
  [context projection]
  (assert (context? context) "Argument is not a formal context!")
  (let [matrix (projection context)
        n (count matrix)
        paths (loop [k 0
                     i 0
                     j 0
                     matrix matrix]
                (if (< k n)
                  (if (< i n)
                    (if (< j n)
                      (recur k i (inc j) (floyd-step matrix k i j))
                      (recur k (inc i) (inc i) matrix))
                    (recur (inc k) 0 0 matrix))
                  ;;If we are finished, we discard all
                  ;; entrys of length 0 (they stand for verticies
                  ;; which are not connected!) and all shortest-path-lengths
                  ;; of a vertice to itself.
                  (remove zero?
                          (mapcat
                            #(drop 1 %)
                            matrix))))]
    (if (empty? paths)
      nil
      (/ (reduce + paths) (count paths)))))

(defn average-shortest-path-objects-and-attributes
  "Computes for a `context' the average-shortest-path of the graph,
   which has as verticies the objects and attributes of the context
   and in which the edges are defined through the incidence-relation."
  [context]
  (average-shortest-path context adjacency-matrix-for-object-and-attribute-projection))

(defn average-shortest-path-objects
  "Computes fo a `context' the average-shortest-path of the graph,
   which has as verticies the objects of the context and in which
   two objects share an edge if they share an attribute."
  [context]
  (average-shortest-path context adjacency-matrix-for-object-projection))

(defn average-shortest-path-attributes
  "Computes for a `context' the average-shortest-path of the graph,
   which has as verticies the attributes of the context and in which
   two attributes share an edge if they share an object."
  [context]
  (average-shortest-path context adjacency-matrix-for-attribute-projection))

;;;Vertice-degrees

(defn vertice-degrees
  "For a given `context' and a `projection', which maps
  contexts to graphs, represented by adjacency-maps,
  the seq of vertice-degrees of (projection context) is returned."
  [context projection]
  (assert (context? context) "First argument must be a formal context!")
  (map
    count
    (vals (projection context))))

(defn vertice-degrees-objects-and-attributes
  "For a given `context', the seq of vertice-degrees
  of the graph, which has as verticies the objects
  and attributes of the context and in which the edges
  are defined through the incidence-relation, is returned."
  [context]
  ;; Note that this function does not use
  ;; the above vertice-degrees function
  ;; for airbitary projections.
  ;; The reason therefore is,
  ;; that the special construction of
  ;; this specific graph allows to directly
  ;; compute the list of the vertice-degrees
  ;; from the context.
  (assert (context? context) "Argument must be a formal context!")
  (concat
    (map
      #(count (object-derivation context #{%}))
      (objects context))
    (map
      #(count (attribute-derivation context #{%}))
      (attributes context))))

(defn vertice-degrees-objects
  "For a given `context', this function returns
  the vertice-degrees of the graph, which has
  as verticies the objects of the context
  and in which two objects share an edge if they share
  an attribute."
  [context]
  (vertice-degrees context object-projection))

(defn vertice-degrees-attributes
  "For a given `context', this function returns
  the vertice-degrees of the graph, which has
  as verticies the attributes of the context and
  in which two attributes share an edge if they share
  an object."
  [context]
  (vertice-degrees context attribute-projection))

;;;
nil
