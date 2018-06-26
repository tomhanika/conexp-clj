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
            [clojure.set :refer [intersection]]))

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

;;;
nil
