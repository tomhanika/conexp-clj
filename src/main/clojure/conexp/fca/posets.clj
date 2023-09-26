;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.posets
  (:require [conexp.base :refer :all]
            [conexp.math.algebra :refer :all]
            [conexp.fca.contexts :refer :all]
            [clojure.math.combinatorics :refer [cartesian-product]]))

(deftype Poset [base-set order-function]
  Object
  (equals [this other]
    (and (= (class this) (class other))
         (= (.base-set this) (.base-set ^Poset other))
         (let [order-this (order this),
               order-other (order other)]
           (or (= order-this order-other)
               (forall [x (.base-set this)
                        y (.base-set this)]
                 (<=> (order-this x y)
                      (order-other x y)))))))
  (hashCode [this]
    (hash-combine-hash Poset base-set))
  ;;;
  Order
  (base-set [this] base-set)
  (order [this]
    (fn order-fn
      ([pair] (order-function (first pair) (second pair)))
      ([x y] (order-function x y)))))



(defmethod print-method Poset [^Poset poset, ^java.io.Writer out]
  (.write out
          ^String (str "Poset on " (count (base-set poset)) " elements.")))

;;; Constructors

(defn make-poset-nc 
  ""
  [base-set order-relation]
  (Poset. (to-set base-set) order-relation))

(defn has-partial-order?
  "Given a poset checks if its order is indeed a partial order."
  [poset]
  (let [<= (order poset)]
    (and (forall [x (base-set poset)]
           (<= x x))
         (forall [x (base-set poset),
                  y (base-set poset)]
           (=> (and (<= x y)
                    (<= y x))
               (= x y)))
         (forall [x (base-set poset),
                  y (base-set poset),
                  z (base-set poset)]
           (=> (and (<= x y)
                    (<= y z))
               (<= x z))))))

(defn make-poset
  "Standard constructor for making a poset from a base set and an order 
   relation.
   Note: This function will test the resulting poset for being one,
   which may take some time. If you don't want this, use
   make-poset-nc."
  [base-set order-relation]
  (let [poset (make-poset-nc base-set order-relation)]
    (when-not (has-partial-order? poset)
      (illegal-argument "Given arguments do not describe a poset."))
    poset))

;; Converter

(defn poset-to-matrix
  "Returns the relational matrix of the base set as one continous vector.
   The function either takes only a poset and or the poset and a presorted 
   base set."
  ([poset]
    (poset-to-matrix poset (vec (base-set poset))))
  ([poset base]
    (let [ord  (order poset)]
      (vec
        (for [x base y base]
          (if (ord x y) 1 0))))))

;; Context

(defmulti poset-context
  "Computes the context of an ordered set."
  (fn [poset] (type poset)))

(defmethod poset-context :default
  [poset]
  (make-context (base-set poset)
                (base-set poset)
                (fn [A B]
                  ((order poset) [A B]))))

(defmethod poset-context Poset
  [poset]
  (make-context (base-set poset)
                (base-set poset)
                (fn [A B]
                  ((order poset) [A B]))))

;; Neighbours

(defn directly-neighboured?
  "Checks whether x is direct lower neighbour of y in poset."
  [poset x y]
  (let [order (order poset)]
    (and (not= x y)
         (order x y)
         (let [base  (disj (base-set poset) x y)]
           (forall [z base]
             (not (and (order x z) (order z y))))))))

(defn poset-upper-neighbours
  "Returns all direct upper neighbours of x in poset."
  [poset x]
  (set-of y [y (base-set poset)
             :when (directly-neighboured? poset x y)]))

(defn poset-lower-neighbours
  "Returns all direct lower neighbours of y in poset."
  [poset y]
  (set-of x [x (base-set poset)
             :when (directly-neighboured? poset x y)]))

(defn order-ideal
  "Returns the order ideal of a set x in poset."
  [poset x]
  (assert (subset? x (base-set poset))
          "x must be a subset of the ordered set.")
  (loop [ideal #{}
         lower-neighbours x
         next-element (first (difference lower-neighbours ideal))]
    (if (nil? next-element)
      ideal
      (recur (union ideal #{next-element})
             (union lower-neighbours (poset-lower-neighbours poset next-element))
             (first (difference lower-neighbours ideal))))))

(defn order-filter
  "Returns the order filter of a set x in poset."
  [poset x]
  (assert (subset? x (base-set poset))
          "x must be a subset of the ordered set.")
  (loop [filter #{}
         upper-neighbours x
         next-element (first (difference upper-neighbours filter))]
    (if (nil? next-element)
      filter
      (recur (union filter #{next-element})
             (union upper-neighbours (poset-upper-neighbours poset next-element))
             (first (difference upper-neighbours filter))))))

(defn poset-product 
  [a b]
  (make-poset (cartesian-product (base-set a) (base-set b))
              (fn [x y] (and ((order a) (first x) (first y)) 
                             ((order b) (last x) (last y)))))
)
