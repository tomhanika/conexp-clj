;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.math.algebra
  (:use conexp.base)
  (:require [conexp.fca.contexts :refer :all]))

;;; Datastructure

(defprotocol Order
  (base-set [order] "Returns the base set.")
  (order [order] "Returns a function of one or two arguments representing the 
                  order relation. If called with one argument it is assumed 
                  that this argument is a pair of elements."))

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

(defn poset-context
  "Returns the context of a poset."
  [poset]
  (make-context (base-set poset)
                (base-set poset)
                (fn [A B]
                  ((order poset) [A B]))))

;;;

(defn closure-induced-preorder
  "Given a closure operation returns the preorder function for two elements a
   and b; 'a (<=_L) b'."
  [closure]
  (fn [a b] (some #{b} (closure a))))

(defn closure-equivalence
  "Given a set and closure operation computes the equivalence classes."
  [set closure]
  (group-by closure set))

;;;

nil
