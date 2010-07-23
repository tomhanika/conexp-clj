;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.fuzzy.logics
  (:use conexp.main))

(ns-doc "Basic definitions for fuzzy logics")

;;; T-Norms

(defmulti t-norm
  "Returns the t-norm and it's residuum corresponding to the name given."
  {:arglists '[(t-norm-name)]}
  identity)

(defmethod t-norm :default
  [norm]
  (illegal-argument "Norm " (str norm) " is not known."))

(defmethod t-norm :łukasiewicz
  [_]
  [(fn [x y] (max 0 (+ x y -1))),
   (fn [x y] (min 1 (+ 1 (- x) y)))])

(defmethod t-norm :lukasiewicz
  [_]
  (t-norm :łukasiewicz))

(defmethod t-norm :gödel
  [_]
  [(fn [x y] (min x y)),
   (fn [x y] (if (<= x y) 1 y))])

(defmethod t-norm :goedel
  [_]
  (t-norm :gödel))

(defmethod t-norm :product
  [_]
  [(fn [x y] (* x y)),
   (fn [x y] (if (<= x y) 1 (/ y x)))])

;;; Basic Fuzzy Logic

(defmacro- define-fuzzy-operator
  "Defines a fuzzy operator, which throws an
  UnsupportedOperationException when called. The operator is meant to be
  rebound."
  [name arity]
  `(defn ~name ~(vec (map (fn [_] (gensym)) (range arity)))
     (unsupported-operation "You need to choose a logic with with-fuzzy-logic.")))

(define-fuzzy-operator f-star 2)
(define-fuzzy-operator f-impl 2)
(define-fuzzy-operator f-and 2)
(define-fuzzy-operator f-or 2)
(define-fuzzy-operator f-neg 1)

(defmacro with-fuzzy-logic
  "For the given t-norm norm and the names of the corresponding operators evaluates body in an
  dynamic environment where the fuzzy logic for norm is in effect."
  [norm & body]
  `(let [[x# y#] (t-norm ~norm)]
     (binding [~'f-star x#,
               ~'f-impl y#,
               ~'f-and (fn [x# y#] (f-star x# (f-impl x# y#))),
               ~'f-or  (fn [x# y#] (f-and (f-impl (f-impl x# y#) y#)
                                          (f-impl (f-impl y# x#) x#))),
               ~'f-neg (fn [x#] (f-impl x# 0))]
       ~@body)))

;;;

nil
