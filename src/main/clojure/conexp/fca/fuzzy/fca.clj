;; Copyright â the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.fuzzy.fca
  "Basic definitions for Fuzzy FCA"
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.many-valued-contexts
        [conexp.fca.fuzzy sets]))


(deftype Fuzzy-Context [objects attributes incidence]
    Object
  (equals [this other]
    (generic-equals [this other] Fuzzy-Context [objects attributes incidence]))
  (hashCode [this]
    (hash-combine-hash Fuzzy-Context objects attributes incidence))

  conexp.fca.contexts/Context
  (objects [this] objects)
  (attributes [this] attributes)
  (incidence [this] incidence))

(defn mv->fuzzy-context-nc
  "Converts a many-valued-context to a fuzzy context, without checking."
  [mv-ctx]
  (Fuzzy-Context. (objects mv-ctx) (attributes mv-ctx) (make-fuzzy-set (incidence mv-ctx)))
  )

(defmethod print-method Fuzzy-Context
  [ctx out]
  (.write ^java.io.Writer out
          ^String (mv-context-to-string (make-mv-context (objects ctx)
                                                         (attributes ctx)
                                                         (fn [a b] ((incidence ctx) [a b])))))
  )

(defmulti make-fuzzy-context
  "Creates a fuzzy context from the given attributes. A fuzzy context
  is nothing else than a Many-Valued Context with real entries between
  0 and 1."
  {:arglists '([objects attributes incidence])}
  (fn [& args]
    (vec (map clojure-type args))))

(defmethod make-fuzzy-context [clojure-coll clojure-coll clojure-fn]
  [objects attributes truth-function]
  (let [mv-ctx (make-mv-context (set objects)
                                (set attributes)
                                truth-function)]
    (when-not (forall [v (vals (incidence mv-ctx))] (and (number? v) (<= 0 v 1)))
      (illegal-argument "Given function does not return real values between 0 and 1."))
    (mv->fuzzy-context-nc mv-ctx)))

(defmethod make-fuzzy-context [clojure-coll clojure-coll clojure-coll]
  [objects attributes values]
  (let [mv-ctx (make-mv-context-from-matrix objects attributes values)]
    (when-not (forall [v (vals (incidence mv-ctx))] (and (number? v) (<= 0 v 1)))
      (illegal-argument "Given value table does not contain of real values between 0 and 1."))
    (mv->fuzzy-context-nc mv-ctx)))

(defn make-fuzzy-context-from-matrix
  "Creates a fuzzy context from the given (number of) objects, (number
  of) attributes and the value table, which must contain only real
  values between 0 and 1."
  [objects attributes values]
  (make-fuzzy-context objects attributes values))

;;;
(defn- fuzzy-operators [norm]
  "Returns fuzzy operations based on the supplied norm, that are required for more complex fuzzy operations."
  (let [t-norm (first norm)
        residuum (second norm)
        f-and #(t-norm %1 (residuum %1 %2))
        f-or #(f-and (residuum (residuum %1 %2) %2)
                     (residuum (residuum %2 %1) %1))
        f-neg #(residuum % 0)]
    [t-norm residuum f-and f-or f-neg])
  )

(defn fuzzy-object-derivation
  "Accepts a fuzzy context and a fuzzy set of objects.
   Computes the fuzzy object derivation of the supplied objects in the supplied fuzzy context."
  ([fctx fobjs norm]
    (fuzzy-object-derivation fctx fobjs norm identity))

  ([fctx fobjs norm hedge]
    (let [[t-norm residuum f-and f-or f-neg] (fuzzy-operators norm)
          inz (incidence fctx)]
      (make-fuzzy-set (into {} (for [attr (attributes fctx)]
                                  [attr
                                  (reduce f-and 1 (for [obj (keys fobjs)] (residuum (hedge (fobjs obj)) (inz [obj attr]))))])))))
  )

(defn fuzzy-attribute-derivation
  "Accepts a fuzzy context and a fuzzy set of attributes.
   Computes the fuzzy object derivation of the supplied attributes in the supplied fuzzy context."
  ([fctx fattrs norm]
   (fuzzy-attribute-derivation fctx fattrs norm identity))

  ([fctx fattrs norm hedge]
   (let [[t-norm residuum f-and f-or f-neg] (fuzzy-operators norm)
         inz (incidence fctx)]
     (make-fuzzy-set (into {} (for [obj (attributes fctx)]
                                [obj
                                (reduce f-and 1 (for [attr (keys fattrs)] (residuum (hedge (fattrs attr)) (inz [obj attr]))))])))))
  )

(defn globalization-hedge [x]
  "Globalization hedge function."
  (if (= x 1) 1 0)
  )

(defn fuzzy-subset?
  "Returns the degree to which fset1 is a subset of fset2. Applies hedge to the truth
  value of an element being in fset1, if given."
  ([fset1 fset2 norm]
   (subsethood fset1 fset2 norm identity))

  ([fset1 fset2 norm hedge]
   (let [[t-norm residuum f-and f-or f-neg] (fuzzy-operators norm)]
     (reduce #(f-and %1 (residuum (hedge (fset1 %2))
                                   (fset2 %2)))
             1
             (keys fset1))))
  )

(defn validity
  "Returns the degree to which the implication A ==> B is true in the
  given fuzzy-context. A and B are fuzzy subsets of the attributes of
  fuzzy-context."
  ([fuzzy-context A B]
     (validity fuzzy-context A B identity))
  ([fuzzy-context A B hedge]
     (subsethood (make-fuzzy-set B)
                 (fuzzy-oprime fuzzy-context
                               (fuzzy-aprime fuzzy-context
                                             (make-fuzzy-set A))
                               hedge))))

; Pairs of t-norms and residuum
(def lukasiewicz-norm [#(max 0 (+ %1 %2 -1)) 
                       #(min 1 (+ 1 (- %1) %2))])

(def goedel-norm [#(min %1 %2) 
                  #(if (<= %1 %2) 1 %2)])

(def product-norm [#(* %1 %2)
                   #(if (<= %1 %2) 1 (/ %2 %1))])

#_
(defmulti t-norm
  "Returns the t-norm and it's residuum corresponding to the name given."
  {:arglists '([(t-norm-name)])}
  (fn [x & args]
    (if args
      (illegal-argument "Wrong number of arguments given.")
      x)))
#_
(defmethod t-norm :default
  [norm]
  (illegal-argument "Norm " (str norm) " is not known."))
#_
(defmethod t-norm :lukasiewicz
  [_]
  [(fn [x y] (max 0 (+ x y -1))),
   (fn [x y] (min 1 (+ 1 (- x) y)))])

;; (defmethod t-norm :łukasiewicz
;;   [_]
;;   (t-norm :lukasiewicz))
#_
(defmethod t-norm :goedel
  [_]
  [(fn [x y] (min x y)),
   (fn [x y] (if (<= x y) 1 y))])

;; (defmethod t-norm :gödel
;;   [_]
;;   (t-norm :goedel))
#_
(defmethod t-norm :product
  [_]
  [(fn [x y] (* x y)),
   (fn [x y] (if (<= x y) 1 (/ y x)))])

;;; Basic Fuzzy Logic
#_
(defmacro- define-fuzzy-operator
  "Defines a fuzzy operator, which throws an
  UnsupportedOperationException when called. The operator is meant to be
  rebound."
  [name arity]
  `(defn ~name ~(vec (map (fn [_] (gensym)) (range arity)))
     (unsupported-operation "You need to choose a logic with with-fuzzy-logic.")))

;(define-fuzzy-operator f-star 2)
;(define-fuzzy-operator f-impl 2)
;(define-fuzzy-operator f-and 2)
;(define-fuzzy-operator f-or 2)
;(define-fuzzy-operator f-neg 1)
#_
(defmacro with-fuzzy-logic
  "For the given t-norm norm and the names of the corresponding operators evaluates body in an
  dynamic environment where the fuzzy logic for norm is in effect."
  [norm & body]
  `(let [[x# y#] (t-norm ~norm)]
     (with-altered-vars [~'f-star (constantly x#),
                         ~'f-impl (constantly y#),
                         ~'f-and  (constantly
                                   (fn [x# y#] (f-star x# (f-impl x# y#)))),
                         ~'f-or   (constantly
                                   (fn [x# y#] (f-and (f-impl (f-impl x# y#) y#)
                                                      (f-impl (f-impl y# x#) x#)))),
                         ~'f-neg  (constantly
                                   (fn [x#] (f-impl x# 0)))]
       ~@body)))


;;;

;;; TODO:
;;;  - Compute fuzzy concepts
;;;  - attribute exploration?
;;;  - non-redundant basis of implications
;;;  - glinclosure

;;;

nil

(def fctx (make-fuzzy-context [1 2 3 4]
                               [1 2 3 4 5 6]
                               [1.0 1.0 0.0 1.0 1.0 0.2,
                                1.0 0.4 0.3 0.8 0.5 1.0,
                                0.2 0.9 0.7 0.5 1.0 0.6,
                                1.0 1.0 0.8 1.0 1.0 0.5]))

(def fset1 (make-fuzzy-set #{1 2}))
(def fset2 (make-fuzzy-set {2 0.6 1 0.4}))
(def fset3 (make-fuzzy-set [5 6]))

(println (fuzzy-object-derivation fctx fset1 lukasiewicz-norm))

(println (subsethood fset1 fset2 lukasiewicz-norm))

