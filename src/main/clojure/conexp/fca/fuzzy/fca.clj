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
        [conexp.fca.fuzzy sets logics]))


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
  (Fuzzy-Context. (objects mv-ctx) (attributes mv-ctx) (make-fuzzy-set (incidence mv-ctx))))

(defmethod print-method Fuzzy-Context
  [ctx out]
  (.write ^java.io.Writer out
          ^String (mv-context-to-string (make-mv-context (objects ctx)
                                                         (attributes ctx)
                                                         (fn [a b] ((incidence ctx) [a b]))))))

;;;

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

(defn fuzzy-object-derivation
  "Computes the fuzzy derivation of the fuzzy set C of objects in the given context, using hedge if
  given."
  ([context C]
     (fuzzy-object-derivation context C identity))
  ([context C hedge]
     (let [inz (incidence context),
           C   (make-fuzzy-set C)]
       (make-fuzzy-set (map-by-fn (fn [m]
                                    (reduce (fn [a g]
                                              (f-and a (f-impl (hedge (C g)) (inz [g m]))))
                                            1
                                            (objects context)))
                                  (attributes context))))))

(defalias fuzzy-oprime fuzzy-object-derivation)

(defn fuzzy-attribute-derivation
  "Computes the fuzzy derivation of the fuzzy set D of attributes in the given context."
  [context D]
  (let [inz (incidence context),
        D   (make-fuzzy-set D)]
    (make-fuzzy-set (map-by-fn (fn [g]
                                 (reduce (fn [a m]
                                           (f-and a (f-impl (D m) (inz [g m]))))
                                         1
                                         (attributes context)))
                               (objects context)))))

(defalias fuzzy-aprime fuzzy-attribute-derivation)

(defn doubly-scale-fuzzy-context
  "Returns the doubly scaled formal context for the given fuzzy
  context ctx."
  [ctx]
  (let [inz  (incidence ctx),
        vals (-> (set (vals inz)) (disj 0)),
        objs (set-of [g ny]
                     [g (objects ctx),
                      ny vals]),
        atts (set-of [m lambda]
                     [m (attributes ctx),
                      lambda vals]),
        inci  (set-of [[g ny] [m lambda]]
                      [[g ny] objs,
                       [m lambda] atts,
                       :when (<= (f-star ny lambda)
                                 (inz [g m]))])]
    (make-context objs atts inci)))



(defn globalization-hedge [x]
  "Globalization hedge function."
  (if (= x 1) 1 0))

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

; Pairs of t-norms and s-norms
(def lukasiewicz-norm [#(max 0 (+ %1 %2 -1)) 
                       #(min 1 (+ 1 (- %1) %2))])

(def goedel-norm [#(min %1 %2) 
                  #(if (<= %1 %2) 1 %2)])

(def product-norm [#(* %1 %2)
                   #(if (<= %1 %2) 1 (/ %2 %1))])

(defmulti t-norm
  "Returns the t-norm and it's residuum corresponding to the name given."
  {:arglists '([(t-norm-name)])}
  (fn [x & args]
    (if args
      (illegal-argument "Wrong number of arguments given.")
      x)))

(defmethod t-norm :default
  [norm]
  (illegal-argument "Norm " (str norm) " is not known."))

(defmethod t-norm :lukasiewicz
  [_]
  [(fn [x y] (max 0 (+ x y -1))),
   (fn [x y] (min 1 (+ 1 (- x) y)))])

;; (defmethod t-norm :łukasiewicz
;;   [_]
;;   (t-norm :lukasiewicz))

(defmethod t-norm :goedel
  [_]
  [(fn [x y] (min x y)),
   (fn [x y] (if (<= x y) 1 y))])

;; (defmethod t-norm :gödel
;;   [_]
;;   (t-norm :goedel))

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
