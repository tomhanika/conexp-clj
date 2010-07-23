;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.fuzzy.fca
  (:use conexp.main
        [conexp.contrib.fuzzy sets logics]))

(ns-doc "Basic definitions for Fuzzy FCA")

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
    (when-not (forall [[_ v] (incidence mv-ctx)] (and (number? v) (<= 0 v 1)))
      (illegal-argument "Given function does not return real values between 0 and 1."))
    mv-ctx))

(defmethod make-fuzzy-context [clojure-coll clojure-coll clojure-coll]
  [objects attributes values]
  (let [mv-ctx (make-mv-context-from-matrix objects attributes values)]
    (when-not (forall [[_ v] (incidence mv-ctx)] (and (number? v) (<= 0 v 1)))
      (illegal-argument "Given value table does not contain of real values between 0 and 1."))
    mv-ctx))

(defn make-fuzzy-context-from-matrix
  "Creates a fuzzy context from the given (number of) objects, (number
  of) attributes and the value table, which must contain only real
  values between 0 and 1."
  [objects attributes values]
  (make-fuzzy-context objects attributes values))

;;;

(defn- ensure-fuzzy-set
  "Ensures that given argument, which may also be a hash map, is a
  fuzzy set."
  [thing]
  (cond
   (fuzzy-set? thing) thing,
   (map? thing) (make-fuzzy-set thing),
   :else (illegal-argument (str thing) " cannot be transformed to a fuzzy set.")))

(defn fuzzy-object-derivation
  "Computes the fuzzy derivation of the fuzzy set C of objects in
  the given context."
  [context C]
  (let [inz (incidence context),
        C   (ensure-fuzzy-set C)]
    (make-fuzzy-set (map-by-fn (fn [m]
                                 (reduce (fn [a g]
                                           (f-and a (f-impl (C g) (inz [g m]))))
                                         1
                                         (objects context)))
                               (attributes context)))))

(defn fuzzy-attribute-derivation
  "Computes the fuzzy derivation of the fuzzy set D of attributes in
  the given context."
  [context D]
  (let [inz (incidence context),
        D   (ensure-fuzzy-set D)]
    (make-fuzzy-set (map-by-fn (fn [g]
                                 (reduce (fn [a m]
                                           (f-and a (f-impl (D m) (inz [g m]))))
                                         1
                                         (attributes context)))
                               (objects context)))))

;;;

nil
