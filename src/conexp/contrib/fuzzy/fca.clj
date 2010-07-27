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

(deftype Fuzzy-Context [objects attributes incidence]
    Object
  (equals [this other]
    (generic-equals [this other] Fuzzy-Context [objects attributes incidence]))
  (hashCode [this]
    (hash-combine-hash Fuzzy-Context objects attributes incidence))
  ;;
  conexp.fca.contexts/Context
  (objects [this] objects)
  (attributes [this] attributes)
  (incidence [this] incidence))

(defn- mv->fuzzy-context-nc
  "Converts a many-valued-context to a fuzzy context, without checking."
  [mv-ctx]
  (Fuzzy-Context. (objects mv-ctx) (attributes mv-ctx) (make-fuzzy-set (incidence mv-ctx))))

(defmethod print-method Fuzzy-Context
  [ctx out]
  (.write ^java.io.Writer out
          ^String (print-mv-context (make-mv-context (objects ctx)
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
  "Computes the fuzzy derivation of the fuzzy set C of objects in
  the given context."
  [context C]
  (let [inz (incidence context),
        C   (make-fuzzy-set C)]
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
        D   (make-fuzzy-set D)]
    (make-fuzzy-set (map-by-fn (fn [g]
                                 (reduce (fn [a m]
                                           (f-and a (f-impl (D m) (inz [g m]))))
                                         1
                                         (attributes context)))
                               (objects context)))))

(defn doubly-scale-fuzzy-context
  "Returns the doubly scaled formal context for the given fuzzy
  context ctx."
  [ctx]
  (let [inz  (incidence ctx),
        objs (set-of [g ny]
                     [g (objects ctx),
                      m (attributes ctx),
                      :let [ny (inz [g m])],
                      :when (not (zero? ny))]),
        atts (set-of [m lambda]
                     [g (objects ctx),
                      m (attributes ctx),
                      :let [lambda (inz [g m])],
                      :when (not (zero? lambda))]),
        inci  (set-of [[g ny] [m lambda]]
                      [[g ny] objs,
                       [m lambda] atts,
                       :when (<= (f-star ny lambda)
                                 (inz [g m]))])]
    (make-context objs atts inci)))

;;; TODO:
;;;  - Compute fuzzy concepts
;;;  - attribute exploration?
;;;  - non-redundant basis of implications
;;;  - glinclosure

nil
