;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.fuzzy.sets
  "Basic definitions for fuzzy sets"
  (:use conexp.base
        conexp.contrib.fuzzy.logics)
  (:require [clojure.math.combinatorics :as comb]))

;;;

(deftype Fuzzy-Set [^clojure.lang.APersistentMap hashmap]
  ;; Contract: hashmap does not contain elements whose values are
  ;; not-numbers or smaller-equal 0 or greater 1.
  ;;
  Object
  (equals [this other]
    (generic-equals [this other] Fuzzy-Set [hashmap]))
  (hashCode [this]
    (hash-combine-hash Fuzzy-Set hashmap))
  (toString [this]
    (str hashmap))
  ;;
  clojure.lang.ISeq
  (first [this]
    (first hashmap))
  (next [this]
    (next hashmap))
  (more [this]
    (if-let [n (next hashmap)]
      n
      ()))
  (cons [this [k v]]
    (when-not (and (number? v) (<= 0 v 1))
      (illegal-argument "Fuzzy sets only support real values in [0,1]"))
    (if (zero? v)
      (Fuzzy-Set. (dissoc hashmap k))
      (Fuzzy-Set. (assoc hashmap k v))))
  (seq [this]
    (seq hashmap))
  (count [this]
    (count hashmap))
  (empty [this]
    (Fuzzy-Set. {}))
  (equiv [this other]
    (.equals this other))
  ;;
  clojure.lang.IFn
  (invoke [this thing]
    (let [result (hashmap thing)]
      (or result 0)))
  (applyTo [this ^clojure.lang.ISeq seq]
    (if (= 1 (count seq))
      (.applyTo hashmap seq)
      (illegal-argument "Cannot apply fuzzy sets to non-singleton sequences.")))
  ;;
  clojure.lang.Associative
  (containsKey [this o]
    true)
  (entryAt [this o]
    (.entryAt hashmap o))
  (assoc [this k v]
    (.cons this [k v]))
  ;;
  clojure.lang.ILookup
  (valAt [this o]
    (get hashmap o 0))
  (valAt [this o not-found]
    (get hashmap o not-found)))

;;; Constructors

(defmulti make-fuzzy-set
  "Constructs a fuzzy set from a given collection."
  clojure-type)

(defmethod make-fuzzy-set :default
  [thing]
  (illegal-argument "Don't know how to create a fuzzy set from " thing "."))

(defmethod make-fuzzy-set clojure-map
  [hashmap]
  (assert (forall [v (vals hashmap)]
            (and (number? v) (<= 0 v 1))))
  (Fuzzy-Set. (select-keys hashmap (remove #(zero? (hashmap %)) (keys hashmap)))))

(defmethod make-fuzzy-set Fuzzy-Set
  [fuzzy-set]
  fuzzy-set)

(defmethod make-fuzzy-set clojure-set
  [set]
  (Fuzzy-Set. (map-by-fn (constantly 1) set)))

(defmethod make-fuzzy-set clojure-coll
  [coll]
  (make-fuzzy-set (set coll)))

(defmethod make-fuzzy-set clojure-vec
  [vec]
  (make-fuzzy-set (set vec)))

(defalias fuzzy-set make-fuzzy-set
  "Alias for make-fuzzy-set")

(defn fuzzy-set-as-hashmap
  "Returns the hashmap corresponding to the given fuzzy set."
  [fuzzy-set]
  (.hashmap ^Fuzzy-Set fuzzy-set))

(defmethod print-method Fuzzy-Set [set out]
  (.write ^java.io.Writer out
          ^String (str "#F" set)))

(defn fuzzy-set?
  "Tests thing for being a fuzzy set."
  [thing]
  (instance? Fuzzy-Set thing))

;;; Set Operations

(defn- pointwise-fuzzy
  "Returns a fuzzy set where op is done pointwise on the fuzzy sets."
  [op fuzzy-sets]
  ;; slow, make it faster!
  (make-fuzzy-set (map-by-fn (fn [k]
                               (apply op (map #(% k) fuzzy-sets)))
                             (apply union (map (comp set keys fuzzy-set-as-hashmap) fuzzy-sets)))))

(defmacro define-fuzzy-set-operation
  "Defines a fuzzy set operation by applying op pointwise to all its
  arguments. If only one argument is given, it is returned."
  [name docstring op]
  `(defn ~name
     ~docstring
     [& ~'fuzzy-sets]
     (condp = (count ~'fuzzy-sets)
      0 (illegal-argument ~name " needs at least one argument."),
      1 (first ~'fuzzy-sets),
      (pointwise-fuzzy ~op ~'fuzzy-sets))))

(define-fuzzy-set-operation fuzzy-intersection
  "Intersection of fuzzy sets."
  min)

(define-fuzzy-set-operation fuzzy-union
  "Union of fuzzy sets."
  max)

(define-fuzzy-set-operation fuzzy-difference
  "Difference of fuzzy set."
  (fn [first & rest]
    (max 0 (apply - first rest))))

(defn fuzzy-subsets
  "Returns all fuzzy subsets of the given fuzzy set base-set with given fuzzy values."
  [values base-set]
  (let [values     (sort values),
        base-set   (seq (make-fuzzy-set base-set)),
        max-values (vec (map second base-set)),
        crisp-base (map first base-set)]
    (map #(make-fuzzy-set (zipmap crisp-base %))
         (apply comb/cartesian-product
                (for [i (range (count base-set))]
                  (take-while #(<= % (nth max-values i)) values))))))

(defn fuzzy-subset?
  "Returns true iff fuzzy-set-1 is a subset of fuzzy-set-2."
  [fuzzy-set-1 fuzzy-set-2]
  (forall [k (keys (fuzzy-set-as-hashmap fuzzy-set-1))]
    (<= (fuzzy-set-1 k)
        (fuzzy-set-2 k))))

(defn subsethood
  "Returns the degree to which fuzzy-set-1 is a subset of fuzzy-set-2. Applies hedge to the truth
  value of an element being in fuzzy-set-1, if given."
  ([fuzzy-set-1 fuzzy-set-2]
     (subsethood fuzzy-set-1 fuzzy-set-2 identity))
  ([fuzzy-set-1 fuzzy-set-2 hedge]
     (reduce (fn [result element]
               (f-and result (f-impl (hedge (fuzzy-set-1 element))
                                     (fuzzy-set-2 element))))
             1
             (keys (fuzzy-set-as-hashmap fuzzy-set-1)))))

;;;

nil
