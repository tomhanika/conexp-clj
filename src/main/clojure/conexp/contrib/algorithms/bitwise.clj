;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms.bitwise
  (:import [java.util BitSet])
  (:import [java.util.concurrent SynchronousQueue])
  (:use [conexp.fca.contexts :only (objects attributes incidence)]))


;;; Helpers to convert to and from BitSets

(defmacro deep-aget
  "Implements fast, type-hinted aget. From Christophe Grand."
  ([hint array idx]
    `(aget ~(vary-meta array assoc :tag hint) (int ~idx)))
  ([hint array idx & idxs]
    `(let [a# (aget ~(vary-meta array assoc :tag 'objects) (int ~idx))]
       (deep-aget ~hint a# ~@idxs))))

(defmacro forall-in-bitset
  "Returns true iff body holds for all var in bitset."
  [[var bitset] & body]
  `(loop [~var (int (.nextSetBit ~bitset 0))]
     (cond
       (== -1 ~var) true,
       (not ~@body) false,
       :else (recur (.nextSetBit ~bitset (inc ~var))))))

(defmacro dobits
  "Executes body for every bit set in bitset. Var gets bound to the
  current location of the bit. Before every iteration, end-test (if
  given) is tested and the loop is stopped if it returns true."
  [[var bitset & end-test] & body]
  `(let [^BitSet bitset# ~bitset]
     (loop [~var (int (.nextSetBit bitset# 0))]
       (if (or (== -1 ~var) ~@end-test)
         nil
         (do
           ~@body
           (recur (int (.nextSetBit bitset# (inc ~var)))))))))

(defn filter-bitset
  "Returns a new bitset of all bits in bitset for which predicate returns true."
  [predicate, ^BitSet bitset]
  (let [result (BitSet.)]
    (dobits [i bitset]
      (when (predicate i)
        (.set result i)))
    result))

(defn to-bitset
  "Converts hashset to a BitSet. Elements are associated with indices
  as given by element-vector (i.e. the order in this vector is the
  order of the elements in the resulting BitSet."
  [element-vector hashset]
  (let [^BitSet bs (BitSet. (count element-vector))]
    (loop [elements (seq element-vector)
           index    0]
      (if elements
        (let [elt (first elements)]
          (when (contains? hashset elt)
            (.set bs index))
          (recur (next elements) (inc index)))
        bs))))

(defn to-hashset
  "Converts given bitset to a set, where element-vector gives the
  actual objects to be collected, i.e. element v at position p in
  element-vector is included in the result iff p is set in bitset."
  [element-vector, ^BitSet bitset]
  (loop [pos    (int (.nextSetBit bitset 0)),
         result (transient #{})]
    (if (== -1 pos)
      (persistent! result)
      (recur (.nextSetBit bitset (inc pos))
             (conj! result (nth element-vector pos))))))

(defn to-binary-matrix
  "Converts the incidence-relation to a binary matrix (in the sense of
  Java) filled with 1 and 0."
  [object-vector attribute-vector incidence]
  (let [incidence-matrix (make-array Integer/TYPE (count object-vector) (count attribute-vector))]
    (dotimes [obj-idx (count object-vector)]
      (let [^ints row (aget ^objects incidence-matrix obj-idx)]
        (dotimes [att-idx (count attribute-vector)]
          (aset row att-idx
                (if (incidence [(nth object-vector obj-idx)
                                (nth attribute-vector att-idx)])
                  (int 1)
                  (int 0))))))
    incidence-matrix))

(defn to-binary-context
  "Returns [object-vector, attribute-vector, object-count,
  attribute-count, incidence-matrix] of context with the obvious
  definitions."
  [context]
  (let [object-vector    (vec (objects context)),
        attribute-vector (vec (attributes context)),
        object-count     (count object-vector),
        attribute-count  (count attribute-vector),
        incidence-matrix (to-binary-matrix object-vector attribute-vector (incidence context))]
    [object-vector attribute-vector object-count attribute-count incidence-matrix]))

(defmacro with-binary-context
  "For a given context defines object-vector, attribute-vector,
  object-count, attribute-count and incidence-matrix in the obvious
  way."
  [context & body]
  `(let [[~'object-vector ~'attribute-vector ~'object-count ~'attribute-count ~'incidence-matrix]
         (to-binary-context ~context)]
     ~@body))

(defn bitwise-object-derivation
  "Implements object derivation for BitSets."
  [incidence-matrix, object-count, attribute-count, ^BitSet bitset]
  (let [^BitSet derived-attributes (BitSet. attribute-count)]
    (dotimes [att (int attribute-count)]
      (when (forall-in-bitset [obj bitset]
              (== 1 (deep-aget ints incidence-matrix obj att)))
        (.set derived-attributes att)))
    derived-attributes))

(defn bitwise-attribute-derivation
  "Implements attribute derivation for BitSets."
  [incidence-matrix, object-count, attribute-count, ^BitSet bitset]
  (let [^BitSet derived-objects (BitSet. object-count)]
    (dotimes [obj (int object-count)]
      (when (forall-in-bitset [att bitset]
              (== 1 (deep-aget ints incidence-matrix obj att)))
        (.set derived-objects obj)))
    derived-objects))

;;;

nil
