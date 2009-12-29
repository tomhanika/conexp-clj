(ns conexp.contrib.algorithms.bitwise
  (:import [java.util BitSet])
  (:import [java.util.concurrent SynchronousQueue])
  (:use [clojure.contrib.seq-utils :only (indexed)])
  (:use [conexp.fca.contexts :only (objects attributes incidence)]))


(set! *warn-on-reflection* true)

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
       (== -1 ~var)
       true,
       (not ~@body)
       false,
       :else
       (recur (.nextSetBit ~bitset (inc ~var))))))

(defmacro dobits
  ""
  [[var bitset & end-test] & body]
  `(let [#^BitSet bitset# ~bitset]
     (loop [~var (int (.nextSetBit bitset# 0))]
       (if (or (== -1 ~var)
	       ~@end-test)
	 nil
	 (do
	   ~@body
	   (recur (int (.nextSetBit bitset# (inc ~var)))))))))

(defn filter-bitset
  "Returns a new bitset of all bits in bitset for which predicate returns true."
  [predicate bitset]
  (let [result (BitSet.)]
    (dobits [i bitset]
      (when (predicate i)
	(.set result i)))
    result))

(defn to-bitset
  ""
  [element-vector hashset]
  (let [#^BitSet bs (BitSet. (count element-vector))]
    (doseq [pair (indexed element-vector)
	    :when (contains? hashset (second pair))]
      (.set bs (first pair)))
    bs))

(defn to-hashset
  ""
  [element-vector #^BitSet bitset]
  (loop [pos (int (.nextSetBit bitset 0))
	 result #{}]
    (if (== -1 pos)
      result
      (recur (.nextSetBit bitset (inc pos))
	     (conj result (nth element-vector pos))))))

(defn to-binary-matrix
  ""
  [object-vector attribute-vector incidence-relation]
  (let [incidence-matrix (make-array Integer/TYPE (count object-vector) (count attribute-vector))]
    (doseq [obj-idx (range (count object-vector))
	    att-idx (range (count attribute-vector))]
      (aset incidence-matrix obj-idx att-idx
	    (if (contains? incidence-relation [(nth object-vector obj-idx)
					       (nth attribute-vector att-idx)])
	      (int 1)
	      (int 0))))
    incidence-matrix))

(defn to-binary-context
  ""
  [context]
  (let [object-vector (vec (objects context))
	attribute-vector (vec (attributes context))

	object-count (count object-vector)
	attribute-count (count attribute-vector)

	incidence-matrix (to-binary-matrix object-vector attribute-vector (incidence context))]
    [object-vector attribute-vector,
     object-count  attribute-count,
     incidence-matrix]))

(defn bitwise-object-derivation
  ""
  [incidence-matrix object-count attribute-count #^BitSet bitset]
  (let [#^BitSet derived-attributes (BitSet. attribute-count)]
    (dotimes [att (int attribute-count)]
      (if (forall-in-bitset [obj bitset]
	    (== 1 (deep-aget ints incidence-matrix obj att)))
	(.set derived-attributes att)))
    derived-attributes))

(defn bitwise-attribute-derivation
  ""
  [incidence-matrix object-count attribute-count #^BitSet bitset]
  (let [#^BitSet derived-objects (BitSet. object-count)]
    (dotimes [obj (int object-count)]
      (if (forall-in-bitset [att bitset]
	    (== 1 (deep-aget ints incidence-matrix obj att)))
	(.set derived-objects obj)))
    derived-objects))

nil
