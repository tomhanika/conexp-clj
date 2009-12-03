(ns #^{:doc "Algorithms which work with bits to improve performance."}
  conexp.contrib.algorithms.bitwise
  (:import [java.util BitSet])
  (:use [clojure.contrib.seq-utils :only (indexed)])
  (:use [conexp.fca.contexts :only (objects attributes incidence)]))

;;; Helpers to convert to and from BitSets

(defn- to-bitset
  ""
  [element-vector hashset]
  (let [#^BitSet bs (BitSet. (count element-vector))]
    (doseq [pair (indexed element-vector)
	    :when (contains? hashset (second pair))]
      (.set bs (first pair)))
    bs))

(defn- to-hashset
  ""
  [element-vector #^BitSet bitset]
  (loop [pos (.nextSetBit bitset 0)
	 result #{}]
    (if (= -1 pos)
      result
      (recur (.nextSetBit bitset (inc pos))
	     (conj result (nth element-vector pos))))))

(defn- to-binary-matrix
  ""
  [object-vector attribute-vector incidence-relation]
  (let [incidence-matrix (make-array Byte/TYPE (count object-vector) (count attribute-vector))]
    (doseq [obj-idx (range (count object-vector))
	    att-idx (range (count attribute-vector))]
      (aset incidence-matrix obj-idx att-idx
	    (if (contains? incidence-relation [(nth object-vector obj-idx)
					       (nth attribute-vector att-idx)])
	      (byte 1)
	      (byte 0))))
    incidence-matrix))

(defn- bitwise-object-derivation
  ""
  [incidence-matrix object-count attribute-count #^BitSet bitset]
  (let [#^BitSet derived-attributes (BitSet. attribute-count)]
    (dotimes [att attribute-count]
      (if (loop [obj (dec object-count)]
	    (cond
	      (< obj 0)
	      true
	      (and (.get bitset obj) (== 0 (aget incidence-matrix obj att)))
	      false
	      :else
	      (recur (dec obj))))
	(.set derived-attributes att)))
    derived-attributes))

(defn- bitwise-attribute-derivation
  ""
  [incidence-matrix object-count attribute-count #^BitSet bitset]
  (let [#^BitSet derived-objects (BitSet. object-count)]
    (dotimes [obj object-count]
      (if (loop [att (dec attribute-count)]
	    (cond
	      (< att 0)
	      true
	      (and (.get bitset att) (== 0 (aget incidence-matrix obj att)))
	      false
	      :else
	      (recur (dec att))))
	(.set derived-objects obj)))
    derived-objects))

;;; Concept Calculation

(defmulti concepts
  "Computes concepts with various algorithms, given as first argument."
  (fn [& args] (first args)))

;; NextClosure with BitSets

(defn- lectic-<_i
  ""
  [base-count i #^BitSet A #^BitSet B]
  (and (.get B i)
       (not (.get A i))
       (loop [j (dec i)]
	 (cond
	   (< j 0)
	   true
	   (not (= (.get A j) (.get B j)))
	   false
	   :else
	   (recur (dec j))))))

(defn- oplus
  ""
  [base-count clop #^BitSet A i]
  (let [#^BitSet bs (BitSet. base-count)]
    (.set bs (int i) true)
    (dotimes [j i]
      (.set bs (int j) (.get A j)))
    (clop bs)))

(defn- next-closed-set
  ""
  [base-count clop #^BitSet A]
  (loop [i (dec base-count)]
    (if (== -1 i)
      nil
      (let [A_i (oplus base-count clop A i)]
	(if (lectic-<_i base-count i A A_i)
	  A_i
	  (recur (dec i)))))))

(defmethod concepts :next-closure [_ context]
  (let [object-vector (vec (objects context))
	attribute-vector (vec (attributes context))
	incidence-matrix (to-binary-matrix object-vector attribute-vector (incidence context))

	object-count (count object-vector)
	attribute-count (count attribute-vector)

	attribute-prime (memoize (fn [bitset]
				   (bitwise-attribute-derivation incidence-matrix
								 object-count
								 attribute-count
								 bitset)))
	object-prime (memoize (fn [bitset]
				(bitwise-object-derivation incidence-matrix
							   object-count
							   attribute-count
							   bitset)))
	clop (fn [bitset]
	       (object-prime (attribute-prime bitset)))

	intents (take-while identity
			    (iterate #(next-closed-set object-count clop %) (BitSet.)))]
    (map (fn [bitset]
	   [(to-hashset object-vector (attribute-prime bitset))
	    (to-hashset attribute-vector bitset)])
	 intents)))

;; Krajca-Outrata-Vychodil

;; In-Close

;;;

nil
