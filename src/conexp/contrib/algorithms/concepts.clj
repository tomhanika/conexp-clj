;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms.concepts
  (:use conexp.contrib.algorithms.bitwise
        conexp.contrib.algorithms.generators
        [conexp.contrib.algorithms.next-closure :only (next-closed-set)])
  (:use [conexp.fca.contexts :only (objects attributes incidence)])
  (:import [java.util BitSet Vector])
  (:import [java.util.concurrent SynchronousQueue]))

(ns-doc
 "Implements various algorithms to compute the concepts of a given
 context efficiently.")

(set! *warn-on-reflection* true)

;;; Concept Calculation Multi-Method

(defmulti concepts
  "Computes concepts with various algorithms, given as first
  argument. Default is :next-closure."
  {:arglists '([algorithm context] [context])}
  (fn [& args]
    (cond
     (= 1 (count args)) ::default-concepts
     (= 2 (count args)) (first args)
     :else (illegal-argument "Wrong number of args passed to c.c.algorithms.concepts."))))

(defmethod concepts ::default-concepts [context]
  (concepts :next-closure context))


;; NextClosure (:next-closure)

(defmethod concepts :next-closure
  [_ context]
  (with-binary-context context
    (let [o-prime (partial bitwise-object-derivation incidence-matrix object-count attribute-count),
          a-prime (partial bitwise-attribute-derivation incidence-matrix object-count attribute-count),
          start   (o-prime (a-prime (BitSet.))),
          intents (take-while identity
                              (iterate #(next-closed-set object-count
                                                         attribute-count
                                                         incidence-matrix
                                                         %)
                                       start))]
      (map (fn [bitset]
             [(to-hashset object-vector (a-prime bitset)),
              (to-hashset attribute-vector bitset)])
           intents))))


;; Vychodil (:vychodil)

(defn- compute-closure
  [object-count, attribute-count, incidence-matrix, rows, ^BitSet A, ^BitSet B, y]
  (let [^BitSet C (BitSet.),
	^BitSet D (BitSet.),
	^BitSet E (.clone A),
	y (int y)]
    (.set D 0 (int attribute-count))
    (.and E (aget ^objects rows y))
    (dobits [i E]
      (.set C i)
      (dotimes [j attribute-count]
	(if (== 0 (deep-aget ints incidence-matrix i j))
	  (.set D j false))))
    [C, D]))

(defg generate-from
  [object-count, attribute-count, incidence-matrix, rows, ^BitSet A, ^BitSet B, y]
  (yield [A, B])
  (when (and (not (== attribute-count (.cardinality B)))
             (< (int y) (int attribute-count)))
    (doseq [j (range (int y) (int attribute-count))
	    :when (not (.get B j))
	    :let [[^BitSet C, ^BitSet D] (compute-closure object-count attribute-count,
                                                          incidence-matrix rows,
                                                          A B j)
		  skip (loop [k (int 0)]
			 (cond
			   (== k j)
			   false,
			   (not= (.get D k) (.get B k))
			   true,
			   :else
			   (recur (inc k))))]
	    :when (not skip)]
      (generate-from object-count attribute-count,
		     incidence-matrix rows,
		     C D (inc j)))))

(alter-meta! (var generate-from) assoc :private true)

(defmethod concepts :vychodil
  [_ context]
  (with-binary-context context
    (let [rows          (into-array (map (fn [y]
                                           (let [^BitSet bs (BitSet.)]
                                             (.set bs y)
                                             (bitwise-attribute-derivation incidence-matrix
                                                                           object-count
                                                                           attribute-count
                                                                           bs)))
                                         (range attribute-count)))
          empty-down    (bitwise-attribute-derivation incidence-matrix
                                                      object-count
                                                      attribute-count
                                                      (BitSet.))
          empty-down-up (bitwise-object-derivation incidence-matrix
                                                   object-count
                                                   attribute-count
                                                   empty-down)]
      (map (fn [pair]
             [(to-hashset object-vector (first pair))
              (to-hashset attribute-vector (second pair))])
           (generate (generate-from object-count
                                    attribute-count
                                    incidence-matrix
                                    rows
                                    empty-down
                                    empty-down-up
                                    0))))))


;; In-Close (:in-close)

;; TODO: Check this, it's too slow.

(defn- cannonical?
  "Implements the IsCannonical method of In-Close."
  [incidence-matrix, ^BitSet A-new, ^BitSet B, y]
  (loop [j (dec (int y))]
    (cond
     (< j 0) true,
     (.get B j) (recur (dec j)),
     (forall-in-bitset [i A-new]
       (== 1 (deep-aget ints incidence-matrix i j)))
     false,
     :else (recur (dec j)))))

(defn- in-close
  "Implements the InClose method of In-Close."
  [attribute-count, incidence-matrix, ^Vector As, ^Vector Bs, last, current, y]
  (swap! last + 1)
  (.add As @last (BitSet.))
  (loop [j (int y)]
    (when (< j attribute-count)
      (.clear ^BitSet (.get As @last))
      (dobits [i (.get As current)]
	(when (== 1 (deep-aget ints incidence-matrix i j))
	  (.set ^BitSet (.get As @last) i)))
      (if (== (.cardinality ^BitSet (.get As current))
	      (.cardinality ^BitSet (.get As @last)))
	(.set ^BitSet (.get Bs current) j)
	(when (cannonical? incidence-matrix (.get As @last) (.get Bs current) j)
	  (.add Bs @last (.clone ^BitSet (.get Bs current)))
	  (.set ^BitSet (.get Bs @last) j)
	  (in-close attribute-count incidence-matrix As Bs last @last (+ j 1))))
      (recur (inc j)))))

(defmethod concepts :in-close
  [_ context]
  (with-binary-context context
    (let [^Vector As (Vector.),
          ^Vector Bs (Vector.),
          last (atom 0)]
      (.add As 0 (BitSet.))
      (.set ^BitSet (.get As 0) 0 (int object-count))
      (.add Bs 0 (BitSet.))
      (in-close attribute-count incidence-matrix As Bs last 0 0)
      (map (fn [A B]
             [(to-hashset object-vector A),
              (to-hashset attribute-vector B)])
           As Bs))))

;;;

(set! *warn-on-reflection* false)

;;;

nil
