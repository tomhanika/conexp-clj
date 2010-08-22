;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms.next-closure
  (:use conexp.contrib.algorithms.bitwise)
  (:import [java.util BitSet]))

;;;

;; TODO: Sort attributes before starting (see naïve implementation of next closure)

(defn- lectic-<_i
  ""
  [i, ^BitSet A, ^BitSet B]
  (let [i (int i)]
    (and (.get B i)
	 (not (.get A i))
	 (loop [j (int (dec i))]
	   (cond
	     (< j 0)
	     true,
	     (not= (.get A j) (.get B j))
	     false,
	     :else
	     (recur (dec j)))))))

(defn- oplus
  ""
  [object-count, attribute-count, incidence-matrix, ^BitSet A, i]
  (let [^BitSet A-short (.clone A),
	^BitSet B (BitSet.),
	i (int i)]
    (.set A-short i true)
    (.set A-short (inc i) (int attribute-count) false)
    (dotimes [obj object-count]
      (if (forall-in-bitset [att A-short]
	    (== 1 (deep-aget ints incidence-matrix obj att)))
	(.set B obj)))
    (dotimes [att attribute-count]
      (if (and (not (.get A-short att))
	       (forall-in-bitset [obj B]
		 (== 1 (deep-aget ints incidence-matrix obj att))))
	(.set A-short att)))
    A-short))

(defn next-closed-set
  ""
  [object-count, attribute-count, incidence-matrix, ^BitSet A]
  (loop [i (dec (int attribute-count))]
    (cond
      (== -1 i)
      nil,
      (.get A i)
      (recur (dec i)),
      :else
      (let [A_i (oplus object-count attribute-count incidence-matrix A i)]
	(if (lectic-<_i i A A_i)
	  A_i
	  (recur (dec i)))))))

;;;

nil
