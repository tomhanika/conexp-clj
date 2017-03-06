;; Copyright ⓒ the conexp-clj developers; all rights reserved.
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

(defn- lectic-<_i
  "Returns true iff A is lectically smaller than B at position i."
  [^long i, ^BitSet A, ^BitSet B]
  (and (.get B i)
       (not (.get A i))
       (loop [j (long (dec i))]
         (cond
          (< j 0)
          true,
          (not= (.get A j) (.get B j))
          false,
          :else
          (recur (dec j))))))

(defn bitwise-context-attribute-closure
  "Computes the closure of A in the context given by the parameters."
  [^long object-count, ^long attribute-count, incidence-matrix, ^BitSet A]
  (let [^BitSet A (.clone A),
        ^BitSet B (BitSet.)]
    (dotimes [obj object-count]
      (when (forall-in-bitset [att A]
              (== 1 (deep-aget ints incidence-matrix obj att)))
        (.set B obj)))
    (dotimes [att attribute-count]
      (when (and (not (.get A att))
                 (forall-in-bitset [obj B]
                   (== 1 (deep-aget ints incidence-matrix obj att))))
        (.set A att)))
    A))

(defn next-closed-set
  "Computes the next closed set of closure after A. Returns nil if there is none."
  [^long attribute-count, closure, ^BitSet A]
  (let [^BitSet B (.clone A)]
    (loop [i (dec attribute-count)]
      (cond
       (== -1 i)
       nil,
       (.get B i)
       (do (.clear B i)
           (recur (dec i))),
       :else
       (do (.set B i)
           (let [A_i (closure B)]
             (if (lectic-<_i i A A_i)
               A_i
               (do (.clear B i)
                   (recur (dec i))))))))))

;;;

nil
