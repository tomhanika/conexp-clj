;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(in-ns 'conexp.contrib.algorithms)

(use 'conexp.contrib.algorithms.bitwise
     '[conexp.fca.implications :only (make-implication)])
(require '[conexp.contrib.algorithms.next-closure :as nc])

(import '[java.util BitSet List LinkedList ListIterator])
(import '[conexp.fca.implications Implication])


;;; Computing Implicational Closures

(defn- add-immediate-elements
  "Destructs implications and input-bitset"
  [^List implications, ^BitSet input-set, subset-test]
  (let [^ListIterator iter (.listIterator implications)]
    (loop [changed? false]
      (if-not (.hasNext iter)
        [input-set, implications, changed?]
        (let [^Implication impl (.next iter)]
          (if (subset-test (.premise impl) input-set)
            (do (dobits [x (.conclusion impl)]
                  (.set input-set x))
                (.remove iter)
                (recur true))
            (recur changed?)))))))

(defn close-under-implications
  "Yields new bitset that is the closure of the input bitset under the given collection of
  implications of bitsetsn"
  [implications, ^BitSet set]
  (loop [set   (.clone set),
         impls (LinkedList. implications)]
    (let [[new impls changed?] (add-immediate-elements impls
                                                       set
                                                       (fn [^BitSet A, ^BitSet B]
                                                         (forall-in-bitset [x A]
                                                           (.get B x))))]
      (if changed?
        (recur new impls)
        new))))

(defn clop-by-implications
  [implications]
  (partial close-under-implications implications))


;;; Canonical Base

(defn canonical-base
  ([ctx]
   (canonical-base ctx #{}))
  ([ctx background-knowledge]
   (with-binary-context ctx
     (let [bg-knowledge (map (fn [^Implication impl]
                               (Implication. (to-bitset attribute-vector (.premise impl))
                                             (to-bitset attribute-vector (.conclusion impl))))
                             background-knowledge),
           next-closure (fn [implications last]
                          (nc/next-closed-set attribute-count
                                              (clop-by-implications implications)
                                              last)),
           runner       (fn runner [implications, ^BitSet candidate]
                          (when candidate
                            (let [conclusions (nc/bitwise-context-attribute-closure
                                               object-count
                                               attribute-count
                                               incidence-matrix
                                               candidate)]
                              (if (not= candidate conclusions)
                                (let [impl  (Implication. candidate
                                                          (filter-bitset #(not (.get candidate %))
                                                                         conclusions)),
                                      impls (conj implications impl)]
                                  (cons impl
                                        (lazy-seq (runner impls (next-closure impls candidate)))))
                                (recur implications (next-closure implications candidate))))))]
       (map (fn [^Implication bit-impl]
              (make-implication (to-hashset attribute-vector (.premise bit-impl))
                                (to-hashset attribute-vector (.conclusion bit-impl))))
            (lazy-seq (runner (vec bg-knowledge)
                              (close-under-implications bg-knowledge
                                                        (to-bitset attribute-vector #{})))))))))

;;;

nil
