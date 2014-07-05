;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(in-ns 'conexp.contrib.algorithms)

(use 'conexp.contrib.algorithms.bitwise)
(require '[conexp.main :as cm])

(import '[conexp.fca.implications Implication])


;;; Computing Implicational Closures

(defn- add-immediate-elements
  [implications, ^BitSet initial-set, subset-test]
  (loop [^BitSet conclusions  initial-set,
         impls        implications,
         unused-impls (transient [])]
    (if-let [^Implication impl (first impls)]
      (if (subset-test (.premise impl) initial-set)
        (recur (do (dobits [x (.conclusion impl)]
                     (.set conclusions x))
                   conclusions)
               (rest impls)
               unused-impls)
        (recur conclusions
               (rest impls)
               (conj! unused-impls impl)))
      [conclusions, (persistent! unused-impls)])))

(defn close-under-implications
  [implications, ^BitSet set]
  (loop [set   set,
         impls implications]
    (let [[new impls] (add-immediate-elements impls (.clone set)
                                              (fn [^BitSet A, ^BitSet B]
                                                (forall-in-bitset [x A]
                                                  (.get B x))))]
      (if (= new set)
        new
        (recur new impls)))))

(defn clop-by-implications
  [implications]
  (partial close-under-implications implications))


;;; Canonical Base

(defn canonical-base
  ([ctx]
     (canonical-base ctx #{}))
  ([ctx background-knowledge]
     (assert (context? ctx)
             "First argument must be a formal context")
     (with-binary-context ctx 
       (let [next-closure (fn [implications last]
                            (next-closed-set object-count
                                             attribute-count
                                             (clop-by-implications implications)
                                             last)),
             runner       (fn runner [implications, ^BitSet candidate]
                            (when candidate
                              (let [conclusions (bitwise-context-attribute-closure
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
         (map (fn [bit-impl]
                (cm/make-implication (to-hashset attribute-vector (premise bit-impl))
                                     (to-hashset attribute-vector (conclusion bit-impl))))
              (lazy-seq (runner (vec background-knowledge)
                                (close-under-implications background-knowledge
                                                          (to-bitset attribute-vector #{})))))))))

;;;

nil
