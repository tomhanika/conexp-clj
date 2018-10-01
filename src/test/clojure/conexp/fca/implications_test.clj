;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.implications-test
  (:use clojure.test)
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications)
  (:require [conexp.fca.contexts-test :as contexts]))

;;;

(deftest test-make-implication
  (is (make-implication #{} #{}))
  (is (make-implication [] []))
  (is (make-implication () ()))
  (is (make-implication [1 2 3] '[a b c])))

(deftest test-Implication-equals
  (is (= (make-implication [] []) (make-implication [] [])))
  (is (= (make-implication [1] [2]) (make-implication [1] [1 2])))
  (is (= (make-implication '[a] '[b c d]) (make-implication '#{a} '#{b c d})))
  (is (not= (make-implication '[a] '[b c d]) (make-implication '[b] '[a c d]))))

;;;

(def testing-data
  [(make-implication [] []),
   (make-implication [1 2 3] '[a b c]),
   (make-implication [1 2 3] '[3 a b c])
   (make-implication #{1 2 3 4 5} #{3 4 5 6 7})])

(deftest test-Implication-hashCode
  (with-testing-data [impl-1 testing-data,
                      impl-2 testing-data]
    (=> (= impl-1 impl-2)
        (= (hash impl-1) (hash impl-2)))))

(deftest test-premise-conclusion
  (are [p c pre clc] (let [impl (make-implication p c)]
                       (and (= pre (premise impl))
                            (= clc (conclusion impl))))
       #{1 2 3} #{3 4 5} #{1 2 3} #{4 5},
       #{} #{} #{} #{},
       #{1 2 3} #{1 2 3} #{1 2 3} #{},
       #{nil} #{true} #{nil} #{true}))

(deftest test-implication?
  (is (implication? (make-implication #{1} #{2})))
  (is (not (implication? 1))))

;;;

(deftest test-respects?
  (is (respects? #{1 2 3} (make-implication #{1} #{2})))
  (is (not (respects? #{1 2 3} (make-implication #{1} #{4}))))
  (is (respects? #{} (make-implication '#{some} '#{thing nil}))))

(deftest test-holds?
  (is (holds? (make-implication #{1} #{2})
              (make-context [1 2] [1 2] <=)))
  (is (not (holds? (make-implication #{1} #{2})
                   (make-context [1 2] [1 2] >=)))))

(deftest test-close-under-implications
  (are [start impls result] (= result
                               (close-under-implications (map (partial apply make-implication)
                                                              impls)
                                                         start))
       #{} [[#{} #{1}]] #{1},
       #{1} [[#{1} #{2}] [#{2} #{3}] [#{3} #{4}] [#{4} #{5}]] #{1 2 3 4 5},
       #{1 2 3} [[#{1 2} #{4}] [#{1 4} #{5}] [#{1 6} #{7}]] #{1 2 3 4 5}
       #{1 2 3 4 5 6 7 8} [[#{1} #{4}] [#{2} #{3}] [#{4} #{5 6 7}] [#{6 7} #{8}] [#{9 10 11} #{8}]] #{1 2 3 4 5 6 7 8}))

(deftest test-clop-by-implications
  (let [clop (clop-by-implications #{(make-implication #{1} #{2}),
                                     (make-implication #{3} #{4}),
                                     (make-implication #{} #{1})})]
    (is (= #{1 2} (clop #{1}) (clop #{2}) (clop #{})))
    (is (= #{1 2 3 4} (clop #{3})))
    (is (= #{1 2 4} (clop #{4})))
    (is (= #{#{1 2} #{1 2 4} #{1 2 3 4}} (set (all-closed-sets [1 2 3 4] clop))))
    (is (not= #{1 2 3 4} (clop #{1 2 4})))))

(deftest test-pseudo-close-under-implications
  (are [start impls result] (= result
                               (pseudo-close-under-implications
                                (map (partial apply make-implication) impls)
                                start))
       #{} [[#{} #{1}]] #{},
       #{1} [[#{1} #{2}] [#{2} #{3}] [#{3} #{4}] [#{4} #{5}]] #{1},
       #{1 2 3} [[#{1 2} #{4}] [#{1 4} #{5}] [#{1 6} #{7}]] #{1 2 3 4 5}))

(deftest test-pseudo-clop-by-implications
  (let [pclop (pseudo-clop-by-implications #{(make-implication #{1} #{2}),
                                            (make-implication #{3} #{4}),
                                            (make-implication #{} #{1})})]
    (is (= #{#{} #{1} #{1 2} #{1 2 4} #{1 2 3 4}} (set (all-closed-sets [1 2 3 4] pclop))))
    (is (= #{1 2 4} (pclop #{1 2 4})))
    (is (= #{} (pclop #{})))
    (is (= #{1 2 3 4} (pclop #{1 3})))
    (is (= #{1 2} (pclop #{2})))
    (is (not= #{1 2} (pclop #{1})))))

(deftest test-follows-semantically?
  (let [impls #{(make-implication #{1} #{2})
                (make-implication #{2} #{4})
                (make-implication #{} #{5})}]
    (is (follows-semantically? (make-implication #{1 2} #{4 5}) impls))
    (is (not (follows-semantically? (make-implication #{1} #{3}) impls)))
    (is (follows-semantically? (make-implication #{1} #{4}) impls))
    (is (follows-semantically? (make-implication #{} #{5}) impls))))

(deftest test-minimal-implication-set?
  (is (minimal-implication-set? #{(make-implication #{1 2} #{3}),
                                  (make-implication #{1 3} #{2})}))
  (is (not (minimal-implication-set? #{(make-implication #{1 2} #{3}),
                                       (make-implication #{1 3} #{2}),
                                       (make-implication #{1} #{2})}))))

(deftest test-sound-implication-set?
  (is (sound-implication-set? (make-context-from-matrix 3 3 [0 1 0
                                                             0 0 1
                                                             1 0 1])
                              #{(make-implication #{0} #{2}),
                                (make-implication #{0 1} #{2})}))
  (is (not (sound-implication-set? (make-context [0 1 2] [0 1 2] =)
                                   #{(make-implication #{0} #{1})}))))

(deftest test-complete-implication-set?
  (is (complete-implication-set? (diag-context [0 1])
                                 #{}))
  (is (complete-implication-set? (diag-context [0 1 2])
                                 #{(make-implication #{0 1} #{2}),
                                   (make-implication #{1 2} #{0}),
                                   (make-implication #{2 0} #{1})}))
  (is (not (complete-implication-set? (diag-context [0 1 2])
                                      #{(make-implication #{0 1} #{2}),
                                        (make-implication #{1 2} #{0}),
                                        (make-implication #{2 0} #{3})})))
  (is (complete-implication-set? (diag-context [0 1 2])
                                 #{(make-implication #{} #{0 1 2})})))

(deftest test-equivalent-implications?
  (is (equivalent-implications? #{(make-implication #{1} #{2}),
                                  (make-implication #{1} #{3})}
                                #{(make-implication #{1} #{2 3})}))
  (is (not (equivalent-implications? #{(make-implication #{1} #{2})}
                                     #{(make-implication #{2} #{1})}))))

(deftest test-irredundant-subset
  (is (= (irredundant-subset #{(make-implication #{1} #{2})
                                               (make-implication #{2} #{3})
                                               (make-implication #{1} #{3})})
         #{(make-implication #{1} #{2})
           (make-implication #{2} #{3})}))
  (let [ctx         (make-context-from-matrix [0 1 2 3]
                                              [0 1 2 3]
                                              [1 0 0 1
                                               1 1 1 0
                                               0 1 1 0
                                               0 0 1 1]),
        intent-base (set-of (make-implication A (adprime ctx A)) | A (subsets #{0 1 2 3})),
        irr-subset  (irredundant-subset intent-base)]
    (is (minimal-implication-set? irr-subset))
    (is (sound-implication-set? ctx irr-subset))
    (is (complete-implication-set? ctx irr-subset))))

(def canonical-base-test-contexts [contexts/test-ctx-01,
                                   contexts/test-ctx-04
                                   contexts/test-ctx-07,
                                   contexts/test-ctx-08])

(deftest test-canonical-base
  (is (= 1 (count (canonical-base (one-context #{1 2 3 4 5})))))
  (doseq [ctx canonical-base-test-contexts]
    (let [sb (canonical-base ctx)]
      (is (minimal-implication-set? sb))
      (is (sound-implication-set? ctx sb))
      (is (complete-implication-set? ctx sb))
      (is (let [premises (map premise sb)]
            (forall [X premises]
              (forall [Y premises]
                (=> (proper-subset? Y X)
                    (proper-subset? (adprime ctx Y) X)))))))))

(deftest test-canonical-base-with-background-knowledge
  ;; make sure implications with empty premise are handled correctly
  (is (let [ctx (make-context-from-matrix 5 5
                                          [0 0 1 0 1
                                           0 1 1 0 0
                                           0 0 1 0 1
                                           0 0 1 1 1
                                           0 0 1 0 1]),
            bgk #{(impl ==> 2)}]
        (= (set (canonical-base ctx bgk))
           #{(impl 0 2 ==> 1 3 4)
             (impl 2 3 ==> 4)
             (impl 1 2 4 ==> 0 3)}))))

(deftest test-canonical-base-with-minimal-support-constraints
  (with-testing-data [ctx canonical-base-test-contexts
                      mis [0 1/5 1/3 1/2 4/5 1]]
    (= (set (filter #(<= mis (support % ctx))
                    (canonical-base ctx)))
       (set (canonical-base ctx #{} #(<= mis (support % ctx)))))))

(deftest test-pseudo-intents
  (with-testing-data [ctx (random-contexts 10 20)]
    (= (set (pseudo-intents ctx))
       (set (map premise (canonical-base ctx)))))
  (is (empty? (pseudo-intents (adiag-context [0 1 2 3]))))
  (is (empty? (pseudo-intents (adiag-context [nil true adiag-context])))))

(deftest test-proper-conclusion
  (let [diag (diag-context [0 1 2 3 4])]
    (is (forall [x (attributes diag)]
          (empty? (proper-conclusion diag #{x})))))
  (let [ctx (make-context-from-matrix 3 2 [1 1 1 1 0 0])]
    (is (= #{0} (proper-conclusion ctx #{1}))))
  (let [ctx (make-context-from-matrix 2 2 [1 1 1 1])]
    (is (= #{} (proper-conclusion ctx #{1})))))

(deftest test-proper-premise?
  (with-testing-data [ctx [contexts/test-ctx-01,
                           contexts/test-ctx-04,
                           contexts/test-ctx-07,
                           contexts/test-ctx-08]]
    (forall [P (subsets (attributes ctx))]
      (<=> (proper-premise? ctx P)
           (not (empty? (proper-conclusion ctx P)))))))

(deftest test-proper-premises-for-attributes
  (with-testing-data [ctx (random-contexts 10 20),
                      m   (attributes ctx),
                      P   (proper-premises-for-attribute ctx m)]
    (and (proper-premise? ctx P)
         (not (contains? P m))
         (contains? (adprime ctx P) m))))

(deftest test-proper-premises
  (are [ctx] (and (is (= (set (proper-premises ctx))
                         (set-of A | A (subsets (attributes ctx))
                                     :when (proper-premise? ctx A))))
                  (let [pp-impls (proper-premise-implications ctx)]
                    (is (sound-implication-set? ctx pp-impls))
                    (is (complete-implication-set? ctx pp-impls))
                    (is (forall [impl pp-impls]
                          (not (empty? (conclusion impl)))))))
    contexts/test-ctx-01,
    contexts/test-ctx-04,
    contexts/test-ctx-07,
    contexts/test-ctx-08
    (make-context #{1 2 3 4 5} #{1 2 3 4 5 6 7 8}
                  #{[1 1] [1 3] [1 6] [1 7] [2 1]
                    [2 4] [2 6] [2 8] [3 1] [3 4]
                    [3 5] [3 8] [4 1] [4 3] [4 5]
                    [4 8] [5 2] [5 3] [5 5] [5 7]})))

(deftest test-canonical-base-from-base
  (with-testing-data [ctx (random-contexts 10 20)]
    (= (set (canonical-base ctx))
       (canonical-base-from-base (proper-premise-implications ctx))))
  (= (canonical-base-from-base [(make-implication #{1} #{2 3})
                           (make-implication #{1} #{2 4})])
     #{(make-implication #{1} #{2 3 4})}))

(deftest test-canonical-base-from-clop
  (with-testing-data [ctx canonical-base-test-contexts]
    (and (= (set (canonical-base ctx))
            (set (canonical-base-from-clop #(adprime ctx %) (attributes ctx)))))))

(deftest test-parallel-canonical-base-from-clop
  (with-testing-data [ctx canonical-base-test-contexts]
    (and (= (set (canonical-base ctx))
            (set (parallel-canonical-base-from-clop #(adprime ctx %)
                                                    (attributes ctx)))))))

(deftest test-parallel-canonical-base
  (with-var-bindings [canonical-base parallel-canonical-base]
    (test-canonical-base)
    (test-canonical-base-with-background-knowledge)))

(deftest test-intersect-implicational-theories
  (with-testing-data [ctx canonical-base-test-contexts,
                      n   (range 1 (count (objects ctx)))]
    (= (set (canonical-base ctx))
       (set (apply intersect-implicational-theories (attributes ctx)
                   (map #(set (canonical-base (make-context % (attributes ctx) (incidence ctx))))
                        (partition-all n (objects ctx))))))))

;;;

(def- ctx-1 (make-context #{0 1 2 3 4 5 6 7 8 9}
                          #{0 1 2 3 4 5 6 7 8 9}
                          #{[6 5] [1 0] [4 4] [9 9] [0 0] [1 1] [3 4] [6 7]
                            [8 9] [1 2] [3 5] [6 8] [0 2] [3 6] [5 8] [0 3]
                            [1 4] [0 4] [3 8] [0 5] [3 9] [2 9] [9 0] [7 0]
                            [8 1] [7 1] [9 3] [7 2] [7 3] [9 5] [6 3] [3 1]
                            [6 4] [8 6] [2 0]}))

(def- ar-testing-data [ctx-1
                       contexts/empty-context
                       contexts/test-ctx-01
                       contexts/test-ctx-04
                       contexts/test-ctx-07
                       contexts/test-ctx-08])

(deftest test-support
  (is (= 1 (support #{} contexts/empty-context)))
  (with-testing-data [ctx ar-testing-data]
    (let [oc (count (objects ctx))]
      (forall [x (subsets (attributes ctx))]
        (= (* oc (support x ctx))
           (count (attribute-derivation ctx x)))))))

(deftest test-confidence
  (is (= 1 (confidence (make-implication #{} #{})
                       contexts/empty-context))))

(deftest test-frequent-closed-itemsets
  (is (= 1 (count (frequent-closed-itemsets (one-context [1 2 3 4]) 0.0))))
  (with-testing-data [ctx [contexts/test-ctx-01
                           contexts/test-ctx-02
                           contexts/test-ctx-04
                           contexts/test-ctx-07
                           contexts/test-ctx-08],
                      spp [0 1/3 1/2 3/5 7/8 1]]
    (= (set-of C [C (intents ctx) :when (>= (support C ctx) spp)])
       (set (frequent-closed-itemsets ctx spp)))))

(deftest test-luxenburger-basis
  (with-testing-data [ctx [ctx-1
                           contexts/test-ctx-01
                           contexts/test-ctx-02
                           contexts/test-ctx-04
                           contexts/test-ctx-07
                           contexts/test-ctx-08]
                      spp [0 1/2 9/10 1]
                      cnf [0 1/2 9/10 1]]
    (let [impls (luxenburger-basis ctx spp cnf)]
      (forall [impl impls]
        (and (<= spp (support impl ctx))
             (<= cnf (confidence impl ctx))))))
  (are [n ctx s c] (= n (count (luxenburger-basis ctx s c)))
       58 ctx-1 0 0,
        2 ctx-1 1/3 1/2,
       26 ctx-1 1/5 1/2,
        0 contexts/test-ctx-04 1/5 1/2,
        1 contexts/test-ctx-01 1/5 1/2))

(deftest test-luxenburger-basis-with-predicate
  (with-testing-data [ctx [ctx-1
                           contexts/test-ctx-01
                           contexts/test-ctx-02
                           contexts/test-ctx-04
                           contexts/test-ctx-07
                           contexts/test-ctx-08]
                      cnt [0 1 2 3 4 5]
                      cnf [0 1/2 9/10 1]]
    (let [impls (luxenburger-basis ctx #(<= (count %) cnt) cnf)]
      (forall [impl impls]
        (and (<= (count (premise impl)) cnt)
             (<= cnf (confidence impl ctx))))))

  (with-testing-data [ctx [ctx-1
                           contexts/test-ctx-01
                           contexts/test-ctx-02
                           contexts/test-ctx-04
                           contexts/test-ctx-07
                           contexts/test-ctx-08]
                      cnt (range (inc (count (objects ctx))))]
    (let [impls (luxenburger-basis ctx #(<= (count %) cnt) 0)
          intes (set (filter #(<= (count %) cnt) (intents ctx)))
          comps (union (set (map premise impls))
                       (set (map #(union (premise %) (conclusion %)) impls)))]
      (and (subset? comps intes)
           (or (= comps intes)
               (and (singleton? intes)
                    (empty? comps)))))))

(deftest test-ryssel-base
  (with-testing-data [ctx (random-contexts 10 15)]
    (let [base (ryssel-base ctx)]
      (and (sound-implication-set? ctx base)
           (complete-implication-set? ctx base)))))

;;;

(deftest test-learn-implications-by-queries
  ;; example Angluin, Frazier, Pitt: Learning Conjunctions of Horn Clauses
  (let [background-hypothesis #{(make-implication '#{a c} '#{d}) (make-implication '#{a b} '#{c})}]
    (is (= (set
            (learn-implications-by-queries '#{a b c d}
                                           (membership-oracle-by-implications background-hypothesis)
                                           (equivalence-oracle-by-implications background-hypothesis)))
           #{(make-implication '#{a b} '#{c d})
             (make-implication '#{a c} '#{d})})))
  (with-testing-data [ctx (random-contexts 10 15)]
    (let [base (canonical-base ctx)]
      (equivalent-implications?
       base
       (learn-implications-by-queries (attributes ctx)
                                      (membership-oracle-by-implications base)
                                      (equivalence-oracle-by-implications base))))))
;;;

(deftest test-approx-canonical-base
  (let [test-bound (fn [ctx ε δ]
                     (let [exact-base (canonical-base ctx)
                           approx-base (approx-canonical-base ctx ε δ)
                           exact-models (set (all-closed-sets (attributes ctx)
                                                              (clop-by-implications exact-base)))
                           approx-models (set (all-closed-sets (attributes ctx)
                                                               (clop-by-implications approx-base)))]
                       (<= (+ (count (difference exact-models approx-models))
                              (count (difference approx-models exact-models)))
                           (* ε (expt 2 (count (attributes ctx)))))))]
    (let [ε 0.3,
          δ 1e-10]
      (with-testing-data [ctx (random-contexts 10 15)]
        (test-bound ctx ε δ)))
    (let [ε 0.3,
          δ 0.5
          n 50]
      (let [failures (filter (comp not #(test-bound % ε δ)) (random-contexts n 15))]
        (< (count failures)
           (* n (- 1 δ)))))))

;;;

nil
