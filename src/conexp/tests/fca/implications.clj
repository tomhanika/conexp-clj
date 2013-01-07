;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.tests.fca.implications
  (:use clojure.test)
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications)
  (:require [conexp.tests.fca.contexts :as contexts]))

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

(defvar testing-data
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
  (are [start impls result] (= result (close-under-implications (map (partial apply make-implication) impls) start))
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

(deftest test-stem-base
  (is (= 1 (count (stem-base (one-context #{1 2 3 4 5})))))
  (are [ctx] (let [sb (stem-base ctx)]
               (is (minimal-implication-set? sb))
               (is (sound-implication-set? ctx sb))
               (is (complete-implication-set? ctx sb)))
    contexts/test-ctx-01,
    contexts/test-ctx-04
    contexts/test-ctx-07,
    contexts/test-ctx-08))

(deftest test-pseudo-intents
  (with-testing-data [ctx (random-contexts 10 20)]
    (= (set (pseudo-intents ctx))
       (set (map premise (stem-base ctx)))))
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
    (is (forall [P (subsets (attributes ctx))]
          (<=> (proper-premise? ctx P)
               (not (empty? (proper-conclusion ctx P))))))))

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

(deftest test-stem-base-from-base
  (with-testing-data [ctx (random-contexts 10 20)]
    (= (stem-base ctx)
       (stem-base-from-base (proper-premise-implications ctx))))
  (= (stem-base-from-base [(make-implication #{1} #{2 3})
                           (make-implication #{1} #{2 4})])
     #{(make-implication #{1} #{2 3 4})}))

;;;

(defvar- ctx-1 (make-context #{0 1 2 3 4 5 6 7 8 9}
                             #{0 1 2 3 4 5 6 7 8 9}
                             #{[6 5] [1 0] [4 4] [9 9] [0 0] [1 1] [3 4] [6 7]
                               [8 9] [1 2] [3 5] [6 8] [0 2] [3 6] [5 8] [0 3]
                               [1 4] [0 4] [3 8] [0 5] [3 9] [2 9] [9 0] [7 0]
                               [8 1] [7 1] [9 3] [7 2] [7 3] [9 5] [6 3] [3 1]
                               [6 4] [8 6] [2 0]}))

(defvar- ar-testing-data [ctx-1
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

(deftest test-ryssel-base
  (with-testing-data [ctx (random-contexts 10 15)]
    (let [base (ryssel-base ctx)]
      (and (sound-implication-set? ctx base)
           (complete-implication-set? ctx base)))))

;;;

nil
