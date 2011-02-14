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

;; add-immediate-elements (private) not considered

(deftest test-close-under-implications
  (are [start impls result] (= result (close-under-implications (map (partial apply make-implication) impls) start))
       #{} [[#{} #{1}]] #{1},
       #{1} [[#{1} #{2}] [#{2} #{3}] [#{3} #{4}] [#{4} #{5}]] #{1 2 3 4 5},
       #{1 2 3} [[#{1 2} #{4}] [#{1 4} #{5}] [#{1 6} #{7}]] #{1 2 3 4 5}))

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
                                        (make-implication #{2 0} #{3})}))))

(deftest test-equivalent-implications?
  (is (equivalent-implications? #{(make-implication #{1} #{2}),
                                  (make-implication #{1} #{3})}
                                #{(make-implication #{1} #{2 3})}))
  (is (not (equivalent-implications? #{(make-implication #{1} #{2})}
                                     #{(make-implication #{2} #{1})}))))

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

(deftest test-minimal-intersection-sets
  (let [minimal-intersection-sets @#'conexp.fca.implications/minimal-intersection-sets]
    (are [sets minimal-sets] (= (set minimal-sets) (set (minimal-intersection-sets sets)))
      [#{2} #{2 4}] [#{2}])))

(deftest test-proper-premises
  (let [A-dot @#'conexp.fca.implications/A-dot]
    (are [ctx] (and (is (forall [A (proper-premises ctx)]
                          (proper-premise? ctx A)))
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
                      [4 8] [5 2] [5 3] [5 5] [5 7]}))))

;;;

nil
