;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.incomplete-contexts.incomplete-contexts-test
  (:use clojure.test)
  (:require [conexp.fca.incomplete-contexts.incomplete-contexts :refer :all]
            [conexp.fca.implications :refer [make-implication]]))

;;; A small hand-computed incomplete context:
;;;      0 1
;;;   0 |x .        object 0: has 0, definitely not 1
;;;   1 |? x        object 1: unknown for 0, has 1
(def K (make-incomplete-context-from-matrix 2 2 [known-true  known-false
                                                 unknown     known-true]))

;;; Construction and accessors

(deftest test-accessors
  (is (= #{0 1} (objects K)))
  (is (= #{0 1} (attributes K)))
  (is (= {[0 0] known-true  [0 1] known-false
          [1 0] unknown     [1 1] known-true}
         (incidence K)))
  (is (incomplete-context? K))
  (is (not (incomplete-context? [1 2]))))

(deftest test-empty-and-single-object
  (let [e (make-empty-incomplete-context #{:a :b})]
    (is (= #{} (objects e)))
    (is (= #{:a :b} (attributes e))))
  (let [s (make-single-object-incomplete-context :g #{:a :b :c} #{:a} #{:b})]
    (is (= #{:g} (objects s)))
    (is (= #{:a :b :c} (attributes s)))
    (is (= known-true  (get (incidence s) [:g :a])))
    (is (= known-false (get (incidence s) [:g :b])))
    (is (= unknown     (get (incidence s) [:g :c])))))

;;; Value predicates and completeness

(deftest test-value-predicates
  (is (known-true?  known-true))
  (is (known-false? known-false))
  (is (unknown?     unknown))
  (is (not (known-true?  known-false)))
  (is (not (known-false? unknown)))
  (is (not (unknown?     known-true))))

(deftest test-completeness
  (is (not (complete-incomplete-context? K)))                       ; contains ?
  (is (complete-incomplete-context?
       (make-incomplete-context-from-matrix 2 2 [known-true  known-false
                                                 known-false known-true]))))

(deftest test-incidence-partitions
  (is (= #{[0 0] [1 1]} (set (map first (true-incidence K)))))
  (is (= #{[0 1]}       (set (map first (false-incidence K)))))
  (is (= #{[1 0]}       (set (map first (unknown-incidence K))))))

;;; Information order on the three values

(deftest test-information-supremum
  (is (= known-true  (information-supremum known-true  unknown)))
  (is (= known-false (information-supremum known-false unknown)))
  (is (= known-true  (information-supremum unknown     known-true)))
  (is (= known-true  (information-supremum known-true  known-true)))
  (is (= unknown     (information-supremum unknown     unknown)))
  (is (= known-true  (information-supremum known-true  nil)))
  (is (= unknown     (information-supremum nil         nil)))
  ;; contradictory information is rejected
  (is (thrown? IllegalArgumentException
               (information-supremum known-true known-false))))

;;; Derivations (certain = box, possible = diamond)

(deftest test-certain-object-derivation
  (is (= #{0}   (certain-object-derivation K #{0})))
  (is (= #{1}   (certain-object-derivation K #{1})))
  (is (= #{}    (certain-object-derivation K #{0 1})))
  (is (= #{0 1} (certain-object-derivation K #{}))))          ; vacuous over no objects

(deftest test-possible-object-derivation
  (is (= #{0}   (possible-object-derivation K #{0})))
  (is (= #{0 1} (possible-object-derivation K #{1})))
  (is (= #{0}   (possible-object-derivation K #{0 1}))))

(deftest test-certain-attribute-derivation
  (is (= #{0} (certain-attribute-derivation K #{0})))
  (is (= #{1} (certain-attribute-derivation K #{1})))
  (is (= #{}  (certain-attribute-derivation K #{0 1}))))

(deftest test-possible-attribute-derivation
  (is (= #{0 1} (possible-attribute-derivation K #{0})))
  (is (= #{1}   (possible-attribute-derivation K #{1})))
  (is (= #{1}   (possible-attribute-derivation K #{0 1}))))

(deftest test-attributes-not-had
  (is (= #{1} (attributes-not-had K #{0})))
  (is (= #{}  (attributes-not-had K #{1}))))

;;; Counterexamples

(deftest test-counterexamples
  (let [;; :a has 1, definitely not 2  -> counterexample to 1 -> 2
        witness   (make-incomplete-context-from-matrix [:a] [1 2] [known-true known-false])
        ;; :a has both 1 and 2  -> supports 1 -> 2, not a counterexample
        supporter (make-incomplete-context-from-matrix [:a] [1 2] [known-true known-true])
        impl      (make-implication #{1} #{2})]
    (is (is-counterexample? witness impl :a))
    (is (not (is-counterexample? supporter impl :a)))))
