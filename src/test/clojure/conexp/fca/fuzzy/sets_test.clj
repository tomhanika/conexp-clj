;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.sets-test
  (:use conexp.fca.fuzzy.sets
        conexp.fca.fuzzy.logics)
  (:use clojure.test))

(def fset1 (make-fuzzy-set {1 0.8 2 1.0 3 0.6 4 0.4}))
(def fset2 (make-fuzzy-set {2 0.6 4 0.4}))
(def fset2 (make-fuzzy-set {3 0.5 4 0.9 5 0.7}))

(define-fuzzy-set-operation union "Set Union of Fuzzy Sets." fuzzy-union)
(define-fuzzy-set-operation intersection "Set Intersection of Fuzzy Sets." fuzzy-intersection)
(define-fuzzy-set-operation difference "Set Difference of Fuzzy Sets." fuzzy-difference)
;;;

;; Fuzzy-Set
;; make-fuzzy-set
;; fuzzy-set-as-hashmap
;; fuzzy-set?

;;;

;; fuzzy-intersection
;; fuzzy-union
;; fuzzy-difference

(deftest test-fuzzy-subsets
  (is (= (set (fuzzy-subsets [0 1/2 1] (make-fuzzy-set {1 1/2 2 1 3 0})))
         (set (map make-fuzzy-set
                   (list {} {1 1/2} {2 1/2} {2 1/2, 1 1/2} {2 1} {2 1, 1 1/2}))))))

(deftest fuzzy-set-operations


)
;; fuzzy-subset?
;; subsethood

;;;

nil
