;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.fuzzy.sets-test
  (:use conexp.fca.fuzzy.sets)
  (:use clojure.test))

(def fset1 (make-fuzzy-set {1 0.8 2 1.0 3 0.6 4 0.4}))
(def fset2 (make-fuzzy-set {2 0.6 4 0.4}))
(def fset3 (make-fuzzy-set {3 0.5 4 0.9 5 0.7}))


(deftest test-fuzzy-subsets
  (is (= (set (fuzzy-subsets [0 1/2 1] (make-fuzzy-set {1 1/2 2 1 3 0})))
         (set (map make-fuzzy-set
                   (list {} {1 1/2} {2 1/2} {2 1/2, 1 1/2} {2 1} {2 1, 1 1/2})))))

  (is (fuzzy-subset? (make-fuzzy-set {1 0.5 2 0.3}) (make-fuzzy-set {1 1.0 2 0.5})))
  (is (fuzzy-subset? (make-fuzzy-set {1 0.3 2 0.7}) (make-fuzzy-set {1 0.3 2 1.0})))

  (is (not (fuzzy-subset? (make-fuzzy-set {1 0.5 2 0.3}) (make-fuzzy-set {1 1.0 2 0.2}))))
  (is (not (fuzzy-subset? (make-fuzzy-set {1 0.3 2 0.7}) (make-fuzzy-set {1 0.5}))))

  )

(deftest fuzzy-set-operations

  (is (= (fuzzy-union fset1 fset2)
         (make-fuzzy-set {1 0.8 2 1.0 3 0.6 4 0.4})))
  (is (= (fuzzy-union fset2 fset3)
         (make-fuzzy-set {2 0.6 3 0.5 4 0.9 5 0.7})))
  (is (= (fuzzy-union fset1 fset3)
         (make-fuzzy-set {1 0.8 2 1.0 3 0.6 4 0.9 5 0.7})))

  (is (= (fuzzy-intersection fset1 fset2)
         (make-fuzzy-set {2 0.6 4 0.4})))
  (is (= (fuzzy-intersection fset2 fset3)
         (make-fuzzy-set {4 0.4})))
  (is (= (fuzzy-intersection fset1 fset3)
         (make-fuzzy-set {3 0.5 4 0.4})))

  (is (= (fuzzy-difference fset1 fset2)
         (make-fuzzy-set {1 0.8 2 0.4 3 0.6})))
  (is (= (fuzzy-difference fset2 fset3)
         (make-fuzzy-set {2 0.6})))
  (is (= (fuzzy-difference fset1 fset3)
         (make-fuzzy-set {1 0.8 2 1.0 3 0.09999999999999998})))
)
