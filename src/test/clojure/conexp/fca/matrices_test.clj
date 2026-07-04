;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.matrices-test
  (:use clojure.test)
  (:require [conexp.fca.matrices :refer :all]))

;;; Accessors and shape

(deftest test-transpose
  (is (= [[1 4] [2 5] [3 6]] (transpose [[1 2 3] [4 5 6]])))
  (is (= [[1 2 3] [4 5 6]] (transpose (transpose [[1 2 3] [4 5 6]]))))
  (is (= [[1] [2] [3]] (transpose [[1 2 3]]))))

(deftest test-matrix-row-and-column
  (let [M [[1 2 3] [4 5 6]]]
    (is (= [1 2 3] (matrix-row M 0)))
    (is (= [4 5 6] (matrix-row M 1)))
    (is (= [1 4] (matrix-column M 0)))
    (is (= [3 6] (matrix-column M 2)))))

(deftest test-row-and-col-number
  (let [M [[1 2 3] [4 5 6]]]
    (is (= 2 (row-number M)))
    (is (= 3 (col-number M)))))

;;; Row/column mutation

(deftest test-add-row
  (is (= [[1 2] [3 4] [5 6]] (add-row [[1 2] [3 4]] [5 6])))
  (is (= [[1 2]] (add-row [] [1 2])))
  (is (thrown? Exception (add-row [[1 2]] [1 2 3]))))

(deftest test-set-row
  (is (= [[9 9] [3 4]] (set-row [[1 2] [3 4]] 0 [9 9])))
  (is (= [[1 2] [9 9]] (set-row [[1 2] [3 4]] 1 [9 9]))))

(deftest test-add-column
  (is (= [[1 2 5] [3 4 6]] (add-column [[1 2] [3 4]] [5 6])))
  (is (= [[1] [2]] (add-column [] [1 2])))
  (is (thrown? Exception (add-column [[1 2] [3 4]] [5 6 7]))))

(deftest test-set-column
  (is (= [[1 9] [3 9]] (set-column [[1 2] [3 4]] 1 [9 9])))
  (is (= [[9 2] [9 4]] (set-column [[1 2] [3 4]] 0 [9 9]))))

;;; Vector / matrix arithmetic

(deftest test-scalar-product
  (is (= 32 (scalar-product [1 2 3] [4 5 6])))
  (is (= 0 (scalar-product [0 0] [7 9])))
  (is (= 0 (scalar-product [] []))))

(deftest test-outer-prod
  (is (= [[3 4] [6 8]] (outer-prod [1 2] [3 4])))
  (is (= [[0 0] [0 0]] (outer-prod [0 0] [1 2]))))

(deftest test-boolean-matrix-product
  ;; identity on the left leaves M2 unchanged
  (is (= [[1 1] [0 1]] (boolean-matrix-product [[1 0] [0 1]] [[1 1] [0 1]])))
  ;; entries saturate at 1 (boolean OR), never accumulate
  (is (= [[1]] (boolean-matrix-product [[1 1]] [[1] [1]]))))

(deftest test-matrix-boolean-difference
  (is (= [[1 0] [0 0]] (matrix-boolean-difference [[1 1] [0 1]] [[0 1] [1 1]])))
  ;; 0 - 1 clamps to 0
  (is (= [[0 0]] (matrix-boolean-difference [[0 1]] [[1 1]]))))

(deftest test-matrix-entrywise-product
  (is (= [[5 12] [21 32]] (matrix-entrywise-product [[1 2] [3 4]] [[5 6] [7 8]])))
  (is (= [[0 0] [0 0]] (matrix-entrywise-product [[1 2] [3 4]] [[0 0] [0 0]]))))
