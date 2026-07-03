;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.math.markov-test
  (:use clojure.test)
  (:require [conexp.math.markov :refer [generate-sobol generate-svdc]]))

(deftest test-generate-sobol-known-values
  (is (= 0 (generate-sobol 0)))
  (is (= [0 1/2 1/4 3/4 1/8 5/8 3/8 7/8 1/16]
         (mapv generate-sobol (range 9)))))

(deftest test-generate-sobol-low-discrepancy
  ;; The first 2^k points of a base-2 low-discrepancy sequence are exactly
  ;; the dyadic rationals {0, 1/2^k, ..., (2^k - 1)/2^k}; in particular they
  ;; are all distinct -- the property the previous implementation violated
  ;; (it repeated 1/8 and 3/8 and omitted 5/8 and 7/8).
  (doseq [k (range 1 8)]
    (let [size (bit-shift-left 1 k)
          pts  (map generate-sobol (range size))]
      (is (apply distinct? pts)
          (str "first 2^" k " points must be distinct"))
      (is (= (set (map #(/ % size) (range size)))
             (set pts))
          (str "first 2^" k " points must be the dyadic rationals")))))

(deftest test-generate-sobol-range
  (is (every? #(and (<= 0 %) (< % 1)) (map generate-sobol (range 256)))))

(deftest test-generate-svdc-range
  ;; svdc is a *scrambled* (randomised) van der Corput sequence, so only its
  ;; range is deterministic.
  (is (every? #(and (<= 0 %) (< % 1))
              (map #(generate-svdc % 2) (range 1 21)))))
