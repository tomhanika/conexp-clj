;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.math.util-test
  (:use conexp.math.util)
  (:use clojure.test))

;;;

(deftest test-binomial-coefficient
  (is (= 2203961430
         (binomial-coefficient 34 18))))

(deftest test-eval-polynomial
  (is (= 100
         (eval-polynomial
           (take 100 (repeat 1))
           1)))
  (is (= 3.0 (eval-polynomial [3] 0.42)))
  (let [polynomial [3 2 1 5]]
    (is (= (/ 39 8) (eval-polynomial polynomial (/ 1 2))))
    (is (= 3.48 (eval-polynomial polynomial 0.2)))))

;;;

nil
