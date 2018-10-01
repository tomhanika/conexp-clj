;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.math.statistics-test
  (:use conexp.math.statistics)
  (:use clojure.test))

;;;

(deftest test-linear-regression
  (is (= [0.0 1.0]
         (linear-regression [[0 0] [1 1] [2 2]]))))

;;;

nil
