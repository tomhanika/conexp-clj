;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.math.statistics
  (:require [conexp.base :refer :all])
  (:import org.apache.commons.math.stat.regression.SimpleRegression))

;;;

(defn linear-regression
  "Computes the linear regression of the given sequence of points
  (at least 2). Returns the pair of intercept and slope of the linear
  regression, i.e. for y = a + b*x the pair [a b] is returned."
  [points]
  (if (> 2 (count points))
    (illegal-argument "linear-regression needs at least 2 points."))
  (let [^SimpleRegression regression (SimpleRegression.)]
    (doseq [[x y] points]
      (.addData regression (double x) (double y)))
    [(.getIntercept regression) (.getSlope regression)]))

;;;

nil
