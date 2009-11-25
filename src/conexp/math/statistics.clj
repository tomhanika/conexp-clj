(ns conexp.math.statistics
  (:use conexp.base
	conexp.math.util)
  (:import [org.apache.commons.math.stat.regression SimpleRegression]))

(defn linear-regression
  "Computes the linear regression of the given sequence of points
  (at least 2). Returns the pair of intercept and slope of the linear
  regression, i.e. for y = a + b*x the pair [a b] is returned."
  [points]
  (if (> 2 (count points))
    (illegal-argument "linear-regression needs at least 2 points."))
  (let [regression (SimpleRegression.)]
    (doseq [[x y] points]
      (.addData regression (double x) (double y)))
    [(.getIntercept regression) (.getSlope regression)]))

nil
