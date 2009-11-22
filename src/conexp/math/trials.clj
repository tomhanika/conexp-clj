(ns conexp.math.trials
  (:import [org.apache.commons.math.analysis MultivariateRealFunction]
	   [org.apache.commons.math.optimization RealPointValuePair GoalType]
	   [org.apache.commons.math.optimization.direct NelderMead])
  (:use conexp.base))

(defn as-multivariate-fn [fn]
  (proxy [MultivariateRealFunction] []
    (value [double-point]
      (apply fn (seq double-point)))))

(defn minimize [fn starting-point]
  (let [point-value-pair (.optimize (NelderMead.)
				    (as-multivariate-fn fn)
				    GoalType/MINIMIZE
				    (into-array Double/TYPE starting-point))]
    [(seq (.getPoint point-value-pair))
     (.getValue point-value-pair)]))

nil
