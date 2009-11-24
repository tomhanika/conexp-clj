(ns conexp.math.optimize
  (:use conexp.math.util)
  (:import [org.apache.commons.math.optimization RealPointValuePair GoalType]
	   [org.apache.commons.math.optimization.direct NelderMead]))

(defn optimize [fn starting-point goal]
  (let [point-value-pair (.optimize (NelderMead.)
				    (as-multivariate-real-fn fn)
				    goal
				    (into-array Double/TYPE starting-point))]
    [(seq (.getPoint point-value-pair))
     (.getValue point-value-pair)]))

(defn minimize [fn starting-point]
  (optimize fn starting-point GoalType/MINIMIZE))

(defn maximize [fn starting-point]
  (optimize fn starting-point GoalType/MAXIMIZE))

nil
