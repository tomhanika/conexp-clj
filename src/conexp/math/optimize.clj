(ns conexp.math.optimize
  (:use conexp.math.util
	conexp.base)
  (:import [org.apache.commons.math.optimization RealPointValuePair GoalType]
	   [org.apache.commons.math.optimization.direct DirectSearchOptimizer NelderMead]
	   [org.apache.commons.math.optimization.general AbstractScalarDifferentiableOptimizer
	                                                 NonLinearConjugateGradientOptimizer
	                                                 ConjugateGradientFormula]))

;;;

(defvar *direct-optimizer* (fn [] (NelderMead.))
  "Direct optimizer used by directly-optimize")
(defvar *differential-optimizer*
  (fn [] (NonLinearConjugateGradientOptimizer. ConjugateGradientFormula/FLETCHER_REEVES))
  "Optimizer for differentiable functions used by differentially-optimize.")

(defn- point-value-pair-to-vector
  "Converts RealPointValuePair to a point-value-vector."
  [#^RealPointValuePair pvp]
  [(vec (.getPoint pvp)) (.getValue pvp)])

(defn- directly-optimize
  "Optimizes fn according to goal as given by *direct-optimizer*."
  [fn starting-point goal]
  (let [point-value-pair (.optimize (*direct-optimizer*)
				    (as-multivariate-real-fn fn)
				    goal
				    (into-array Double/TYPE starting-point))]
    (point-value-pair-to-vector point-value-pair)))

(defn- differentially-optimize
  "Optimizes fn according to goal as given by
  *differential-optimizer*. partial-derivatives must be a function
  computing the k-th partial derivation (as clojure function) when
  given k."
  [fn partial-derivatives starting-point goal]
  (let [point-value-pair (.optimize (*differential-optimizer*)
				    (as-differentiable-multivariate-real-fn
				     fn
				     (count starting-point)
				     partial-derivatives)
				    goal
				    (into-array Double/TYPE starting-point))]
    (point-value-pair-to-vector point-value-pair)))

(defn minimize
  "Minimizes fn starting at starting-point. When given
  partial-derivatives uses a differential optimizer, otherwise uses a
  direct one."
  ([fn starting-point]
     (directly-optimize fn starting-point GoalType/MINIMIZE))
  ([fn partial-derivatives starting-point]
     (differentially-optimize fn partial-derivatives starting-point GoalType/MINIMIZE)))

(comment
  Use this way

  (minimize #(Math/sin %)
	    (fn [k]
	      (condp = k
		0 #(Math/cos %)
		(constantly 0.0)))
	    [0.0])

  -> [[-1.5707963267948966] -1.0]
)

(defn maximize
  "Maximizes fn starting at starting-point. When given
  partial-derivatives uses a differential optimizer, otherwise uses a
  direct one."
  ([fn starting-point]
     (directly-optimize fn starting-point GoalType/MAXIMIZE))
  ([fn partial-derivatives starting-point]
     (differentially-optimize fn partial-derivatives starting-point GoalType/MAXIMIZE)))

;;;

nil
