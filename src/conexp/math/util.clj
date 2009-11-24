(ns conexp.math.util
  (:import [org.apache.commons.math.analysis MultivariateRealFunction
	                                     DifferentiableMultivariateRealFunction
	                                     MultivariateVectorialFunction]))

(defn as-multivariate-real-fn
  "Transforms given function fn to a MultivariateRealFunction as
  needed by commons-math. fn must be a function taking a fixed number
  of doubles and returning one scalar double value."
  [fn]
  (proxy [MultivariateRealFunction] []
    (value [double-point]
      (double (apply fn (seq double-point))))))

(defn as-multivariate-vectorial-fn
  "Transforms given function fn to a MultivariateVectorialFunction as
  needed by commons-math. fn must be a function taking a fixed number
  of doubles and returning a sequence of double values of fixed length."
  [fn]
  (proxy [MultivariateVectorialFunction] []
    (value [double-point]
      (into-array Double/TYPE (apply fn (seq double-point))))))

(defn as-differentiable-multivariate-real-fn [fun partial-derivatives]
  "Transforms given function fn to a DifferentiableMultivariateRealFunction as
  needed by commons-math. fn must be a function suitable for as-multivariate-real-fn
  and partial-derivatives must be a function taking a variable index k and returning
  a function representing the k-th partial derivative of fn, also being suitable for
  as-multivariate-real-fn."
  (let [partials (map partial-derivatives (iterate inc 0))]
    (proxy [DifferentiableMultivariateRealFunction] []
      (value [double-point]
        (double (apply fun (seq double-point))))
      (partialDerivative [k]
        (as-multivariate-real-fn (nth partials k)))
      (gradient []
        (as-multivariate-vectorial-fn
	 (fn [& args]
	   (map #(%1 %2) partials args)))))))

nil
