;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.math.util
  (:import [org.apache.commons.math.analysis MultivariateRealFunction
	                                     DifferentiableMultivariateRealFunction
	                                     MultivariateVectorialFunction]))

;;; Interfacing Apache Math

(defn as-multivariate-real-fn
  "Transforms given function fn to a MultivariateRealFunction as
  needed by commons-math. fn must be a function taking an array of
  doubles and returning one scalar double value."
  [fn]
  (proxy [MultivariateRealFunction] []
    (value [double-point]
      (double (fn double-point)))))

(defn as-multivariate-vectorial-fn
  "Transforms given function fn to a MultivariateVectorialFunction as
  needed by commons-math. fn must be a function taking an array of
  doubles and returning a sequence of double values of fixed length."
  [fn]
  (proxy [MultivariateVectorialFunction] []
    (value [double-point]
      (into-array Double/TYPE (fn double-point)))))

(defn as-differentiable-multivariate-real-fn
  "Transforms given function fn to a DifferentiableMultivariateRealFunction as
  needed by commons-math. fn must be a function suitable for as-multivariate-real-fn
  and partial-derivatives must be a function taking a variable index k and returning
  a function representing the k-th partial derivative of fn, also being suitable for
  as-multivariate-real-fn."
  [fun number-of-args partial-derivatives]
  (let [partials (map partial-derivatives (range number-of-args))]
    (proxy [DifferentiableMultivariateRealFunction] []
      (value [double-point]
        (double (fun double-point)))
      (partialDerivative [k]
        (as-multivariate-real-fn (nth partials k)))
      (gradient []
        (as-multivariate-vectorial-fn
	 (fn [& args]
	   (map #(apply % args) partials)))))))

;;; Types

(defmacro with-doubles
  "Defines all vars to be double valued in the evaluation of body."
  [vars & body]
  `(let ~(vec (mapcat (fn [var]
			`[~var (double ~var)])
		      vars))
     ~@body))

;;; Dirty Math Tricks

(defn partial-derivatives
  "Returns a function returning for an index n the partial derivative
  after the n-th coordinate of the given function f.

  The derivatives are computed with double-side differential quotients, so
  be aware of every anomaly which may occur."
  [f precision]
  (let [advance-coord-by (fn [values index h]
			   (assoc values index
				  (+ (nth values index) h)))]
    (fn [n]
      (fn [& args]
	(let [args (vec args)]
	  (/ (- (apply f (advance-coord-by args n precision))
		(apply f (advance-coord-by args n (- precision))))
	     (* 2 precision)))))))

;;;

nil
