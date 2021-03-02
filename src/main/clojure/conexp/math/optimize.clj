;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.math.optimize
  (:require [conexp.math.util :refer :all])
  (:import [org.apache.commons.math.optimization DifferentiableMultivariateRealOptimizer GoalType RealConvergenceChecker RealPointValuePair]
           [org.apache.commons.math.optimization.direct DirectSearchOptimizer NelderMead]
           [org.apache.commons.math.optimization.general ConjugateGradientFormula NonLinearConjugateGradientOptimizer]))

;;;

;; better way to type hint without code duplication?

(defn- make-direct-optimizer
  "Direct optimizer used by directly-optimize."
  [options]
  (let [^DirectSearchOptimizer optimizer (NelderMead.)]
    (when (:iterations options)
      (let [^RealConvergenceChecker cc (.getConvergenceChecker optimizer)]
        (.setConvergenceChecker optimizer
                                (proxy [RealConvergenceChecker] []
                                  (converged [iterations previous current]
                                    (or (.converged cc iterations previous current)
                                        (>= iterations (:iterations options))))))))
    optimizer))

(defn- make-differential-optimizer
  "Optimizer for differentiable functions used by differentially-optimize."
  [options]
  (let [^DifferentiableMultivariateRealOptimizer optimizer
        (NonLinearConjugateGradientOptimizer. ConjugateGradientFormula/FLETCHER_REEVES)]
    (when (:iterations options)
      (let [^RealConvergenceChecker cc (.getConvergenceChecker optimizer)]
        (.setConvergenceChecker optimizer
                                (proxy [RealConvergenceChecker] []
                                  (converged [iterations previous current]
                                    (or (.converged cc iterations previous current)
                                        (>= iterations (:iterations options))))))))
    optimizer))

(defn- point-value-pair-to-vector
  "Converts RealPointValuePair to a point-value-vector."
  [^RealPointValuePair pvp]
  [(vec (.getPoint pvp)) (.getValue pvp)])

(defn- directly-optimize
  "Optimizes fn according to goal as given by
  *direct-optimizer*. options is a hash-map to control the optimizer,
  fn must be suitable for as-multivariate-fn."
  [fn starting-point goal options]
  (let [point-value-pair (.optimize ^DirectSearchOptimizer (make-direct-optimizer options)
                                    (as-multivariate-real-fn fn)
                                    goal
                                    (into-array Double/TYPE starting-point))]
    (point-value-pair-to-vector point-value-pair)))

(defn- differentially-optimize
  "Optimizes fn according to goal as given by
  *differential-optimizer*. partial-derivatives must be a function
  computing the k-th partial derivation (as clojure function) when
  given k. options is a hash-map to control the optimizer, fn must be
  suitable for as-differentiable-multivariate-real-fn."
  [fn partial-derivatives starting-point goal options]
  (let [point-value-pair (.optimize ^DifferentiableMultivariateRealOptimizer (make-differential-optimizer options)
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
  direct one. options is a hash-map to control the optimizer."
  ([fn starting-point options]
     (directly-optimize fn starting-point GoalType/MINIMIZE options))
  ([fn partial-derivatives starting-point options]
     (differentially-optimize fn partial-derivatives starting-point GoalType/MINIMIZE options)))

(defn maximize
  "Maximizes fn starting at starting-point. When given
  partial-derivatives uses a differential optimizer, otherwise uses a
  direct one. options is a hash-map to control the optimizer."
  ([fn starting-point options]
     (directly-optimize fn starting-point GoalType/MAXIMIZE options))
  ([fn partial-derivatives starting-point options]
     (differentially-optimize fn partial-derivatives starting-point GoalType/MAXIMIZE options)))

;;;

nil
