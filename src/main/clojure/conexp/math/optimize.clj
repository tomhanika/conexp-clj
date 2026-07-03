;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.math.optimize
  (:require [conexp.math.util :refer :all])
  (:import [org.apache.commons.math3.optim ConvergenceChecker InitialGuess MaxEval MaxIter OptimizationData PointValuePair SimpleValueChecker]
           [org.apache.commons.math3.optim.nonlinear.scalar GoalType ObjectiveFunction ObjectiveFunctionGradient]
           [org.apache.commons.math3.optim.nonlinear.scalar.noderiv NelderMeadSimplex SimplexOptimizer]
           [org.apache.commons.math3.optim.nonlinear.scalar.gradient NonLinearConjugateGradientOptimizer NonLinearConjugateGradientOptimizer$Formula]))

;;;

;; Relative and absolute thresholds for value-based convergence.
(def ^:private convergence-tolerances [1e-14 1e-30])

(defn- convergence-checker
  "Returns a ConvergenceChecker that converges when the underlying
  value-based checker converges or, when max-iterations is given, once
  that many iterations have been performed.  The iteration bound mirrors
  the behaviour of the old commons-math 2 optimizers used here, where an
  :iterations option forced convergence (returning the current best)
  rather than raising an exception as MaxEval/MaxIter do in
  commons-math 3."
  [max-iterations]
  (let [[rel abs] convergence-tolerances
        base      (SimpleValueChecker. rel abs)]
    (reify ConvergenceChecker
      (converged [_ iteration previous current]
        (or (.converged base iteration previous current)
            (boolean (and max-iterations (>= iteration (long max-iterations)))))))))

(defn- point-value-pair-to-vector
  "Converts PointValuePair to a point-value-vector."
  [^PointValuePair pvp]
  [(vec (.getPoint pvp)) (.getValue pvp)])

(defn- directly-minimize
  "Minimizes fn using a Nelder-Mead simplex optimizer. options is a
  hash-map to control the optimizer, fn must be suitable for
  as-multivariate-real-fn."
  [fn starting-point options]
  (let [optimizer (SimplexOptimizer. (convergence-checker (:iterations options)))
        result    ^PointValuePair
                  (.optimize optimizer
                             (into-array OptimizationData
                                         [(MaxEval/unlimited)
                                          (MaxIter/unlimited)
                                          (ObjectiveFunction. (as-multivariate-real-fn fn))
                                          GoalType/MINIMIZE
                                          (InitialGuess. (double-array starting-point))
                                          (NelderMeadSimplex. (count starting-point))]))]
    (point-value-pair-to-vector result)))

(defn- differentially-minimize
  "Minimizes fn using a non-linear conjugate gradient optimizer.
  partial-derivatives must be a function computing the k-th partial
  derivation (as clojure function) when given k. options is a hash-map
  to control the optimizer, fn must be suitable for
  as-multivariate-real-fn."
  [fn partial-derivatives starting-point options]
  (let [optimizer (NonLinearConjugateGradientOptimizer.
                   NonLinearConjugateGradientOptimizer$Formula/FLETCHER_REEVES
                   (convergence-checker (:iterations options)))
        result    ^PointValuePair
                  (.optimize optimizer
                             (into-array OptimizationData
                                         [(MaxEval/unlimited)
                                          (MaxIter/unlimited)
                                          (ObjectiveFunction. (as-multivariate-real-fn fn))
                                          (ObjectiveFunctionGradient.
                                           (as-gradient-fn (count starting-point) partial-derivatives))
                                          GoalType/MINIMIZE
                                          (InitialGuess. (double-array starting-point))]))]
    (point-value-pair-to-vector result)))

(defn minimize
  "Minimizes fn starting at starting-point. When given
  partial-derivatives uses a differential optimizer, otherwise uses a
  direct one. options is a hash-map to control the optimizer."
  ([fn starting-point options]
     (directly-minimize fn starting-point options))
  ([fn partial-derivatives starting-point options]
     (differentially-minimize fn partial-derivatives starting-point options)))

(defn maximize
  "Maximizes fn starting at starting-point. When given
  partial-derivatives uses a differential optimizer, otherwise uses a
  direct one. options is a hash-map to control the optimizer.

  Implemented as the minimization of the negated function (and negated
  gradient), which gives commons-math 3's optimizers the same
  convergence behaviour for maximization as for minimization."
  ([fn starting-point options]
     (let [[point value] (minimize (comp - fn) starting-point options)]
       [point (- value)]))
  ([fn partial-derivatives starting-point options]
     (let [neg-derivatives #(comp - (partial-derivatives %))
           [point value]   (minimize (comp - fn) neg-derivatives starting-point options)]
       [point (- value)])))

;;;

nil
