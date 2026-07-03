;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.math.util
  (:import [org.apache.commons.math3.analysis MultivariateFunction MultivariateVectorFunction]))

;;; Interfacing Apache Commons Math

(defn as-multivariate-real-fn
  "Transforms given function fn to a MultivariateFunction as needed by
  commons-math. fn must be a function taking an array of doubles and
  returning one scalar double value."
  [fn]
  (reify MultivariateFunction
    (value [_ double-point]
      (double (fn double-point)))))

(defn as-multivariate-vectorial-fn
  "Transforms given function fn to a MultivariateVectorFunction as
  needed by commons-math. fn must be a function taking an array of
  doubles and returning a sequence of double values of fixed length."
  [fn]
  (reify MultivariateVectorFunction
    (value [_ double-point]
      (into-array Double/TYPE (fn double-point)))))

(defn as-gradient-fn
  "Returns a MultivariateVectorFunction computing the gradient of a
  function of number-of-args arguments from its partial derivatives.
  partial-derivatives must be a function taking a variable index k and
  returning a function representing the k-th partial derivative, itself
  taking an array of doubles and returning a scalar double.  This
  replaces the old commons-math 2 DifferentiableMultivariateRealFunction,
  as commons-math 3's optimizers take the objective and its gradient as
  separate arguments."
  [number-of-args partial-derivatives]
  (let [partials (map partial-derivatives (range number-of-args))]
    (as-multivariate-vectorial-fn
     (fn [double-point]
       (map #(% double-point) partials)))))

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


;;; Combinatorics

(defn binomial-coefficient
  "Straight forward computing of ${n\\choose k}$ by multiplying
  all (n-k) factors, where `run' is incremented up to k, divided by
  the incrementing divisor `run'"
  [n k]
  (assert (>= n k))
  (assert (>= k 0))
  (let [target (inc n)]
    (loop [run 1 result 1]
      (if (> run k)
        result
        (recur (inc run) (* (/ (- target run) run) result))))))

;;; Evaluation of Polynomials.

(defn eval-polynomial
  "Evaluates the polynomial p, represented by it's Coefficient-vector,
  at the point value. The function uses the `Horner Schema', see:
  https://de.wikipedia.org/wiki/Horner-Schema."
  [coefficients value]
  (assert (and (sequential? coefficients)
               (not (empty? coefficients)))
          "First argument must be a list of coefficients.")
  (assert (number? value)
          "Second argument must be a real number.")
  (reduce
    (fn [x y]
      (+ (* value x) y))
    0
    (reverse coefficients)))

;;;

nil
