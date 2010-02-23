;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.framework.models
  (:use conexp
	conexp.contrib.dl.framework.syntax))

;;; Model definition

(deftype Model [language base-set base-interpretation])

(defn model-base-set
  "Returns base set of a given model."
  [model]
  (:base-set model))

(defn model-language
  "Returns language of the given model."
  [model]
  (:language model))

(defn model-base-interpretation
  "Returns base interpretation of given model."
  [model]
  (:base-interpretation model))

(defn make-model
  "Returns a model for a given DL language on the given base set."
  [language base-set base-interpretation]
  (Model language base-set base-interpretation))

;;; Interpretation

(defmulti compile-expression
  "Compiles an expression to a function mapping a model to the extent
  of this expression in the model."
  (fn [dl-expression]
    (cond
     (compound? dl-expression)  (operator dl-expression),
     (primitive? dl-expression) ::base-case,
     :else                      [(expression-language dl-expression) ::base-semantics])))

(defmethod compile-expression :default [dl-expression]
  (illegal-argument "Dont know how to interpret " (print-str dl-expression)))

(defn interpret
  "Interprets given expression in given model and returns the
  corresponding extent."
  [model dl-expression]
  ((compile-expression (if (dl-expression? dl-expression)
			 dl-expression
			 (make-dl-expression (model-language model) dl-expression)))
   model))

(defmethod compile-expression ::base-case [dl-expression]
  (fn [model]
    ((model-base-interpretation model) (expression dl-expression))))

(defmethod compile-expression 'and [dl-expression]
  (fn [model]
    (reduce intersection (model-base-set model)
	    (map #(interpret model %) (arguments dl-expression)))))

(defmethod compile-expression 'exists [dl-expression]
  (fn [model]
    (let [r-I (interpret model (first (arguments dl-expression))),
	  C-I (interpret model (second (arguments dl-expression)))]
      (set-of x [x (model-base-set model),
		 :when (exists [y C-I]
			 (contains? r-I [x y]))]))))

(defmacro define-base-semantics
  "Define how to interpret an expression which is neither compound nor
  a primitive concept, i.e. TBox-ABox pairs and the like."
  [language [model dl-expression] & body]
  `(defmethod compile-expression [~language ::base-semantics] [~dl-expression]
     (fn [~model]
       ~@body)))

;;;

(defmacro define-model
  "Defines model for language on base-set: interpretation maps atomic
  expressions to their extents."
  [name language base-set interpretation]
  `(def ~name (make-model ~language (set '~base-set) '~interpretation)))

;;;

nil
