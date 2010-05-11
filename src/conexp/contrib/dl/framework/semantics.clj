;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.framework.semantics
  (:use conexp.main
	conexp.contrib.dl.framework.syntax
        conexp.contrib.dl.framework.boxes))

(update-ns-meta! conexp.contrib.dl.framework.semantics
  :doc "Defines basic notions for semantics of description logics.")

;;; Model definition

(defrecord Model [language base-set interpretation])

(defn model-base-set
  "Returns base set of a given model."
  [model]
  (:base-set model))

(defn model-language
  "Returns language of the given model."
  [model]
  (:language model))

(defn model-interpretation
  "Returns interpretation function of given model."
  [model]
  (:interpretation model))

(defn make-model
  "Returns a model for a given DL language on the given base set."
  [language base-set interpretation]
  (Model. language base-set interpretation))

(defmethod print-method Model [model out]
  (let [#^String output (with-out-str (print (list 'Model
                                                   (model-language model)
                                                   (model-base-set model)
                                                   (model-interpretation model))))]
    (.write out (.trim output))))

;;; Interpretations

(defmulti compile-expression
  "Compiles an expression to a function mapping a model to the extent
  of this expression in the model."
  (fn [dl-expression]
    (cond
     (compound? dl-expression)  (operator dl-expression),
     :else                      ::base-case)))

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
    (let [result ((model-interpretation model) (expression-term dl-expression))]
      (if (nil? result)
	(let [base-semantics (get-method compile-expression
					 [(language-name (expression-language dl-expression)) ::base-semantics]),
	      default        (get-method compile-expression
					 :default)]
	  (when (= base-semantics default)
	    (throw (IllegalStateException. (str "Cannot interpret " (print-str dl-expression) "."))))
	  ((base-semantics dl-expression) model))
	result))))

;;; base semantics (i.e. what to do if nothing else applies)

(defmacro define-base-semantics
  "Define how to interpret an expression which is neither compound nor
  a primitive concept, i.e. TBox-ABox pairs and the like."
  [language [model dl-expression] & body]
  `(defmethod compile-expression [(language-name ~language) ::base-semantics] [~dl-expression]
     (fn [~model]
       ~@body)))


;;; defining new constructors

(defmacro define-constructor
  "Defines a new constructor for description logics. Captures the
  variables «model» and «dl-exp» for representing the model and the
  dl-expression used."
  [name & body]
  `(do
     (defmethod compile-expression '~name [~'dl-exp]
       (fn [~'model]
         ~@body))
     (add-common-constructor! '~name)))

(define-constructor and
  (reduce intersection (model-base-set model)
          (map #(interpret model %) (arguments dl-exp))))

(define-constructor or
  (reduce union #{}
          (map #(interpret model %) (arguments dl-exp))))

(define-constructor not
  (difference (model-base-set model)
              (interpret model (first (arguments dl-exp)))))

(define-constructor exists
  (let [r-I (interpret model (first (arguments dl-exp))),
        C-I (interpret model (second (arguments dl-exp)))]
    (set-of x [x (model-base-set model)
               :when (exists [y C-I] (contains? r-I [x y]))])))

(define-constructor forall
  (let [r-I (interpret model (first (arguments dl-exp))),
        C-I (interpret model (second (arguments dl-exp)))]
    (set-of x [x (model-base-set model)
               :when (forall [y C-I] (contains? r-I [x y]))])))

(define-constructor inverse
  (let [r-I (interpret model (first (arguments dl-exp)))]
    (set-of [y x] [[x y] r-I])))

;;; Model Syntax

(defmacro model
  "Defines model for language on base-set: interpretation maps atomic
  expressions to their extents."
  [language base-set & interpretation]
  `(let [interpretation-map# '~(apply hash-map interpretation),
	 defined-symbols# (keys interpretation-map#),
	 undefined-symbols# (difference (union (concept-names ~language)
					       (role-names ~language))
					(set defined-symbols#))]
     (when (not (empty? undefined-symbols#))
       (illegal-argument "Definition of model is incomplete. The symbols "
			 undefined-symbols# " are missing."))
     (make-model ~language (set '~base-set) interpretation-map#)))

(add-dl-syntax! 'model)

(defmacro define-model
  "Globally defines model with name for language on base-set:
  interpretation maps atomic expressions to their extents."
  [name language base-set & interpretation]
  `(def ~name (model ~language ~base-set ~@interpretation)))

;;; Most Specific Concepts

(defmulti most-specific-concept
  "Computes the model based most specific concept of a set of objects
  in a given model."
  (fn [model dl-exp] (language-name (model-language model))))

(defmethod most-specific-concept :default [model _]
  (illegal-argument "Language " (print-str (model-language model)) " does not provide msc."))

(defmacro define-msc
  "Defines model based most specific concepts for a language, a model
  and a set of objects. Must return a dl-expression."
  [language [model objects] & body]
  `(defmethod most-specific-concept (language-name ~language)
     [~model ~objects]
     ~@body))

(defn model-closure
  "Return the most specific concept of the interpretation of dl-exp in
  model."
  [model dl-exp]
  (with-memoized-fns [interpret]
    (most-specific-concept model (interpret model dl-exp))))

;;;

(defn extend-model
  "Extends model by given interpretation function i. i should return
  nil if it doesn't change a value of model's original interpretion,
  where then the original interpretation is used."
  [model i]
  (make-model (model-language model)
	      (model-base-set model)
	      (fn [A]
		(or (i A)
		    ((model-interpretation model) A)))))

(defn holds-in-model?
  "Returns true iff subsumption holds in given model."
  [model subsumption]
  (subset? (interpret model (subsumee subsumption))
	   (interpret model (subsumer subsumption))))

(defnk interpretation->model
  "Given concepts as a hash-map from symbols to sets and roles as a
  hash-map from symbols to sets of pairs returns a model containing
  the hash-maps as interpretation. If parameter :base-lang is given
  the description logic used in this model will be an extension of the
  parameter value."
  [concepts roles :base-lang nil]
  (let [concept-names (keys concepts),
        role-names    (keys roles),
        language      (make-dl (gensym)
                               concept-names
                               role-names
                               []
                               :extends base-lang),
        base-set      (union (set-of x [[conc extension] concepts,
                                        x extension])
                             (set-of x [[role extension] roles,
                                        pair extension,
                                        x pair]))]
    (make-model language base-set (merge concepts roles))))


;;; TBox interpretations

(defn- fixed-point
  "Apply f to data until (= old-data new-data)."
  [f data]
  (let [runner (fn runner [old-data]
		 (let [new-data (f old-data)]
		   (if (= new-data old-data)
		     new-data
		     (recur new-data))))]
    (runner data)))

(defn- next-tbox-interpretation
  "Defines a new interpretation on the defined concepts of tbox in
  model through an interpretation i of the defined concepts of tbox."
  [model tbox i]
  (let [new-model (extend-model model i)]
    (into {} (for [[sym sym-def] (tbox-definition-map tbox)]
               [sym (interpret new-model (definition-expression sym-def))]))))

(defn- constant-tbox-interpretation
  "Returns an interpretation on the defined concepts of tbox,
  constantly returning value."
  [tbox value]
  (into {} (for [concept (defined-concepts tbox)]
             [concept value])))

(defn gfp-model
  "Returns the gfp-model of tbox in model."
  [tbox model]
  (extend-model model
                (fixed-point (fn [i] (next-tbox-interpretation model tbox i))
                             (constant-tbox-interpretation tbox (model-base-set model)))))

(defn lfp-model
  "Returns the lfp-model of tbox in model."
  [tbox model]
  (extend-model model
                (fixed-point (fn [i] (next-tbox-interpretation model tbox i))
                             (constant-tbox-interpretation tbox #{}))))

;;;

nil
