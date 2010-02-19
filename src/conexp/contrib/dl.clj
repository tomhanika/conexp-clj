(ns conexp.contrib.dl
  (:use conexp))

(update-ns-meta! conexp.contrib.dl
  :doc "Some experiments with description logics.")

;;;

(deftype DL [concept-names role-names constructors])

(defn concept-names
  "Returns the concept names of the given language."
  [language]
  (:concept-names language))

(defn role-names
  "Returns the role names of the given language."
  [language]
  (:role-names language))

(defn constructors
  "Returns all allowed constructors of the given language."
  [language]
  (:constructors language))

(defn make-language
  "Creates a DL from concept-names, role-names and constructors."
  [concept-names role-names constructors]
  (DL (set concept-names) (set role-names) (set constructors)))

;;;

(deftype Model [langauge base-set base-interpretation])

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

;;;

(deftype DL-expression [language sexp function])

(defn expression
  "Returns the s-exp describing this expression."
  [dl-expression]
  (:sexp dl-expression))

(defmethod print-method ::DL-expression [dl-exp out]
  (.write out (str (expression dl-exp))))

(defmulti compile-dl-expression
  "Compiles given DL s-expression in language, returning a function
  mapping a model to the interpretation of this expression in this
  model."
  (fn [language dl-sexp]
    [language (if (seq? dl-sexp)
		(first dl-sexp)
		::base-case)]))

(defn make-dl-expression
  "Takes a DL and a s-exp describing a concept description and returns
  a DL-expression."
  [language dl-sexp]
  (DL-expression language dl-sexp (compile-dl-expression language dl-sexp)))

(defn interpret
  "Interprets given dl-expression in model."
  [model dl-expression]
  ((:function dl-expression) model))

;;;

(defmacro define-dl
  "Defines a description logic from the given
  arguments. interpretations is a sequence of alternating constructor
  name and functions taking a model and the arguments of a dl-sexp
  with the given constructor."
  [name concept-names role-names constructors & interpretations]
  (when (not= (set constructors)
	      (set (take-nth 2 interpretations)))
    (illegal-argument "Interpretations of constructors invalid."))
  `(do
     (def ~name (make-language '~concept-names '~role-names '~constructors))
     (defmethod compile-dl-expression [~name ::base-case]
       [language# dl-sexp#]
       (fn [model#]
	 ((model-base-interpretation model#) dl-sexp#)))
     ~@(map (fn [cons-int-pair#]
	      (let [constructor# (first cons-int-pair#),
		    interpretation# (second cons-int-pair#)]
		`(defmethod compile-dl-expression [~name '~constructor#]
		   [language# dl-sexp#]
		   (fn [model#]
		     (binding [~'interpret (fn [model# thing#]
					     (thing# model#))]
		       (apply ~interpretation# model# 
			      (map (fn [sexp#]
				     (compile-dl-expression language# sexp#))
				   (rest dl-sexp#))))))))
	    (partition 2 interpretations))
     ~name))

;;;

nil
