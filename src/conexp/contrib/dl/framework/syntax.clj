;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.framework.syntax
  (:use conexp)
  (:use clojure.contrib.pprint
	[clojure.walk :only (walk)]))


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

(defn signature
  "Returns the signature of the given language, i.e. the pair of role
  names and concept names."
  [language]
  [(role-names language), (concept-names language)])

(defn constructors
  "Returns all allowed constructors of the given language."
  [language]
  (:constructors language))

(defmethod print-method ::DL [dl out]
  (let [#^String output (with-out-str
			  (pprint (list 'DL
					(concept-names dl)
					(role-names dl)
					(constructors dl))))]
    (.write out (.trim output))))

(defn make-language
  "Creates a DL from concept-names, role-names and constructors."
  [concept-names role-names constructors]
  (DL (set concept-names) (set role-names) (set constructors)))

;;;

(deftype DL-expression [language sexp])

(defn expression
  "Returns the s-exp describing this expression."
  [dl-expression]
  (:sexp dl-expression))

(defn expression-language
  "Returns the language of this expression."
  [dl-expression]
  (:language dl-expression))

(defmethod print-method ::DL-expression [dl-exp out]
  (.write out (print-str (list 'DL-expr (expression dl-exp)))))

;;;

(defmulti transform-expression
  "Transforms given DL expression as defined in language."
  (fn [language expression]
    (if (list? expression)
      [language (first expression)]
      language)))

(defmethod transform-expression :default [language expression]
  (let [base-transformer (get-method transform-expression language),
	default-transformer (get-method transform-expression :default)]
    (when (or (nil? base-transformer)
	      (= base-transformer default-transformer))
      (illegal-argument "Language " (print-str language) " not known for transformation."))
    (base-transformer language expression)))

(defn dl-expression?
  "Returns true iff thing is a DL expression."
  [thing]
  (= (type thing) ::DL-expression))

(defn- dl-sexp->term
  "Ensures no dl-expression objects in the syntax expression given."
  [expr]
  (cond
   (dl-expression? expr) (expression expr),
   (sequential? expr)    (walk dl-sexp->term identity expr),
   :else                 expr))

(defn make-dl-expression
  "Takes a DL and a s-exp describing a concept description and returns
  a DL-expression."
  [language dl-sexp]
  (DL-expression language (dl-sexp->term dl-sexp)))

;;;

(defmacro dl-expression
  "Allows input of DL s-expression without quoting. Symbols starting
  with a capital letter are quoted, symbols in the first position of a
  sequence are quoted and everything else is left as it is."
  [expression]
  (let [transform-symbol (fn [symbol]
			   (if (Character/isUpperCase (first (str symbol)))
			     (list 'quote symbol)
			     symbol)),
	transform (fn transform [sexp]
		    (cond
		     (seq? sexp)        (if (empty? sexp)
					  sexp
					  (list* 'list (list 'quote (first sexp))
						 (walk transform identity (rest sexp)))),
		     (sequential? sexp) (walk transform identity sexp),
		     (symbol? sexp)     (transform-symbol sexp),
		     :else              sexp))]
     (transform expression)))

(defmacro define-dl
  "Defines a DL."
  [name concept-names role-names constructors & options]
  (let [options (apply hash-map options)]
    (when (exists [name (concat concept-names role-names)]
	    (not (Character/isUpperCase (first (str name)))))
      (illegal-argument "Concept and role names must start with a capital letter."))
    `(do
       (def ~name (make-language '~concept-names '~role-names '~constructors))

       ;; untested
       (defmethod transform-expression ~name [language# expression#]
	 expression#)
       ~@(map (fn [dl-sexp body]
		(let [cons-name (first dl-sexp),
		      cons-args (rest dl-sexp)]
		  `(defmethod transform-expression [~name '~cons-name]
		     [language# expression#]
		     (let [~(vec cons-args) (map transform-expression (rest expression#))]
		       ~@body))))
	      (:syntax-transformers options))

       ~name)))

;;;

(defn compound?
  "Returns true iff given expression is a compound expression."
  [dl-expression]
  (let [expr (expression dl-expression)]
    (seq? expr)))

(defn atomic?
  "Returns true iff given expression is an atomic expression."
  [dl-expression]
  (not (compound? dl-expression)))

(defn primitive?
  "Returns true iff given expression consists of a concept name or a
  role name only."
  [dl-expression]
  (and (atomic? dl-expression)
       (or (contains? (concept-names (expression-language dl-expression))
		      (expression dl-expression))
	   (contains? (role-names (expression-language dl-expression))
		      (expression dl-expression)))))

(defn operator
  "Returns the operator of the expression."
  [dl-expression]
  (when-not (compound? dl-expression)
    (illegal-argument "Given expression is atomic and has no operator."))
  (first (expression dl-expression)))

(defn arguments
  "Returns the operator arguments of the expression."
  [dl-expression]
  (when-not (compound? dl-expression)
    (illegal-argument "Given expression is atomic and has no arguments."))
  (map #(if-not (dl-expression? %)
	  (make-dl-expression (expression-language dl-expression) %)
	  %)
       (rest (expression dl-expression))))

(defn symbols-in-expression
  "Returns all symbols used in expressions."
  [dl-expression]
  (let [collector (fn collector [expr]
		    (cond
		     (seq? expr) (vec (reduce concat (map collector (rest expr)))),
		     (dl-expression? expr) (collector (expression expr)),
		     :else [expr]))]
    (set (collector (expression dl-expression)))))

(defn role-names-in-expression
  "Returns all role names used in the given expression."
  [dl-expression]
  (intersection (role-names (expression-language dl-expression))
		(symbols-in-expression dl-expression)))

(defn concept-names-in-expression
  "Returns all concept names used in the given expression."
  [dl-expression]
  (intersection (concept-names (expression-language dl-expression))
		(symbols-in-expression dl-expression)))

(defn free-symbols-in-expression
  "Returns all free symbols in the given expression."
  [dl-expression]
  (difference (symbols-in-expression dl-expression)
	      (union (role-names-in-expression dl-expression)
		     (concept-names-in-expression dl-expression))))

;;; Definitions

(deftype DL-definition [target dl-expression])

(defn definition-target
  "Returns target of this definition."
  [definition]
  (:target definition))

(defn definition-expression
  "Returns expression of this definition."
  [definition]
  (:dl-expression definition))

(defmethod print-method ::DL-definition [definition out]
  (.write out (with-out-str
		(print (definition-target definition))
		(print " := ")
		(print (definition-expression definition)))))

(defn make-dl-definition
  "Creates and returns a DL definition."
  ([target definition-expression]
     (DL-definition target definition-expression))
  ([language target definition-sexp]
     (DL-definition target (make-dl-expression language definition-sexp))))

;;; Subsumptions

;;; Equivalences

;;;

nil
