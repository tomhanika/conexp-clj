;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.framework.syntax
  (:use conexp))


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
  (.write out "DL"))

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
  (.write out (str (expression dl-exp))))

(defn compound?
  "Returns true iff given expression is a compound expression."
  [dl-expression]
  (list? (expression dl-expression)))

(defn atomic?
  "Returns true iff given expression is an atomic expression."
  [dl-expression]
  (not (compound? dl-expression)))

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
  (rest (expression dl-expression)))

;;;

(defmulti transform-expression
  "Transforms given DL expression as defined in language."
  (fn [language expression]
    (if (list? expression)
      [language (first expression)]
      language)))

(defmethod transform-expression :default [language expression]
  (let [base-transformer (get-method transform-expression language)]
    (when (nil? base-transformer)
      (illegal-argument "Language " language " not known."))
    (base-transformer language expression)))

(defn make-dl-expression
  "Takes a DL and a s-exp describing a concept description and returns
  a DL-expression."
  [language dl-sexp]
  (DL-expression language (transform-expression language dl-sexp)))

;;;

(defmacro define-dl
  "Defines a DL."
  ;; FIXME: Extend description
  [name concept-names role-names constructors & syntax-transformers]
  `(do
     (def ~name (make-language '~concept-names '~role-names '~constructors))
     (defmethod transform-expression ~name [language# expression#]
       expression#)
     ~@(map (fn [dl-sexp body]
	      (let [cons-name (first dl-sexp),
		    cons-args (rest dl-sexp)]		
		`(defmethod transform-expression [~name '~cons-name]
		   [language# expression#]
		   (let [~(vec cons-args) (map transform-expression (rest expression#))]
		     ~@body))))
	    syntax-transformers)
     ~name))

;;;

nil
