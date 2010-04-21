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

;;; Interpretation

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
    (let [result ((model-interpretation model) (expression dl-expression))]
      (if (nil? result)
	(let [base-semantics (get-method compile-expression
					 [(language-name (expression-language dl-expression)) ::base-semantics]),
	      default        (get-method compile-expression
					 :default)]
	  (when (= base-semantics default)
	    (throw (IllegalStateException. (str "Cannot interpret " (print-str dl-expression) "."))))
	  ((base-semantics dl-expression) model))
	result))))

(defmethod compile-expression 'and [dl-expression]
  (fn [model]
    (reduce intersection (model-base-set model)
	    (map #(interpret model %) (arguments dl-expression)))))

(add-common-constructor! 'and)

(defmethod compile-expression 'exists [dl-expression]
  (fn [model]
    (let [r-I (interpret model (first (arguments dl-expression))),
	  C-I (interpret model (second (arguments dl-expression)))]
      (set-of x [x (model-base-set model),
		 :when (exists [y C-I]
			 (contains? r-I [x y]))]))))

(add-common-constructor! 'exists)

(defmacro define-base-semantics
  "Define how to interpret an expression which is neither compound nor
  a primitive concept, i.e. TBox-ABox pairs and the like."
  [language [model dl-expression] & body]
  `(defmethod compile-expression [(language-name ~language) ::base-semantics] [~dl-expression]
     (fn [~model]
       ~@body)))

;;;

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

;;;

(defmulti most-specific-concept
  "Computes the model based most specific concept of a set of objects
  in a given model."
  (fn [model dl-exp] (language-name (model-language model))))

(defmethod most-specific-concept :default [model _]
  (illegal-argument "Language " (print-str (model-language model)) " does not provide msc."))

(defmacro define-msc
  "Defines model based most specific concepts for a language, a model
  and a set of objects."
  [language [model objects] & body]
  `(defmethod most-specific-concept (language-name ~language)
     [~model ~objects]
     ~@body))

(defn model-closure
  "Return the most specific concept of the interpretation of dl-exp in
  model."
  [model dl-exp]
  (most-specific-concept model (interpret model dl-exp)))

;;;

(defn extend-model
  "Extends model by given interpretation function i. i should return
  nil if it doesn't change a value of model's original interpretion,
  where this original interpretation is used."
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

(defn context->model
  "Returns for a given context ctx and a given description logic dl a
  model given by the incidence relation of ctx. Note that the set of
  attributes must be the set of primitive concepts of dl together with
  a set of element of the form (r x), where r is a role name of dl and
  y incident with (r x) shall mean that (x y) is in the interpretation
  of r in the resulting model."
  [dl ctx]
  (let [base-set       (objects ctx),
        interpretation (loop [result {},
                              attributes (attributes ctx)]
                         (if (empty? attributes)
                           result
                           (let [next (first attributes)]
                             (if (and (seq? next)
                                      (= 2 (count next))
                                      (contains? (role-names dl) (first next)))
                               (let [role (first next),
                                     x    (second next)]
                                 (recur (assoc result role
                                               (into (get result role #{})
                                                     (for [y (attribute-derivation ctx #{next})]
                                                       [x y])))
                                        (rest attributes)))
                               (recur (assoc result next
                                             (attribute-derivation ctx #{next}))
                                      (rest attributes))))))]
    (make-model dl base-set interpretation)))

;;; TBox interpretations

(defn next-interpretation
  "Defines a new interpretation on the defined concepts of tbox in
  model through interpretation."
  [model tbox interpretation]
  (let [extended-model (extend-model model interpretation)]
    (loop [defs  (seq (tbox-definitions tbox)),
	   new-i {}]
      (if (empty? defs)
	new-i
	(recur (rest defs)
	       (let [def (first defs)]
		 (conj new-i [(definition-target def)
			      (interpret extended-model (definition-expression def))])))))))

(defn fixed-point
  "Apply f to data until (= old-data new-data)."
  [f data]
  (let [runner (fn runner [old-data]
		 (let [new-data (f old-data)]
		   (if (= new-data old-data)
		     new-data
		     (recur new-data))))]
    (runner data)))

(defn constant-interpretation
  "Return interpretation on the defined concepts of tbox, constantly
  returning value."
  [tbox value]
  (hashmap-by-function (constantly value)
		       (defined-concepts tbox)))

(defn gfp-model
  "Returns the gfp-model of tbox in model."
  [tbox model]
  (fixed-point (fn [i]
		 (next-interpretation model tbox i))
	       (constant-interpretation tbox (model-base-set model))))

(defn lfp-model
  "Returns the lfp-model of tbox in model."
  [tbox model]
  (fixed-point (fn [i]
		 (next-interpretation model tbox i))
	       (constant-interpretation tbox #{})))

;;;

nil
