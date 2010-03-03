;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.framework.boxes
  (:use [conexp :exclude (transitive-closure)]
	conexp.contrib.dl.framework.syntax)
  (:use clojure.contrib.pprint
	clojure.contrib.graph))

;;; TBox definitions

(deftype TBox [language definitions])

(defn tbox-language
  "Returns language for which tbox is a tbox."
  [tbox]
  (:language tbox))

(defn tbox-definitions
  "Returns the definitions in a tbox."
  [tbox]
  (:definitions tbox))

(defmethod print-method ::TBox [tbox out]
  (let [#^String output (with-out-str (pprint (tbox-definitions tbox)))]
    (.write out (.trim output))))

(defn make-tbox
  "Creates and returns a tbox for language from the given
  definitions."
  [language definitions]
  (TBox language (set definitions)))

(defn tbox?
  "Returns true iff thing is a tbox."
  [thing]
  (= (type thing) ::TBox))

(defn tbox-target-pair?
  "Returns true iff dl-expr is a tbox-target-pair."
  [dl-expr]
  (and (dl-expression? dl-expr)
       (let [expr (expression dl-expr)]
	 (and (vector? expr)
	      (= 2 (count expr))
	      (tbox? (first expr))
	      (contains? (defined-concepts (first expr)) (second expr))))))

;;; accessing used role names, primitive and defined concepts

(defn defined-concepts
  "Returns all defined concepts in tbox."
  [tbox]
  (set-of (definition-target def) [def (tbox-definitions tbox)]))

(defn used-role-names
  "Returns all used role names in tbox."
  [tbox]
  (apply union #{}
	 (map #(role-names-in-expression (definition-expression %))
	      (tbox-definitions tbox))))

(defn used-concept-names
  "Returns all used concept names in tbox."
  [tbox]
  (apply union #{}
	 (map #(concept-names-in-expression (definition-expression %))
	      (tbox-definitions tbox))))

;;;

(defmacro define-tbox
  "Defines a TBox. Definitions are names interleaved with dl-sexps."
  [name language & definitions]
  (let [definitions (partition 2 definitions)]
  `(def ~name (make-tbox ~language
			 (for [pair# '~definitions]
			   (make-dl-definition (first pair#)
					       (make-dl-expression ~language (second pair#))))))))

;;;

(defn find-definition
  "Returns definition of target A in tbox, if it exists."
  [tbox A]
  (let [result (first (filter #(= A (definition-target %))
			      (tbox-definitions tbox)))]
    (if (nil? result)
      (illegal-argument "Cannot find definition for " A " in tbox " (print-str tbox) ".")
      result)))

(defn uniquify-tbox-target-pair
  "Substitutes for every defined concept name in tbox a new, globally
  unique, concept name and finally substitutes traget with its new name."
  [[tbox target]]
  (let [symbols     (defined-concepts tbox),
	new-symbols (hashmap-by-function (fn [sym]
					   (gensym (str sym)))
					 symbols)]
    [(make-tbox (tbox-language tbox)
		(set-of (make-dl-definition (new-symbols target) (substitute def-exp new-symbols))
			[def (tbox-definitions tbox),
			 :let [target (definition-target def),
			       def-exp (definition-expression def)]]))
     (new-symbols target)]))

(defn uniquify-tbox
  "Substitutes for every defined concept anme in tbox a new, globally
  unique, concept name."
  [tbox]
  (if (empty? (tbox-definitions tbox))
    tbox
    (first (uniquify-tbox-target-pair [tbox (definition-target (first (tbox-definitions tbox)))]))))

(defn tbox-union
  "Returns the union of tbox-1 and tbox-2."
  [tbox-1 tbox-2]
  (let [tbox-1 (uniquify-tbox tbox-1),
	tbox-2 (uniquify-tbox tbox-2)]
    (make-tbox (tbox-language tbox-1)
	       (union (tbox-definitions tbox-1)
		      (tbox-definitions tbox-2)))))

;;;

(defn- usage-graph
  "Returns usage graph of a given tbox."
  [tbox]
  (struct directed-graph
	  (defined-concepts tbox)
	  (fn [A]
	    (free-symbols-in-expression (definition-expression (find-definition tbox A))))))

(defn acyclic?
  "Returns true iff tbox is acyclic."
  [tbox]
  (zero? (count (self-recursive-sets (usage-graph tbox)))))

(defn collect-targets
  "Collects all targets reachable in the usage graph of tbox, starting
  from targets."
  [tbox targets seen]
  (if (empty? targets)
    seen
    (let [target  (first targets),
	  seen    (conj seen target),
	  targets (difference (into targets (free-symbols-in-expression
					     (definition-expression (find-definition tbox target))))
			      seen)]
      (recur tbox targets seen))))

(defn clarify-tbox
  "Clarifies tbox for target, i.e. removes all definitions from tbox
  which are not needed to define target."
  [[tbox target]]
  (let [needed-targets (collect-targets tbox #{target} #{})]
    [(make-tbox (tbox-language tbox)
		(for [def (tbox-definitions tbox)
		      :when (contains? needed-targets (definition-target def))]
		  def)),
     target]))

(defn substitute-definitions
  "Substitutes defined concepts in the definition of target by their
  definitions. Note that this function does not finish when the tbox is
  recursive in target."
  [[tbox target]]
  (let [symbols (free-symbols-in-expression (definition-expression (find-definition tbox target)))]
    (if (empty? symbols)
      [tbox target]
      (let [target-definition  (find-definition tbox target),
	    rest-definitions   (disj (tbox-definitions tbox) target-definition),
	    needed-definitions (for [def rest-definitions
				     :when (contains? symbols (definition-target def))]
				 [(definition-target def) (definition-expression def)]),

	    new-target-definition (make-dl-definition target
						      (substitute (definition-expression target-definition)
								  (into {} needed-definitions)))

	    [new-tbox target]  (clarify-tbox [(make-tbox (tbox-language tbox)
							 (conj rest-definitions new-target-definition))
					      target])]
	(recur [new-tbox target])))))

(defn reduce-tbox
  "Reduces tbox for target as much as possible, returning a pair of
  the reduced tbox and target."
  [[tbox target]]
  (let [c-tbox (clarify-tbox [tbox target])]
    (if (acyclic? (first c-tbox))
      (substitute-definitions c-tbox)
      c-tbox)))

;;;

nil
