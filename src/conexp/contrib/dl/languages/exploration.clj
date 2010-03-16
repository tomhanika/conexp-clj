;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.languages.exploration
  (:use conexp
	conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.framework.models
	conexp.contrib.dl.languages.interaction
	conexp.contrib.dl.framework.reasoning)
  (:use clojure.contrib.pprint))

(update-ns-meta! conexp.contrib.dl.languages.exploration
  :doc "Implements exploration for description logics EL and EL-gfp.")


;;;

(defn- induced-context
  "Returns context induced by the set of concept descriptions and the
  given model."
  [descriptions model]
  (let [objects    (model-base-set model),
	attributes descriptions,
	incidence  (set-of [g m] [m attributes,
				  g (interpret model m)])]
    (make-context objects attributes incidence)))

(defn- obviously-true?
  "Returns true iff the given subsumption is obviously true."
  [subsumption]
  (subsumed-by? (subsumee subsumption) (subsumer subsumption)))

(defn- extend-attributes
  "Takes a sequence of concepts and a sequence of new concepts to be
  added to the first sequence. If any element in the new sequence is
  equivalent to some element in the old one, it is not added."
  [concepts new-concepts]
  (loop [concepts concepts,
	 new-concepts new-concepts]
    (if (empty? new-concepts)
      concepts
      (recur (let [next (first new-concepts)]
	       (if (some #(equivalent? next %) concepts)
		 concepts
		 (conj concepts next)))
	     (rest new-concepts)))))

;;; rewriting

(defn- arguments*
  "Returns the arguments of the given DL expression as set, if it is
  not atomic. If it is, returns the singleton set of the dl-expression
  itself."
  [dl-expression]
  (if (atomic? dl-expression)
    (set [dl-expression])
    (set (arguments dl-expression))))

(defn- implication-kernel
  "Given a set of concepts returns a minimal subset of concepts which,
  when closed under the given implications, yields a superset of the
  original set given."
  [set-of-concepts set-of-implications]
  (let [implication-closure (clop-by-implications set-of-implications)]
    (loop [to-consider (seq set-of-concepts),
	   concepts set-of-concepts]
      (if (empty? to-consider)
	concepts
	(let [next-concept (first to-consider)]
	  (recur (rest to-consider)
		 (if (contains? (implication-closure (disj concepts next-concept))
				next-concept)
		   (disj concepts next-concept)
		   concepts)))))))

(defn- abbreviate-subsumption
  "Takes a subsumption whose subsumee and subsumer are in normal form
  and returns a subsumption where from the subsumer every term already
  present in the subsumee is removed."
  [subsumption background-knowledge]
  (let [language (expression-language (subsumee subsumption)),
	premise-args (implication-kernel (arguments* (subsumee subsumption)) background-knowledge),
	conclusion-args (arguments* (subsumer subsumption))]
    (make-subsumption (make-dl-expression language (cons 'and premise-args))
		      (make-dl-expression language (cons 'and (difference conclusion-args premise-args))))))

(defn- clarify-subsumption-seq
  "Removes all sumsumptions with equal subsumee and subsumer from the
  seq of given subsumptions, abbreviating subsumptions with given
  backround knowledge (a set of implications between sets of
  concepts)."
  [subs background-knowledge]
  (map #(abbreviate-subsumption % background-knowledge)
       (filter #(not (obviously-true? %))
	       subs)))

;;; exploration

(defn explore-model
  "Model exploration algorithm."
  ([initial-model]
     (explore-model initial-model (concept-names (model-language initial-model))))
  ([initial-model initial-ordering]
     (binding [model-closure (memoize model-closure),
	       subsumed-by?  (memoize subsumed-by?)]
       (let [language (model-language initial-model)]
	 (when (and (not= (set initial-ordering) (concept-names language))
		    (not= (count initial-ordering) (count (concept-names language))))
	   (illegal-argument "Given initial-ordering for explore-model must consist "
			     "of all concept names of the language of the given model."))

	 (loop [k     0,
		M_k   (map #(dl-expression language %) initial-ordering),
		K     (induced-context M_k initial-model),
		Pi_k  [],
		P_k   #{},
		model initial-model,
		implications #{},
		background-knowledge #{}]
	   (if (nil? P_k)
	     ;; then return set of implications
	     (clarify-subsumption-seq
	      (for [P Pi_k
		    :let [all-P    (make-dl-expression language (cons 'and P)),
			  mc-all-P (make-dl-expression language (model-closure model all-P))]
		    :when (not= all-P mc-all-P)]
		(make-subsumption all-P mc-all-P))
	      (union implications background-knowledge))

	     ;; else search for next implication
	     (let [all-P_k    (make-dl-expression language (cons 'and P_k)),
		   next-model (loop [model model]
				(let [susu (make-subsumption all-P_k
							     (make-dl-expression language
										 (model-closure model all-P_k)))]
				  (if (or (obviously-true? susu)
					  (not (expert-refuses? susu)))
				    model
				    (recur (extend-model-by-contradiction model susu))))),
		   next-M_k   (extend-attributes M_k (set-of (dl-expression language
									    (exists r (model-closure next-model all-P_k)))
							     [r (role-names language)])),
		   next-K     (induced-context next-M_k next-model),
		   next-Pi_k  (conj Pi_k P_k),

		   implications (set-of impl
					[P_l next-Pi_k
					 :let [impl (make-implication P_l (context-attribute-closure next-K P_l))]
					 :when (not (empty? (conclusion impl)))])
		   background-knowledge (set-of (make-implication #{C} #{D})
						[C next-M_k, D next-M_k
						 :when (and (not= C D) (subsumed-by? C D))])

		   next-P_k   (next-closed-set next-M_k
					       (clop-by-implications (union implications background-knowledge))
					       P_k)]
	       (recur (inc k) next-M_k next-K next-Pi_k next-P_k next-model implications background-knowledge))))))))

;;; gcis

(defn model-gcis
  "Returns a complete and sound set of gcis holding in model."
  [model & args]
  (binding [expert-refuses? (constantly false)]
    (apply explore-model model args)))

;;;

nil
