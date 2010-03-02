;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.framework.exploration
  (:use conexp
	conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.framework.models
	conexp.contrib.dl.framework.interaction
	conexp.contrib.dl.framework.reasoning)
  (:use clojure.contrib.pprint))

(update-ns-meta! conexp.contrib.dl.framework.exploration
  :doc "Implements exploration for description logics EL and EL-gfp.")


;;;

(defn induced-context
  "Returns context induced by the set of concept descriptions and the
  given model."
  [descriptions model]
  (let [objects    (model-base-set model),
	attributes descriptions,
	incidence  (set-of [g m] [m attributes,
				  g (interpret model m)])]
    (make-context objects attributes incidence)))

(defn- arguments*
  "Returns the arguments of the given DL expression as set, if it is
  not atomic. If it is, returns the singleton set of the dl-expression
  itself."
  [dl-expression]
  (if (atomic? dl-expression)
    (set [dl-expression])
    (set (arguments dl-expression))))

(defn- abbreviate-subsumption
  "Takes a subsumption whose subsumer and subsumee are in normal form
  and returns a subsumption where from the subsumee every term already
  present in the subsumer is removed."
  [subsumption]
  (if (or (atomic? (subsumer subsumption))
	  (atomic? (subsumee subsumption)))
    subsumption
    (let [language (expression-language (subsumer subsumption)),
	  premise-args (arguments* (subsumer subsumption)),
	  conclusion-args (arguments* (subsumee subsumption))]
      (make-subsumption (make-dl-expression language (cons 'and premise-args))
			(make-dl-expression language (cons 'and (difference conclusion-args premise-args)))))))

(defn- clarify-subsumption-set
  "Removes all sumsumptions with equal subsumer and subsumee from the
  set of given subsumptions."
  [subs]
  (set-of susu
	  [susu (map abbreviate-subsumption (seq subs)),
	   :when (not (superset? (arguments* (subsumer susu))
				 (arguments* (subsumee susu))))]))

;;;

(defn explore-model
  "Model exploration algorithm."
  ;; This is algorithm 6
  [initial-model]
  (binding [model-closure (memoize model-closure),
	    subsumes?     (memoize subsumes?)]
    (let [language (model-language initial-model)]
      (loop [k     0,
	     M_k   (vec (map #(dl-expression language %)
			     (concept-names language))),
	     M_k-1 [],
	     K     (induced-context M_k initial-model),
	     Pi_k  [],
	     P_k   #{},
	     model initial-model]
	(if (nil? P_k)
	  ;; then return set of implications
	  (clarify-subsumption-set
	   (set-of (make-subsumption all-P mc-all-P)
		   [P Pi_k
		    :let [all-P (make-dl-expression language (cons 'and P)),
			  mc-all-P (make-dl-expression language (model-closure model all-P))]]))

	  ;; else search for next implication
	  (let [all-P_k    (make-dl-expression language (cons 'and P_k)),
		next-model (loop [model model]
			     (let [susu (make-subsumption all-P_k
							  (make-dl-expression language (model-closure model all-P_k)))]
			       (if-not (expert-refuses? susu)
				 model
				 (recur (extend-model-by-contradiction model susu))))),
		next-M_k   (into M_k (for [r (role-names language)]
				       (dl-expression language (exists r (model-closure next-model all-P_k))))),
		next-M_k-1 M_k,
		next-K     (induced-context next-M_k next-model),
		next-Pi_k  (conj Pi_k P_k),
		next-P_k   (if (= (set M_k) (set M_k-1) (set P_k))
			     nil
			     (next-closed-set M_k
					      (clop-by-implications
					       (union (set-of (make-implication P_l (context-attribute-closure next-K P_l))
							      [P_l (rest Pi_k)])
						      (set-of (make-implication #{C} #{D})
							      [C M_k, D M_k
							       :when (and (not= C D)
									  (subsumes? C D))])))
					      P_k))]
	    (recur (inc k) next-M_k next-M_k-1 next-K next-Pi_k next-P_k next-model)))))))

;;;

nil
