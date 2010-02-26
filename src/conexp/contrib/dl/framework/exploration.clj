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
	conexp.contrib.dl.framework.interaction))

(update-ns-meta! conexp.contrib.dl.exploration
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

;;;

(comment

  (defn algorithm-5
    "Model exploration algorithm without background knowledge."
    [language initial-model]
    (loop [k     0,
	   M_k   (vec (concept-names language)),
	   M_k-1 [],
	   K     (induced-context M_k working-model),
	   Pi_k  [],
	   P_k   #{},
	   model initial-model]
      (if (nil? P_k)
	;; return set of implications
	(set-of (make-subsumption P (model-closure model P))
		[P Pi_k])

	;; serach for next implication
	(let [all-P_k    (make-dl-expression language (cons 'and P_k)),
	      next-model (loop [model model]
			   (let [susu (make-subsumption all-P_k (model-closure model all-P_k))]
			     (if-not (expert-refuses? susu)
			       model
			       (recur (extend-model-by-contradiction model susu))))),
	      next-M_k   (into M_k (for [r (role-names language)]
				     (dl-expression (exists r (model-closure next-model all-P_k))))),
	      next-M_k-1 M_k,
	      next-K     (induced-context M_k next-model),
	      next-Pi_k  (conj Pi_k P_k),
	      next-P_k   (if (= M_k M_k-1)
			   nil
			   (next-closed-set M_k
					    (clop-by-implications
					     (set-of (make-implication P_l (context-attribute-closure next-K P_l))
						     [P_l (rest Pi_k)]))
					    P_k))]
	  (recur (inc k) next-M_k next-M_k-1 next-K next-Pi_k next-P_k next-model)))))

  (defn algorithm-6
    "Model exploration algorithm using background knowledge."
    [language initial-model]
    'to-be-done)
  
  'end-comment)

;;;

nil
