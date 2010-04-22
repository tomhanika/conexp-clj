;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.languages.EL-gfp-exploration
  (:use conexp.main
	conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.framework.semantics
	conexp.contrib.dl.framework.reasoning
	conexp.contrib.dl.languages.interaction
	conexp.contrib.dl.languages.EL-gfp
	conexp.contrib.dl.languages.EL-gfp-rewriting
	conexp.contrib.dl.util.concept-sets)
  (:use clojure.contrib.pprint
	clojure.contrib.seq))

(update-ns-meta! conexp.contrib.dl.languages.EL-gfp-exploration
  :doc "Implements exploration for description logics EL and EL-gfp.")


;;; technical helpers

(defn- induced-context
  "Returns context induced by the set of concept descriptions and the
  given model."
  ([descriptions model]
     (induced-context descriptions model (make-context #{} #{} #{})))
  ([descriptions model old-context]
     (let [new-objects    (difference (model-base-set model)
                                      (objects old-context)),
           new-attributes (difference (set descriptions)
                                      (attributes old-context)),
           new-incidence  (union (set-of [x y] [y new-attributes,
                                                x (interpret model y)])
                                 (if (empty? new-objects)
                                   (incidence old-context)
                                   (set-of [x y] [y (attributes old-context),
                                                  x (interpret model y)])))]
       (make-context (union (objects old-context) new-objects)
                     (union (attributes old-context) new-attributes)
                     new-incidence))))

(defn- obviously-true?
  "Returns true iff the given subsumption is obviously true."
  [subsumption]
  (subsumed-by? (subsumee subsumption) (subsumer subsumption)))

;;; actual exploration algorithm

;;; BADBADBAD!!!
(defvar *collected-gcis* (ref []))

(defn explore-model
  "Model exploration algorithm."
  ([initial-model]
     (explore-model initial-model (concept-names (model-language initial-model))))
  ([initial-model initial-ordering]
     (with-memoized-fns [EL-expression->rooted-description-graph
			 interpret
			 model-closure
			 subsumed-by?]
       (let [language (model-language initial-model)]

	 (when (and (not= (set initial-ordering) (concept-names language))
		    (not= (count initial-ordering) (count (concept-names language))))
	   (illegal-argument "Given initial-ordering for explore-model must consist "
			     "of all concept names of the language of the given model."))

	 (loop [k     0,
		M_k   (make-concept-set (map #(dl-expression language %) initial-ordering)),
		K     (induced-context (seq-on M_k) initial-model),
		Pi_k  [],
		P_k   #{},
		model initial-model,
		implications #{},
		background-knowledge #{}]

	   (if (nil? P_k)
	     ;; then return set of implications
	     (let [implicational-knowledge (union implications background-knowledge)]
	       (for [P Pi_k
		     :let [all-P    (make-dl-expression language (cons 'and P)),
			   mc-all-P (make-dl-expression language (model-closure model all-P))]
		     :when (not (subsumed-by? all-P mc-all-P))
		     :let [susu (abbreviate-subsumption (make-subsumption all-P mc-all-P)
							implicational-knowledge)]
		     :when (not (empty? (arguments (subsumer susu))))]
		 susu))

	     ;; else search for next implication
	     (let [_ (println (count (seq-on M_k))),
                   all-P_k    (make-dl-expression language (cons 'and P_k)),
		   next-model (loop [model model]
				(let [susu (abbreviate-subsumption
                                            (make-subsumption all-P_k
                                                              (make-dl-expression language
                                                                                  (model-closure model all-P_k)))
                                            (union implications background-knowledge))]
				  (if (or (obviously-true? susu)
                                          (or (dosync (alter *collected-gcis* conj susu)) true)
					  (not (expert-refuses? susu)))
				    model
				    (recur (extend-model-by-contradiction model susu))))),
		   next-M_k   (apply add-concepts! M_k (for [r (role-names language)]
                                                         (dl-expression language
                                                                        (exists r (model-closure next-model all-P_k))))),
		   next-K     (induced-context (seq-on next-M_k) next-model K),
		   next-Pi_k  (conj Pi_k P_k),

		   implications (set-of impl [P_l next-Pi_k
					      :let [impl (make-implication P_l (context-attribute-closure next-K P_l))]
					      :when (not (empty? (conclusion impl)))]),
		   background-knowledge (minimal-implication-set next-M_k),

		   next-P_k   (next-closed-set (seq-on next-M_k)
					       (clop-by-implications (union implications background-knowledge))
					       P_k)]
	       (recur (inc k) next-M_k next-K next-Pi_k next-P_k next-model implications background-knowledge))))))))

;;; gcis

(defn model-gcis
  "Returns a complete and sound set of gcis holding in model. See
  explore-model for valid args."
  [model & args]
  (binding [expert-refuses? (constantly false)]
    (apply explore-model model args)))

;;;

nil
