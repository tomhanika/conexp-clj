;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.languages.EL-gfp
  (:use conexp.main
	conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.framework.models
	conexp.contrib.dl.framework.boxes
	conexp.contrib.dl.languages.description-graphs
	conexp.contrib.dl.framework.semantics
	conexp.contrib.dl.framework.reasoning))

(update-ns-meta! conexp.contrib.dl.languages.EL-gfp
  :doc "Defines EL-gfp with lcs, msc and subsumption.")

;;; EL-gfp

(define-dl EL-gfp [] [] [exists and])

(define-base-semantics EL-gfp
  [model dl-expression]
  ;; quit, if dl-expression is not a tbox with target
  (when (not (tbox-target-pair? dl-expression))
    (illegal-argument "No base semantics defined for " (print-str dl-expression) "."))
  ;; compute gfp-model and interpret target
  (let [[tbox, target] (expression dl-expression),
	interpretation (gfp-model tbox model)]
    (interpretation target)))

;;; subsumption

(defn ensure-EL-gfp-concept
  "Ensures dl-expression to be a pair of a tbox and a target."
  [dl-expression]
  (let [expr (expression dl-expression)]
    (if (and (vector? expr)
	     (= 2 (count expr)))
      dl-expression
      (let [language (expression-language dl-expression),
	    target   (gensym)]
	(make-dl-expression-nc language
			       [(make-tbox language
					   #{(make-dl-definition target dl-expression)}),
				target])))))

(defn EL-expression->rooted-description-graph
  "Returns for a given EL expression or a tbox-target-pair its
  description graph together with the root corresponding to the
  target."
  [dl-expr]
  (let [[tbox target] (expression (ensure-EL-gfp-concept dl-expr))]
    [(tbox->description-graph tbox), target]))

(define-subsumption EL-gfp
  [C D]
  (let [[G-C C-target] (EL-expression->rooted-description-graph C)
	[G-D D-target] (EL-expression->rooted-description-graph D)]
    (simulates? G-D G-C D-target C-target)))

;;; lcs and msc

(defn EL-gfp-lcs
  "Returns the least common subsumer (in EL-gfp) of A and B in tbox."
  ([tbox A]
     [tbox A])
  ([tbox A B]
     (let [G_T_1 (tbox->description-graph tbox)
	   G-x-G (graph-product G_T_1 G_T_1),
	   T_2   (tbox-union tbox (description-graph->tbox G-x-G))]
       (clarify-tbox (tidy-up-tbox (clarify-tbox [T_2, [A,B]])))))
  ([tbox A B & more]
     (let [[new-tbox new-target] (EL-gfp-lcs tbox A B)]
       (apply EL-gfp-lcs (tbox-union tbox new-tbox) new-target more))))

(defn EL-gfp-object-msc
  "Returns the model based most specific concept of x in model."
  [model x]
  (clarify-tbox
   [(description-graph->tbox (model->description-graph model)), x]))

(defn EL-gfp-msc
  "Returns the model based most specific concept of args in model."
  [model & args]
  (if-not (empty? args)
    (let [tbox (reduce tbox-union
		       (map (comp first (partial EL-gfp-object-msc model))
			    args))]
      (apply EL-gfp-lcs tbox args))
    (let [language (model-language model),
	  all (make-dl-expression language
				  (list* 'and
					 (concat (concept-names language)
						 (for [r (role-names language)]
						   (list 'exists r 'All)))))]
      [(make-tbox language #{(make-dl-definition 'All all)}), 'All])))

(define-msc EL-gfp
  [model objects]
  (let [[tbox target] (reduce-tbox (apply EL-gfp-msc model objects))]
    (if (acyclic? tbox)
      (definition-expression (first (tbox-definitions tbox)))
      [tbox target])))

;;;

nil
