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
	conexp.contrib.dl.framework.semantics
	conexp.contrib.dl.framework.boxes
	conexp.contrib.dl.framework.reasoning
	conexp.contrib.dl.languages.description-graphs
        conexp.contrib.dl.languages.EL-gfp-rewriting))

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
  (let [[tbox, target] (expression-term dl-expression)]
    (EL-gfp-model-interpretation model [tbox target])))


;;; subsumption

(defn ensure-EL-gfp-concept
  "Ensures dl-expression to be a pair of a tbox and a target."
  [dl-expression]
  (let [expr (expression-term dl-expression)]
    (if (and (vector? expr)
	     (= 2 (count expr)))
      dl-expression
      (let [language (expression-language dl-expression),
	    target   (gensym)]
	(make-dl-expression-nc language
			       [(make-tbox language
					   {target (make-dl-definition target dl-expression)}),
				target])))))

(defn EL-expression->rooted-description-graph
  "Returns for a given EL expression or a tbox-target-pair its
  description graph together with the root corresponding to the
  target."
  [dl-expr]
  (let [[tbox target] (expression-term (ensure-EL-gfp-concept dl-expr))]
    [(tbox->description-graph tbox), target]))

(define-subsumption EL-gfp
  [C D]
  (let [[G-C C-target] (EL-expression->rooted-description-graph C)
	[G-D D-target] (EL-expression->rooted-description-graph D)]
    (simulates? G-D G-C D-target C-target)))

;;; lcs and msc

(defn EL-gfp-lcs
  "Returns the least common subsumer (in EL-gfp) of A and B in tbox."
  [tbox concepts]
  (when (empty? concepts)
    (illegal-argument "EL-gfp-lcs called with no concepts."))
  (loop [tbox tbox,
         concepts concepts]
    (if (= 1 (count concepts))
      [tbox (first concepts)]
      (let [A     (first concepts),
            B     (second concepts),
            G_T_A (tbox->description-graph (first (clarify-ttp [tbox, A])))
            G_T_B (tbox->description-graph (first (clarify-ttp [tbox, B])))
            G-x-G (graph-product G_T_A G_T_B),
            T_2   (description-graph->tbox G-x-G),
            [new-tbox new-target] (uniquify-ttp (clarify-ttp (tidy-up-ttp (clarify-ttp [T_2, [A,B]]))))]
        (recur (tbox-union tbox new-tbox) (conj (vec (drop 2 concepts)) new-target))))))

(defn EL-gfp-msc
  "Returns the model based most specific concept of objects in model."
  [model objects]
  (if-not (empty? objects)
    (EL-gfp-lcs (model->tbox model) objects)
    (let [language (model-language model),
	  all (make-dl-expression language
				  (list* 'and
					 (concat (concept-names language)
						 (for [r (role-names language)]
						   (list 'exists r 'All)))))]
      [(make-tbox language {'All (make-dl-definition 'All all)}), 'All])))

(define-msc EL-gfp
  [model objects]
  (let [[tbox target] (normalize-EL-gfp-term
                       (reduce-ttp (tidy-up-ttp (clarify-ttp (EL-gfp-msc model objects)))))]
    (if (acyclic? tbox)
      (definition-expression (first (tbox-definitions tbox)))
      (make-dl-expression (model-language model) [tbox target]))))

;;;

nil
