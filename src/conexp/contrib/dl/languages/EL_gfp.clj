;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.languages.EL-gfp
  (:use conexp
	conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.framework.models
	conexp.contrib.dl.framework.boxes
	conexp.contrib.dl.languages.description-graphs
	conexp.contrib.dl.framework.semantics
	conexp.contrib.dl.framework.reasoning))

;;;

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

(define-msc EL-gfp
  [model objects]
  (let [[tbox target] (reduce-tbox (apply EL-gfp-msc model objects))]
    (if (acyclic? tbox)
      (definition-expression (first (tbox-definitions tbox)))
      [tbox target])))

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

(define-subsumption EL-gfp
  [C D]
  (let [[C-tbox C-target] (uniquify-tbox-target-pair (expression (ensure-EL-gfp-concept C))),
	[D-tbox D-target] (uniquify-tbox-target-pair (expression (ensure-EL-gfp-concept D))),

	tbox (tbox-union C-tbox D-tbox),
	G (tbox->description-graph tbox)]
    (simulates? G G D-target C-target)))

;;;

nil
