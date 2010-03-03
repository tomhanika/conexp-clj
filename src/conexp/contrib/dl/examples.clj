;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.examples
  (:use conexp
	conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.framework.models
	conexp.contrib.dl.framework.boxes
	conexp.contrib.dl.framework.exploration
	conexp.contrib.dl.framework.interaction
	conexp.contrib.dl.languages.description-graphs
	conexp.contrib.dl.framework.semantics
	conexp.contrib.dl.framework.reasoning))

;;;

(define-dl SimpleDL [Father Mother Male Female] [Child] [exists and])

(def dl-exp (dl-expression SimpleDL (exists Child Male)))

(define-model some-model SimpleDL
  #{John Marry Peter Jana}
  Mother #{Marry},
  Father #{John, Peter},
  Male   #{John, Peter},
  Female #{Marry, Jana},
  Child  #{[John Peter], [Marry Peter], [Peter Jana]})

(define-tbox some-tbox SimpleDL
  Grandfather (and Male (exists Child (exists Child (and))))
  Grandmother (and Female (exists Child (exists Child (and)))))

(define-tbox some-normal-tbox SimpleDL
  A (and Male Father (exists Child B)),
  B (and Female (exists Child T)),
  T (and))

(define-base-semantics SimpleDL
  [model dl-expression]
  ;; quit, if dl-expression is not a tbox with target
  (when (not (and (vector? (expression dl-expression))
		  (= 2 (count (expression dl-expression)))))
    (illegal-argument "No base semantics defined for " (print-str dl-expression) "."))
  ;; compute gfp-model and interpret target
  (let [[tbox, target] (expression dl-expression),
	interpretation (gfp-model tbox model)]
    (interpretation target)))

(def ext-dl-exp (dl-expression SimpleDL [some-tbox, Grandfather]))
(def ext-dl-exp-2 (dl-expression SimpleDL (and [some-tbox, Grandfather])))

(define-msc SimpleDL
  [model objects]
  (let [[tbox target] (reduce-tbox (apply EL-gfp-msc model objects))]
    (if (acyclic? tbox)
      (definition-expression (first (tbox-definitions tbox)))
      [tbox target])))

(define-model paper-model SimpleDL
  [John Michelle Mackenzie Paul Linda James]
  Male   #{John Paul James}
  Female #{Michelle Mackenzie Linda}
  Father #{John Paul}
  Mother #{Michelle Linda}
  Child  #{[John Mackenzie] [Michelle Mackenzie]
	   [Paul James] [Linda James]})

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

(define-subsumption SimpleDL
  [C D]
  (let [[C-tbox C-target] (expression (ensure-EL-gfp-concept C)),
	[D-tbox D-target] (expression (ensure-EL-gfp-concept D)),

	G (tbox->description-graph (tbox-union C-tbox D-tbox))]
    (simulates? G G D-target C-target)))

;;;

nil
