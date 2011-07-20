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

(ns-doc "Defines EL-gfp with lcs, msc and subsumption.")

;;; EL-gfp

(define-dl EL-gfp [] [] [exists and])

(defn EL-gfp-model-interpretation
  "For a given tbox-target-pair returns the interpretation of the
  target in the gfp-model of tbox in model."
  [interpretation [tbox target]]
  (let [tbox-graph  (tbox->description-graph tbox),
        inter-graph (interpretation->description-graph interpretation)]
    ((efficient-simulator-sets tbox-graph inter-graph) target)))

(define-base-semantics EL-gfp
  [interpretation dl-expression]
  ;; quit, if dl-expression is not a tbox with target
  (when (not (tbox-target-pair? dl-expression))
    (illegal-argument "No base semantics defined for " (print-str dl-expression) "."))
  ;; compute gfp-model and interpret target
  (let [[tbox, target] (expression-term dl-expression)]
    (EL-gfp-model-interpretation interpretation [tbox target])))


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
  (loop [new-tbox tbox,
         concepts (sort-by #(let [dl-expr (definition-expression (find-definition tbox %))]
                              (count (filter compound? (arguments dl-expr))))
                           concepts)]
    (if (= 1 (count concepts))
      [new-tbox (first concepts)]
      (let [A          (first concepts),
            B          (second concepts),
            [tbox-A A] (clarify-ttp (tidy-up-ttp (clarify-ttp [new-tbox A]))),
            [tbox-B B] (clarify-ttp (tidy-up-ttp (clarify-ttp [tbox B]))),
            G_T_A      (tbox->description-graph tbox-A),
            G_T_B      (tbox->description-graph tbox-B),
            G-x-G      (graph-product G_T_A G_T_B [A,B]),
            [T target] (uniquify-ttp [(description-graph->tbox G-x-G) [A,B]])]
        (if (= #{}
               (set ((vertex-labels G-x-G) [A,B]))
               (set ((neighbours G-x-G) [A,B])))
          [T target]
          (recur T (conj (nthnext concepts 2) target)))))))

(defn EL-gfp-msc
  "Returns the model based most specific concept of objects in model."
  [model objects]
  (if (not-empty objects)
    (let [tbox (interpretation->tbox model)]
      (EL-gfp-lcs tbox objects))
    (let [language (interpretation-language model),
          all      (make-dl-expression language
                                       (list* 'and
                                              (concat (concept-names language)
                                                      (for [r (role-names language)]
                                                        (list 'exists r 'All)))))]
      [(make-tbox language {'All (make-dl-definition 'All all)}), 'All])))

(define-msc EL-gfp
  [model objects]
  (let [[tbox target] (-> (EL-gfp-msc model objects)
                          clarify-ttp
                          tidy-up-ttp
                          reduce-ttp
                          normalize-EL-gfp-term)]
    (if (acyclic? tbox)
      (definition-expression (first (tbox-definitions tbox)))
      (make-dl-expression-nc (interpretation-language model) [tbox target]))))

;;;

nil
