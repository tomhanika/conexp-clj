;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.languages.description-graphs
  (:use conexp
	conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.framework.models
	conexp.contrib.dl.framework.boxes))

;;;

(deftype Description-Graph [language vertices neighbours vertex-labels])

(defn vertices
  "Returns vertices of given description graph."
  [description-graph]
  (:vertices description-graph))

(defn neighbours
  "Returns a function mapping vertices to sets of pairs of roles and
  names."
  [description-graph]
  (:neighbours description-graph))

(defn vertex-labels
  "Returns vertex labeling function of given description graph."
  [description-graph]
  (:vertex-labels description-graph))

(defn graph-language
  "Returns the underlying description language of the given
  description graph."
  [description-graph]
  (:language description-graph))

(defn make-description-graph
  "Creates and returns a description graph for the given arguments."
  [language vertices neighbours vertex-labels]
  (Description-Graph language vertices neighbours vertex-labels))

(defn edges
  "Returns labeld edges of given description graph."
  [description-graph]
  (set-of [A r B] [A (vertices description-graph),
		   [r B] ((neighbours description-graph) A)]))

(defmethod print-method ::Description-Graph [dg out]
  (.write out "Description Graph"))

;;;

(defn normalize-tbox
  "Normalizes given tbox."
  [tbox]
  'to-be-done)

(defn tbox->description-graph
  "Converts a tbox to a description graph."
  [tbox]
  'to-be-done)

(defn description-graph->tbox
  "Converts a description graph to a tbox."
  [description-graph]
  (let [language    (graph-language description-graph),
	labels      (vertex-labels description-graph),
	neighbours  (neighbours description-graph),

	definitions (set-of (make-dl-definition A def-exp)
			    [A (vertices description-graph)
			     :let [def-exp (make-dl-expression language
							       (list* 'and
								      (concat (labels A)
									      (for [[r B] (neighbours A)]
										(list 'exists r B)))))]])]
    (make-tbox language definitions)))

(defn model->description-graph
  "Converts given model to a description graph."
  [model]
  (let [language            (model-language model),
	base-interpretation (model-base-interpretation model),

	vertices            (model-base-set model),
	neighbours          (hashmap-by-function (fn [x]
						   (set-of [r y] [r (role-names language),
								  y (model-base-set model),
								  :when (contains? (base-interpretation r) [x y])]))
						 (model-base-set model)),
	vertex-labels       (hashmap-by-function (fn [x]
						   (set-of P [P (concept-names language),
							      :when (contains? (base-interpretation P) x)]))
						 (model-base-set model))]
    (make-description-graph language vertices neighbours vertex-labels)))

;;;

nil
