;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.interpretations
  (:use conexp
	conexp.contrib.dl.base))

;;;

(defmulti most-specific-concept
  "Computes the model based most specific concept of a set of objects
  in a given model."
  (fn [model dl-exp] (model-language model)))

(defmethod most-specific-concept :default [model _]
  (illegal-argument "Language " (model-language model) " does not provide msc."))

(defmacro define-msc
  "Defines model based most specific concepts for a language, a model
  and a set of objects."
  [[language model objects] & body]
  `(defmethod most-specific-concept ~language
     [~model ~objects]
     ~@body))

(defn model-closure
  "Return the most specific concept of the interpretation of dl-exp in
  model."
  [model dl-exp]
  (most-specific-concept model (interpret model dl-exp)))

;;;

nil

