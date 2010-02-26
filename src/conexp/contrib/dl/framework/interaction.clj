;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.framework.interaction
  (:use conexp
	conexp.contrib.dl.framework.syntax))

;;;

(defn expert-refuses?
  "Asks an expert (the user) whether a given subsumption is to be refuted or not."
  [subsumption]
  ;; stupid implementation for testing right now
  false)

(defn extend-model-by-contradiction
  "Extends given model by asking an expert (the user) to extend model
  to a connected supermodel in which the given subsumption does not
  hold."
  [model subsumption]
  (throw (UnsupportedOperationException. (str "extend-model-by-contradiction not yet implemented"))))

;;;

nil