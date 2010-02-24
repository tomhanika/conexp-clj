;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.base
  (:use conexp))

(update-ns-meta! conexp.contrib.dl.base
  :doc "Some experiments with description logics.")

;;;

;;;

(defmulti subsumption
  "Implements subsumption for a given language."
  (fn [language C D] language))

(defmethod subsumption :default [language C D]
  (illegal-argument "Subsumption not defined for language " language))

(defmacro define-subsumption
  "Defines an algorithm for a DL language and two concept descriptions
  C and D."
  [[language C D] & body]
  `(defmethod subsumption ~language [~language ~C ~D]
     ~@body))

(defn subsumed?
  "Returns true iff C is subsumed by D in language."
  [language C D]
  (subsumption language C D))
  
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

(deftype Subsumption [C D])

(defn make-subsumption
  "Creates and returns a subsumption."
  [C D]
  (Subsumption C D))

;;;

nil
