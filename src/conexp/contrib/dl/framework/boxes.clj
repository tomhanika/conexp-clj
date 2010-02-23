;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.framework.boxes
  (:use conexp
	conexp.contrib.dl.framework.syntax))

;;; TBox definitions

(deftype TBox [language definitions])

(defn tbox-language
  "Returns language for which tbox is a tbox."
  [tbox]
  (:language tbox))

(defn tbox-definitions
  "Returns the definitions in a tbox."
  [tbox]
  (:definitions tbox))

(defmethod print-method ::TBox [tbox out]
  (.write out (with-out-str
		(doseq [def (tbox-definitions tbox)]
		  (println def)))))

(defn make-tbox
  "Creates and returns a tbox for language from the given
  definitions."
  [language definitions]
  (TBox language definitions))

;;; accessing used role names, primitive and defined concepts

(defn defined-concepts
  "Returns all defined concepts in tbox."
  [tbox]
  (set-of (definition-target def) [def (tbox-definitions tbox)]))

(defn used-role-names
  "Returns all used role names in tbox."
  [tbox]
  (apply union #{}
	 (map #(role-names-in-expression (definition-expression %))
	      (tbox-definitions tbox))))

(defn used-concept-names
  "Returns all used concept names in tbox."
  [tbox]
  (apply union #{}
	 (map #(concept-names-in-expression (definition-expression %))
	      (tbox-definitions tbox))))

;;;

(defmacro define-tbox
  "Defines a TBox. Definitions are names interleaved with dl-sexps."
  [name language & definitions]
  (let [definitions (partition 2 definitions)]
  `(def ~name (make-tbox ~language
			 (set (for [pair# '~definitions]
				(make-dl-definition (first pair#)
						    (make-dl-expression ~language (second pair#)))))))))

;;;

nil
