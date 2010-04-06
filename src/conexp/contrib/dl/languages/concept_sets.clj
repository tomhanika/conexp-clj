;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.languages.concept-sets
  (:use conexp.main
	conexp.contrib.dl.util.general-sorted-sets
	conexp.contrib.dl.framework.reasoning)
  (:use [clojure.contrib.seq :only (seq-on)]))

(update-ns-meta! conexp.contrib.dl.languages.concept-sets
  :doc "Implements a orderd set type for concepts with no two elements
  of the set being equivalent.")

;;;

(deftype Concept-Set [gss seq-of-concepts])

(defn- gss-of-concept-set
  "Returns the general-sorted-set of a concept-set."
  [concept-set]
  (:gss concept-set))

(defn- seq-of-concepts
  "Returns the sequence of concepts added to concept-set, in the
  ordere they have been added, latest first."
  [concept-set]
  (:seq-of-concepts concept-set))

(defn make-concept-set
  "Creats a concept-set from the given sequence of concepts. The
  elements in coll will be added from right to left."
  [coll]
  (let [gss (make-general-sorted-set subsumed-by?)]
    (doseq [x (reverse coll)]
      (add-to-gss! gss x))
    (Concept-Set gss (ref (seq coll)))))


;;;

nil
