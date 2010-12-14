;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.util.concept-sets
  (:use conexp.main
        conexp.contrib.util.general-sorted-sets
        conexp.contrib.dl.framework.syntax
        conexp.contrib.dl.framework.reasoning))

(ns-doc
 "Implements a orderd set type for concepts with no two elements of
  the set being equivalent.")

;;;

(deftype Concept-Set [gss seq-of-concepts]
  clojure.lang.Seqable
  (seq [this]
    @seq-of-concepts))

(defn- gss-of-concept-set
  "Returns the general-sorted-set of a concept-set."
  [^Concept-Set concept-set]
  (.gss concept-set))

(defn- seq-of-concepts
  "Returns the sequence of concepts added to concept-set, in the
  ordere they have been added, latest first."
  [^Concept-Set concept-set]
  (.seq-of-concepts concept-set))

(defn make-concept-set
  "Creats a concept-set from the given sequence of concepts. The
  elements in coll will be added from left to right."
  [coll]
  (let [gss (make-general-sorted-set subsumed-by?)]
    (loop [coll coll,
           inserted ()]
      (cond
       (empty? coll) (Concept-Set. gss (ref inserted)),
       (contained-in-gss? gss (first coll)) (recur (rest coll) inserted),
       :else (do
               (add-to-gss! gss (first coll))
               (recur (rest coll) (conj inserted (first coll))))))))

(defmethod print-method Concept-Set [concept-set out]
  (print-method (seq concept-set) out))

;;;

(defn add-concept!
  "Adds given concept to concept-set."
  [concept-set concept]
  (when-not (contained-in-gss? (gss-of-concept-set concept-set) concept)
    (add-to-gss! (gss-of-concept-set concept-set) concept)
    (dosync (alter (seq-of-concepts concept-set) conj concept)))
  concept-set)

(defn add-concepts!
  "Extends concept set by concepts and returns the result."
  [concept-set & concepts]
  (doseq [concept concepts]
    (add-concept! concept-set concept))
  concept-set)

(defn minimal-implication-set
  "Returns a minimal set of implications representing the subsumptions
  in concept-set."
  [concept-set]
  (set-of (make-implication #{C} #{D})
          [[C D] (hasse-graph (gss-of-concept-set concept-set))]))

;;;

nil
