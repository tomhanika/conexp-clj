;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.exploration.util
  "Some utility functions for exploration."
  (:use conexp.base
        conexp.fca.implications))

;;;

(defn falsifies-implication?
  "Returns true iff set of new attributes does not respect implication impl."
  [new-atts impl]
  (and (subset? (premise impl) new-atts)
       (not (subset? (conclusion impl) new-atts))))

(defn examples-by-automorphisms
  "Generates for the given context ctx and a given example row [g
  atts] a sequence of new examples (as rows of the same form) by
  applying the context automorphism in auts. The context automorphisms
  are applied to the attributes in atts only, the corresponding object
  will be a newly generated."
  [ctx [g atts] auts]
  (distinct (conj (map (fn [alpha]
                         [(gensym (str g "-")), (set-of (alpha m) [m atts])])
                       auts)
                  [g atts])))

(defn saturate-partial-example
  "Saturates the partial example given by positives, negatives and
  unknown. Uses the set impls of implications for saturation."
  [impls positives negatives unknown]
  (let [clop    (clop-by-implications impls),
        new-pos (clop positives)]
    (when (exists [n negatives] (contains? new-pos n))
      (illegal-argument "Example given contradicts already known facts."))
    [new-pos,
     (union negatives
            (set-of ? [? unknown,
                       :when (seq (intersection negatives
                                                (clop (conj positives ?))))]))]))

;;;

nil
