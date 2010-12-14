;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.languages.EL-gfp-rewriting
  (:use conexp.main
        conexp.contrib.dl.framework.syntax
        conexp.contrib.dl.framework.boxes
        conexp.contrib.dl.languages.description-graphs)
  (:use [clojure.walk :only (walk)]))

(ns-doc "Provides functions for EL-gfp term rewriting.")

;;; EL and EL-gfp normalization

(defn- minimal-elements
  "For a given sequence sqn of elements and an order relation
  more-specific?, return all minimal elements of sqn wrt
  more-specific?."
  [sqn more-specific?]
  (loop [collected (),
         left sqn]
    (if (empty? left)
      (reverse collected)
      (let [next (first left)]
        (if (some #(more-specific? % next) collected)
          (recur collected (rest left))
          (recur (conj (remove #(more-specific? next %) collected) next) (rest left)))))))

(defn- more-specific?
  "Returns true iff term-1 is more specific than term-2. When no tests succeeds
  (fallback term-1 term-2) is called and its value is returned."
  ([term-1 term-2]
     (more-specific? term-1 term-2 (constantly false)))
  ([term-1 term-2 fallback]
     (or (let [atom?   #(not (seq? %)),
               and?    #(and (seq? %) (= 'and (first %))),
               exists? #(and (seq? %) (= 'exists (first %)))]
           (cond
            (= term-1 term-2) true,

            (and (atom? term-1) (atom? term-2)) false,

            (and (and? term-1)
                 (not (and? term-2)))
            (or (some #(more-specific? % term-2 fallback) (rest term-1))
                false),

            (and (not (and? term-1))
                 (and? term-2))
            (more-specific? (list 'and term-1) term-2),

            (and (and? term-1) (and? term-2))
            (every? #(more-specific? term-1 % fallback) (rest term-2)),

            (and (exists? term-1) (exists? term-2))
            (and (= (second term-1) (second term-2))
                 (more-specific? (nth term-1 2) (nth term-2 2) fallback)),

            :else false))
         (fallback term-1 term-2))))

(defn normalize-EL-gfp-term
  "Normalizes a given EL-gfp term."
  [term]
  (cond
   (and (vector? term)
        (= 2 (count term))
        (tbox? (first term)))
   (let [[tbox target] term]
     [(make-tbox (tbox-language tbox)
                 (reduce! (fn [map [sym def]]
                            (assoc! map
                                    sym
                                    (make-dl-definition
                                     (tbox-language tbox)
                                     (definition-target def)
                                     (normalize-EL-gfp-term (expression-term (definition-expression def))))))
                          {}
                          (tbox-definition-map tbox))),
      target]),

   (and (seq? term)
        (= (first term) 'exists))
   (list (first term) (second term) (normalize-EL-gfp-term (nth term 2))),

   (and (seq? term)
        (= (first term) 'and))
   (walk normalize-EL-gfp-term
         #(list* (first term) (minimal-elements % more-specific?))
         (rest term)),

   :else term))

;;;

(defn- arguments*
  "Returns the arguments of the given DL expression as set, if it is
  not atomic. If it is, returns the singleton set of the dl-expression
  itself."
  [dl-expression]
  (if (atomic? dl-expression)
    #{dl-expression}
    (set (arguments dl-expression))))

(defn- abbreviate-expression
  "Abbreviates expression with given knowledge."
  [expression knowledge]
  (let [language              (expression-language expression),
        implication-closure   (memoize (clop-by-implications knowledge)),
        simple-more-specific? more-specific?]
    (with-altered-vars [more-specific? (constantly
                                        (fn spec?
                                          ([t-1 t-2]
                                             (simple-more-specific?
                                              t-1
                                              t-2
                                              #(contains? (implication-closure #{(make-dl-expression-nc language %1)})
                                                          (make-dl-expression-nc language %2))))
                                          ([t-1 t-2 fallback]
                                             (spec? t-1 t-2))))]
      (make-dl-expression-nc language (normalize-EL-gfp-term (expression-term expression))))))

(defn abbreviate-subsumption
  "Takes a subsumption whose subsumee and subsumer are in normal form
  and returns a subsumption where from the subsumer every term already
  present in the subsumee is removed."
  [subsumption background-knowledge]
  (let [language        (expression-language (subsumee subsumption)),
        premise-args    (arguments* (subsumee subsumption)),
        conclusion-args (difference (arguments* (subsumer subsumption))
                                    premise-args),

        ;; we do the following to have some determinism in the order of the concepts
        premise         (cons 'and (sort-by (comp str expression-term) premise-args)),
        conclusion      (cons 'and (sort-by (comp str expression-term) conclusion-args))]
    (make-subsumption (abbreviate-expression (make-dl-expression language premise)
                                             background-knowledge)
                      (abbreviate-expression (make-dl-expression language conclusion)
                                             background-knowledge))))

;;;

nil

