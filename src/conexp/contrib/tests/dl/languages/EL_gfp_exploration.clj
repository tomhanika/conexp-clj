;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.dl.languages.EL-gfp-exploration
  (:use conexp.main
        conexp.contrib.dl.framework.syntax
        conexp.contrib.dl.framework.boxes
        conexp.contrib.dl.framework.semantics
        conexp.contrib.dl.languages.EL-gfp-exploration
        conexp.contrib.tests.dl.examples)
  (:use clojure.test))

;;;

(defn- term=
  "Returns true iff the term a and b are syntactically equal upto
  trivial rewriting."
  [a b]
  (cond
   (and (not (seq? a)) (not (seq? b)))
   (= a b),

   (or (not (seq? a)) (not (seq? b)))
   false,

   (= 'and (first a) (first b))
   (and (forall [x (rest a)]
          (exists [y (rest b)]
            (term= x y)))
        (forall [y (rest b)]
          (exists [x (rest a)]
            (term= x y)))),

   (= 'exists (first a) (first b))
   (and (= (second a) (second b))
        (term= (nth a 2) (nth b 2))),

   :otherwise
   false))

(defn- subsumption=
  "Returns true iff the subsumptions a and b are term= in both there
  subsumee and subsumer."
  [a b]
  (and (term= (expression-term (subsumee a))
              (expression-term (subsumee b)))
       (term= (expression-term (subsumer a))
              (expression-term (subsumer b)))))

(defn- find-all-unequal
  "Finds all elements in seq-1, that are not subsumption= to the
  corresponding element in seq-2."
  [seq-1 seq-2]
  (when-not (and (empty? seq-1)
                 (empty? seq-2))
    (let [a (first seq-1),
          b (first seq-2)]
      (if (not (subsumption= a b))
        (cons [a b]
              (find-all-unequal (rest seq-1) (rest seq-2)))
        (find-all-unequal (rest seq-1) (rest seq-2))))))

(deftest- model-gcis-returns-correct-result
  (are [model initial-ordering result] (let [unequals (find-all-unequal (model-gcis model 'initial-ordering)
                                                                        result)]
                                         (if-not (empty? unequals)
                                           (do
                                             (doseq [[a b] unequals]
                                               (println "Got:" a)
                                               (println "Exp:" b))
                                             false)
                                           true))
       paper-model [Male Female Father Mother]
       (with-dl SimpleDL
         (list (subsumption (and Female Male)
                            (and [(tbox All (and Father Mother (exists HasChild All))),
                                  All]))
               (subsumption (and Father)
                            (and (exists HasChild (and)) Male))
               (subsumption (and Mother)
                            (and Female (exists HasChild (and))))
               (subsumption (and (exists HasChild (and)) Male)
                            (and Father))
               (subsumption (and Female (exists HasChild (and)))
                            (and Mother))
               (subsumption (and (exists HasChild (and Female)) (exists HasChild (and Male)))
                            (and [(tbox All (and Father Mother (exists HasChild All))),
                                  All]))
               (subsumption (and (exists HasChild (and (exists HasChild (and)))))
                            (and [(tbox All (and Father Mother (exists HasChild All))),
                                  All])))),

       small-model [Female Mother Male Father]
       (with-dl SimpleDL
         (list (subsumption (and Mother)
                            (and Female (exists HasChild (and Female))))
               (subsumption (and Male)
                            (and Father))
               (subsumption (and Father)
                            (and Male))
               (subsumption (and (exists HasChild (and)))
                            (and (exists HasChild (and Female))))
               (subsumption (and (exists HasChild (and Female)) Female)
                            (and Mother))
               (subsumption (and Father Mother)
                            (and [(tbox All (and Father Mother (exists HasChild All))),
                                  All]))
               (subsumption (and (exists HasChild (and (exists HasChild (and Female)))))
                            (and [(tbox All (and Father Mother (exists HasChild All))),
                                  All]))))

       ))

(deftest- model-gcis-returns-correct-count
  (are [model gci-count] (let [gcis (model-gcis model)]
                           (and (= gci-count (count gcis))
                                (forall [gci gcis]
                                  (holds-in-interpretation? model gci))))
       some-model  9,
       riding-model 7,
       family-model 19,
       more-family-model 21,
       grandparent-model 32))

(defn test-ns-hook []
  (model-gcis-returns-correct-result)
  (model-gcis-returns-correct-count))

;;;

nil
