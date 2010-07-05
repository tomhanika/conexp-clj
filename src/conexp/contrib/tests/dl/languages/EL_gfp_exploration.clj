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

(deftest- model-gcis-returns-correct-result
  (are [model initial-ordering result] (if-let [[a b] ((fn find-first-unequal [seq-1 seq-2]
                                                         (when-not (and (empty? seq-1)
                                                                        (empty? seq-2))
                                                           (let [a (first seq-1),
                                                                 b (first seq-2)]
                                                             (if (not= a b)
                                                               [a b]
                                                               (find-first-unequal (rest seq-1)
                                                                                   (rest seq-2))))))
                                                       (model-gcis model 'initial-ordering)
                                                       result)]
                                         (do
                                           (println "First unequal:")
                                           (println "Got:" a)
                                           (println "Exp:" b)
                                           false)
                                         true)
       paper-model [Male Female Father Mother]
       (with-dl SimpleDL
         (list (subsumption (and Male Female)
                            (and [(tbox All (and Father Mother (exists HasChild All))),
                                  All]))
               (subsumption (and Father)
                            (and Male (exists HasChild (and))))
               (subsumption (and Mother)
                            (and (exists HasChild (and)) Female))
               (subsumption (and Male (exists HasChild (and)))
                            (and Father))
               (subsumption (and (exists HasChild (and)) Female)
                            (and Mother))
               (subsumption (and (exists HasChild (and Male)) (exists HasChild (and Female)))
                            (and [(tbox All (and Father Mother (exists HasChild All))),
                                  All]))
               (subsumption (and (exists HasChild (and (exists HasChild (and)))))
                            (and [(tbox All (and Father Mother (exists HasChild All))),
                                  All])))),

       small-model [Female Mother Male Father]
       (with-dl SimpleDL
         (list (subsumption (and Mother)
                            (and (exists HasChild (and Female)) Female))
               (subsumption (and Male)
                            (and Father))
               (subsumption (and Father)
                            (and Male))
               (subsumption (and (exists HasChild (and)))
                            (and (exists HasChild (and Female))))
               (subsumption (and (exists HasChild (and Female)) Female)
                            (and Mother))
               (subsumption (and Male Mother)
                            (and [(tbox All (and Father Mother (exists HasChild All))),
                                  All]))
               (subsumption (and (exists HasChild (and (exists HasChild (and Female)))))
                            (and [(tbox All (and Father Mother (exists HasChild All))),
                                  All])))))
  (are [model gci-count] (let [gcis (model-gcis model)]
                           (and (= gci-count (count gcis))
                                (forall [gci gcis]
                                  (holds-in-model? model gci))))
       some-model  9
       riding-model 7
       family-model 19
       more-family-model 21
       grandparent-model 32))

(defn test-ns-hook []
  (model-gcis-returns-correct-result))

;;;

nil
