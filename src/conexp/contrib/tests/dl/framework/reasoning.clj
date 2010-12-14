;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.dl.framework.reasoning
  (:use conexp.main
        conexp.contrib.dl.framework.syntax
        conexp.contrib.dl.framework.boxes
        conexp.contrib.dl.framework.reasoning
        conexp.contrib.tests.dl.examples)
  (:use clojure.test))

;;;

(deftest test-subsumed-by?
  (are [dl exp] (subsumed-by? (dl-expression dl exp) (dl-expression dl exp))
       SimpleDL (exists HasChild (and Mother Female (exists HasChild (and))))
       SimpleDL Mother)
  (are [dl exp-1 exp-2] (subsumed-by? (dl-expression dl exp-1) (dl-expression dl exp-2))
       SimpleDL (exists HasChild Female) (exists HasChild (and)),
       SimpleDL (and Female Mother) (and Female)
       FamilyDL [parent Self] (exists MarriedTo [parent Self])
       SimpleDL (exists HasChild Female) (exists HasChild (and))
       SimpleDL all-cpt (and all-cpt))
  (are [dl exp-1 exp-2] (not (subsumed-by? (dl-expression dl exp-1) (dl-expression dl exp-2)))
       SimpleDL (exists HasChild (and)) (exists HasChild Female)
       FamilyDL (exists HasChild (and)) (exists MarriedTo (and))
       FamilyDL (and (exists MarriedTo (and))
                     (exists HasChild (and Female))
                     (exists MarriedTo [(tbox FamilyDL
                                              [[[Mackenzie James] Mackenzie] James] (and),
                                              [[[John Linda] Michelle] Paul] (and (exists MarriedTo [[[Michelle Paul] John] Linda])
                                                                                  (exists HasChild [[[Mackenzie James] Mackenzie] James])),
                                              [[[Michelle Paul] John] Linda] (and (exists MarriedTo [[[John Linda] Michelle] Paul])
                                                                                  (exists HasChild [[[Mackenzie James] Mackenzie] James]))),
                                        [[[John Linda] Michelle] Paul]])
                     (exists HasChild (and)))
                [(tbox FamilyDL
                    [Michelle John] (and (exists MarriedTo [John Michelle])
                                         (exists HasChild [Mackenzie Mackenzie])),
                    [John Michelle] (and (exists HasChild [Mackenzie Mackenzie])
                                         (exists MarriedTo [Michelle John]))
                    [Mackenzie Mackenzie] (and Female)),
                 [John Michelle]]))

;;;

nil
