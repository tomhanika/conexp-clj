;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.dl.framework.semantics
  (:use conexp.main
        conexp.contrib.dl.framework.syntax
        conexp.contrib.dl.framework.boxes
        conexp.contrib.dl.framework.semantics
        conexp.contrib.tests.dl.examples)
  (:use clojure.test))

;;;

(defvar- b-tbox (tbox FamilyDL, B [parent Self]))
(defvar- a-tbox (tbox FamilyDL, A [b-tbox B]))

(deftest test-interpret
  (are [expected testing-model expr] (= 'expected
                                        (interpret testing-model
                                                   (dl-expression (interpretation-language testing-model)
                                                                  expr)))
       #{} some-model all-cpt,
       #{John} some-model [some-tbox Grandfather],
       #{Marry} some-model [some-tbox Grandmother],
       #{John Linda Michelle Paul} family-model [parent Self],
       #{John Linda Michelle Paul} family-model [parent Partner]
       #{Marry} some-model Mother,
       #{John Peter} some-model Father,
       #{Jana Marry} some-model Female,
       #{Marry} some-model (nominal Marry),
       #{Marry John} some-model (nominal Marry John),
       #{Marry} some-model (nominal Marry Fred),
       #{} some-model (bottom),
       #{John Marry Peter Jana} some-model (top),
       #{John Linda Michelle Paul} family-model [a-tbox A]
       #{John Marry} some-model dl-exp
       #{John} some-model ext-dl-exp
       #{John} some-model ext-dl-exp-2
       #{John Mackenzie Michelle} small-model [(tbox (interpretation-language small-model)
                                                     A A),
                                               A]
       #{John Mackenzie Michelle} small-model [(tbox (interpretation-language small-model)
                                                     A B,
                                                     B C,
                                                     C D,
                                                     D E,
                                                     E A),
                                               A]))

(deftest test-gfp-lfp-model
  (are [mymodel mytbox] (let [gfp (gfp-model mytbox mymodel)]
                          (forall [def (tbox-definitions mytbox)]
                            (= (interpret gfp (definition-target def))
                            (interpret gfp (definition-expression def)))))
     small-model (tbox SimpleDL A A),
     small-model (tbox SimpleDL A A, B B, C C)
     small-model (tbox SimpleDL A B, B C, C D, D E, E A)))

;;;

nil
