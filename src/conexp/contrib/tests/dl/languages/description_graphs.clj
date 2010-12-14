;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.dl.languages.description-graphs
  (:use conexp.main
        conexp.contrib.dl.framework.syntax
        conexp.contrib.dl.framework.boxes
        conexp.contrib.dl.framework.semantics
        conexp.contrib.dl.languages.description-graphs
        conexp.contrib.tests.dl.examples)
  (:use clojure.test))

;;;

(def paper-tbox (tbox FamilyDL
                      A-1 (and Male A-2 (exists HasChild (exists MarriedTo A-3)))
                      A-2 (and Female A-3 (exists MarriedTo (exists HasChild A-1)))
                      A-3 (and Mother A-2 (exists HasChild (and Male Female)))))

(defn- normalized?
  "Tests a TBox on being normalized."
  [tbox]
  (forall [def (tbox-definitions tbox)]
    (forall [term (arguments (definition-expression def))]
      (or (primitive? term)
          (and (compound? term)
               (= 'exists (operator term))
               (let [operand (second (arguments term))]
                 (and (not (primitive? operand))
                      (not (tbox-target-pair? operand))
                      (atomic? operand))))))))

(deftest test-normalize-gfp
  (are [testing-tbox] (normalized? (normalize-gfp testing-tbox))
       parent
       some-normal-tbox
       some-tbox
       paper-tbox
       all-tbox)
  (are [norm-count testing-tbox] (= norm-count (count (tbox-definitions (normalize-gfp testing-tbox))))
       3 parent
       3 some-normal-tbox
       6 some-tbox
       6 paper-tbox
       1 all-tbox)
  (are [model testing-tbox target] (= (interpret model [testing-tbox target])
                                      (interpret model [(normalize-gfp testing-tbox) target]))
       some-model some-tbox 'Grandfather
       some-model some-tbox 'Grandmother
       some-model some-normal-tbox 'A
       some-model some-normal-tbox 'B
       some-model some-normal-tbox 'T
       some-model all-tbox 'All
       family-model parent 'Self
       family-model parent 'Partner
       family-model parent 'Child
       family-model paper-tbox 'A-1
       family-model paper-tbox 'A-2
       family-model paper-tbox 'A-3))

(deftest test-simulator-sets
  (are [model] (let [graph (interpretation->description-graph model)]
                 (= (schematic-simulator-sets graph graph)
                    (efficient-simulator-sets graph graph)))
    some-model
    small-model
    family-model
    grandparent-model)
  (are [tbox] (let [graph (tbox->description-graph tbox)]
                (= (schematic-simulator-sets graph graph)
                   (efficient-simulator-sets graph graph)))
    some-tbox
    paper-tbox
    all-tbox
    some-normal-tbox
    parent))

;;;

nil
