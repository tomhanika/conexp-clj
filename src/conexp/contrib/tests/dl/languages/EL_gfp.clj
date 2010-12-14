;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.dl.languages.EL-gfp
  (:use conexp.main
        conexp.contrib.dl.framework.syntax
        conexp.contrib.dl.framework.boxes
        conexp.contrib.dl.framework.semantics
        conexp.contrib.dl.languages.description-graphs
        conexp.contrib.dl.languages.EL-gfp
        conexp.contrib.tests.dl.examples)
  (:use clojure.test))

;;;

(deftest test-lcs
  (are [model tbox targets] (let [lcs     (EL-gfp-lcs tbox 'targets),
                                  lcs-int (interpret model lcs)]
                              (forall [target 'targets]
                                (subset? (interpret model [tbox target])
                                         lcs-int)))
       some-model some-tbox [Grandfather]
       some-model some-tbox [Grandmother]
       some-model some-tbox [Grandmother Grandfather]
       paper-model some-tbox [Grandfather Grandmother]
       small-model some-tbox [Grandfather Grandmother]
       family-model parent [Partner Self]
       family-model parent [Partner Self Child]
       family-model parent [Self Self Self Self])
  (are [model tbox target] (= (interpret model (EL-gfp-lcs tbox '[target]))
                              (interpret model [tbox 'target]))
       some-model some-tbox Grandfather
       some-model some-tbox Grandmother
       some-model all-tbox All
       family-model parent Self))

(deftest test-msc
  (are [testing-model objects] (subset? 'objects
                                        (interpret testing-model (EL-gfp-msc testing-model 'objects)))
       some-model #{John}
       some-model #{John Marry}
       some-model #{John Marry Jana}
       some-model #{John Peter}
       some-model #{Jana Marry}
       family-model #{Paul Linda Mackenzie}
       family-model #{Linda}
       family-model #{Michelle}
       family-model #{Paul Linda James John Michelle Mackenzie}
       riding-model #{RechtesVorderrad LinkesHinterrad}
       riding-model #{MeinFahrrad}))

;;;

nil
