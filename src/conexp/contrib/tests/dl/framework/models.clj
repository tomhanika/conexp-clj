;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.dl.framework.models
  (:use conexp
	conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.framework.boxes
	conexp.contrib.dl.framework.models
	conexp.contrib.tests.dl.examples)
  (:use clojure.test))

;;;

(defvar- b-tbox (tbox FamilyDL
		      B [parent Self]))
(defvar- a-tbox (tbox FamilyDL
		      A [b-tbox B]))

(deftest test-interpret
  (are [expected testing-model expr] (= 'expected
					(interpret testing-model
						   (dl-expression (model-language testing-model) expr)))
       #{} some-model all-cpt,
       #{John} some-model [some-tbox Grandfather],
       #{Marry} some-model [some-tbox Grandmother],
       #{John Linda Michelle Paul} family-model [parent Self],
       #{John Linda Michelle Paul} family-model [parent Partner]
       #{Marry} some-model Mother,
       #{John Peter} some-model Father,
       #{Jana Marry} some-model Female,
       #{John Linda Michelle Paul} family-model [a-tbox A]))

;;;

nil
