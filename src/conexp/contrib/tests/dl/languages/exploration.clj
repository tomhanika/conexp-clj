;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.dl.languages.exploration
  (:use conexp
	conexp.contrib.dl.languages.exploration
	conexp.contrib.tests.dl.examples)
  (:use clojure.test))

;;;

(deftest test-model-gcis
  (are [gci-count model] (= gci-count (count (model-gcis model)))
       7 paper-model
       7 small-model
       7 riding-model
       19 family-model))

;;;

nil
