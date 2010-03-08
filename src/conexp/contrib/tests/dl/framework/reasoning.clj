;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.dl.framework.reasoning
  (:use conexp
	conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.framework.reasoning
	conexp.contrib.dl.examples)
  (:use clojure.test))

;;;

(defvar- mf-exp (dl-expression FamilyDL (exists Child (and Mother Female (exists Child (and))))))

(deftest test-subsumed-by?
  (is (subsumed-by? mf-exp mf-exp)))

;;;

nil
