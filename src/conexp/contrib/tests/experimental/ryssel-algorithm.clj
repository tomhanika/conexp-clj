;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.experimental.ryssel-algorithm
  (:use conexp.main
        conexp.contrib.experimental.ryssel-algorithm)
  (:use clojure.test))

;;;

(deftest test-ryssel-algorithm
  (with-testing-data [ctx (random-contexts 10 15)]
    (let [base (ryssel-base ctx)]
      (and (complete-implication-set? ctx base)
           (sound-implication-set? ctx base)))))

;;;

nil
