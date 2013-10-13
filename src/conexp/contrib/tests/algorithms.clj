;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.algorithms
  (:use conexp.main
        clojure.test)
  (:require [conexp.contrib.exec :as e]
            [conexp.contrib.algorithms :as a]
            [conexp.tests.fca.contexts :as c]))

;;; parallel-canonical-base

(defn test-parallel-canonical-base [ctx & nums]
  (with-testing-data [ctx [c/test-ctx-01
                           c/test-ctx-04
                           c/test-ctx-07
                           c/test-ctx-08]
                      num [2 5 10 20]]
    (apply =
           (canonical-base ctx)
           (map #(a/parallel-canonical-base ctx %) num))))

;;; Rest

(tests-to-run conexp.contrib.tests.algorithms.concepts
              conexp.contrib.tests.algorithms.titanic)

;;;

nil

