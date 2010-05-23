;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.algorithms.titanic
  (:use conexp.main
        conexp.contrib.algorithms.titanic)
  (:use clojure.test))

;;;

(defvar- *testing-data*
  [(diag-context (set-of-range 10)),
   (one-context (set-of-range 10)),
   ])

(deftest test-titanic-context-intents
  (with-testing-data [ctx *testing-data*]
    (= (set (context-intents ctx)) (set (titanic-context-intents ctx)))))

(deftest test-titanic-iceberg-intent-set
  (forall [minsupp [0.0 0.2 0.5 0.7 0.9 1.0]]
    (with-testing-data [ctx *testing-data*]
      (= (set (iceberg-intent-set ctx minsupp))
         (set (titanic-iceberg-intent-set ctx minsupp)))))
  (with-testing-data [ctx *testing-data*]
    (= (titanic-context-intents ctx)
       (titanic-iceberg-intent-set ctx 0.0))))

;;;

nil
