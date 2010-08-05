;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.algorithms.concepts
  (:require [conexp.main :as cm])
  (:use conexp.contrib.algorithms.concepts)
  (:use clojure.test))

;;;

(defvar- concepts-methods (remove #(.startsWith (name %) "default")
                                  (keys (methods concepts))))
(defvar- test-runs 50)

(deftest test-concepts
  (dotimes [_ test-runs]
    (let [ctx (cm/rand-context (cm/set-of-range (rand 15)) (rand))]
      (if-not (apply = (map #(set (concepts % ctx)) concepts-methods))
        (do (println "concepts returned different result for\n" ctx)
            (is false))
        (is true)))))

;;;

nil
