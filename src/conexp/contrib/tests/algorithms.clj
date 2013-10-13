;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.algorithms
  (:use conexp.main
        clojure.test
        conexp.contrib.algorithms)
  (:require [conexp.tests.fca.contexts :as contexts]))

;;; parallel-canonical-base

(defn test-parallel-canonical-base [ctx & nums]
  (with-testing-data [ctx [contexts/test-ctx-01
                           contexts/test-ctx-04
                           contexts/test-ctx-07
                           contexts/test-ctx-08]
                      num [2 5 10 20]]
    (apply =
           (canonical-base ctx)
           (map #(parallel-canonical-base ctx %)
                num))))

;;; concepts

(defn- add-if-not-exists [invalids program-name keyword]
  (when-not (program-exists? program-name)
    (conj invalids keyword)))

(defvar- concepts-methods (let [invalid-methods (-> #{}
                                                    (add-if-not-exists "pcbo" :pcbo)
                                                    (add-if-not-exists "fcbo" :fcbo))]
                            (remove #(or (.startsWith ^String (name %) "default")
                                         (contains? invalid-methods %))
                                    (keys (methods concepts)))))

(defvar- test-runs 50)

(deftest test-concepts
  (dotimes [_ test-runs]
    (let [ctx (cm/rand-context (cm/set-of-range (rand 15)) (rand)),
          rst (map set (keep #(try (concepts % ctx) (catch Exception _ nil))
                             concepts-methods))]
      (if-not (apply = rst)
        (do (println "concepts returned different result for\n" ctx)
            (is false))
        (is true)))))

;;; Rest

(tests-to-run conexp.contrib.tests.algorithms.titanic)

;;;

nil

