;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms.concepts-test
  (:use [conexp.base :only (set-of-range def-)]
        [conexp.fca.contexts :only (rand-context)]
        [conexp.contrib.exec :only (program-exists?)]
        conexp.contrib.algorithms)
  (:use clojure.test))

;;;

(defn- add-if-not-exists [invalids program-name keyword]
  (when-not (program-exists? program-name)
    (conj invalids keyword)))

(def- concepts-methods (let [invalid-methods (-> #{}
                                                 (add-if-not-exists "pcbo" :pcbo)
                                                 (add-if-not-exists "fcbo" :fcbo))]
                         (remove #(or (.startsWith ^String (name %) "default")
                                      (contains? invalid-methods %))
                                 (keys (methods concepts)))))

(def- test-runs 50)

(deftest test-concepts
  (dotimes [_ test-runs]
    (let [ctx (rand-context (set-of-range (rand 15)) (rand)),
          rst (map set (keep #(try (concepts % ctx) (catch Exception _ nil))
                             concepts-methods))]
      (if-not (apply = rst)
        (do (println "concepts returned different result for\n" ctx)
            (is false))
        (is true)))))

;;;

nil
