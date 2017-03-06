;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms-test
  (:use clojure.test)
  (:use [conexp.base :only (def- set-of-range)]
        [conexp.fca.contexts :only (rand-context
                                    random-context
                                    random-contexts
                                    make-context-from-matrix)]
        [conexp.fca.implications :only (make-implication impl)]
        [conexp.contrib.exec :only (program-exists?)]
        conexp.contrib.algorithms))


;;; Concept Calculations

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

;;; Canonical Base Computation

(deftest test-canonical-base
  (dotimes [_ test-runs]
    (let [ctx    (rand-context (set-of-range (rand 15)) (rand)),
          base-1 (canonical-base ctx)
          base-2 (conexp.fca.implications/canonical-base ctx)]
      (if-not (= base-1 base-2)
        (do (println "canonical-base returned different result for\n" ctx)
            (println base-1)
            (println base-2)
            (is false))
        (is true))))
  (dotimes [_ test-runs]
    (is (zero? (count (canonical-base (random-context 20 0.7)
                                      #{(make-implication #{} (set-of-range 20))})))))
  (is (let [ctx (make-context-from-matrix 5 5
                                          [0 0 1 0 1
                                           0 1 1 0 0
                                           0 0 1 0 1
                                           0 0 1 1 1
                                           0 0 1 0 1]),
            bgk #{(impl ==> 2)}]
        (= (set (canonical-base ctx bgk))
           #{(impl 0 2 ==> 1 3 4)
             (impl 2 3 ==> 4)
             (impl 1 2 4 ==> 0 3)}))))

;;;

nil

