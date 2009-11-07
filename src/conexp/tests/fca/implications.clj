(ns conexp.tests.fca.implications
  (:use clojure.test
	conexp.base
	conexp.fca.contexts
	conexp.fca.implications))

(deftest test-stem-base
  (is (= 1 (count (stem-base (one-context #{1 2 3 4 5}))))))