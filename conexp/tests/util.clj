(ns conexp.tests.util
  (:use conexp.util
	clojure.test))

(deftest test-ensure-length
  (do (is (= 10 (count (ensure-length "" 10))) 
	  "Test length of ensure-length result")
      (is (every? #(= \- %) (ensure-length "" 10 "-")) 
	  "Test padding of ensure-length result")))

(deftest test-flatten
  (do (is (every? #(not (seq? %)) (flatten [1 2 3 [4 [5 [6 7] [8]] 9 [10]] '(1 2)]))
	  "Testing flatten property of flatten.")))