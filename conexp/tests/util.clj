(ns conexp.tests.util
  (:use conexp.util
	clojure.test
	[clojure.contrib.math :only (gcd)]))

(deftest test-ensure-length
  (is (= 10 (count (ensure-length "" 10)))
      "Test length of ensure-length result")
  (is (every? #(= \- %) (ensure-length "" 10 "-"))
      "Test padding of ensure-length result"))

(deftest test-flatten
  (is (every? #(not (seq? %)) (flatten [1 2 3 [4 [5 [6 7] [8]] 9 [10]] '(1 2)]))
      "Testing flatten property of flatten."))

(deftest test-with-str-out
  (is (= "Hallo, ich bin ein Test."
	 (with-str-out
	   "Hallo" [[[[","]] [" ich" '(" ") "bin"]] " "] "ein Test."))
      "Testing with-str-out."))

(deftest test-zip
  (let [zipped (zip [1 2 3 4] '[a b c])]
    (is (= 3 (count zipped))
	"Testing length of zip result.")
    (is (= [1 2 3] (map first zipped))
	"Testing first element of elements ot zip result")
    (is (= '[a b c] (map second zipped))
	"Testing second elements of zip result.")))

(deftest test-first-non-nil
  (is (= 3 (first-non-nil [3 2 1]))
      "Testing first-non-nil.")
  (is (= nil (first-non-nil [nil nil]))
      "Testing first-non-nil with nil vector.")
  (is (= nil (first-non-nil []))
      "Testing first-non-nil with empty vector.")
  (is (= 3 (first-non-nil [nil nil 3 2 nil]))
      "Testing first-non-nil with mixed vector.")
  (is (= nil (first-non-nil ()))
      "Testing first-non-nil with empty list.")
  (is (= 3 (first-non-nil '(nil 3 nil 2)))
      "Testing first-non-nil with mixed list."))

(deftest test-split-at-first
  (is (= [[1 3 5] [4 7 3 0]] (split-at-first even? [1 3 5 4 7 3 0]))
      "Testing split-at-first with mixed argument.")
  (is (= [[1 3 5] []] (split-at-first even? [1 3 5]))
      "Testing split-at-first with non-succeeding argument."))

(deftest test-split-at-last
  (is (= [[1 3 5 4 7 3 0] [1]] (split-at-last even? [1 3 5 4 7 3 0 1]))
      "Testing split-at-last with mixed argument.")
  (is (= [[] [1 3 5 7 3]] (split-at-last even? [1 3 5 7 3]))
      "Testing split-at-last with non-succeeding argument."))

(deftest test-illegal-argument
  (is (thrown-with-msg? IllegalArgumentException #"Dies ist eine Testnachricht."
			(illegal-argument "Dies " "ist eine " "Testnachricht" "."))
      "Testing illegal-argument."))

(deftest test-=>
  (is (=> true true)
      "Testing true => true.")
  (is (=> false true)
      "Testing false => true.")
  (is (=> false false)
      "Testing false => false.")
  (is (not (=> true false))
      "Testing true => false."))

(deftest test-<=>
 (is (<=> true true)
     "Testing true <=> true.")
 (is (<=> false false)
     "Testing false <=> false")
 (is (not (<=> true false))
     "Testing true <=> false.")
 (is (not (<=> false true))
     "Testing false <=> true."))

(deftest test-forall
  (is (not (forall [x (iterate inc 0)]
	     (even? x)))
      "Testing whether all numbers are even.")
  (is (forall [x (range 100)]
        (< x 100))
      "Testing whether all numbers below 100 are below 100.")
  (is (forall [x []] false)
      "Testing whether forall is true for the empty set."))

(deftest test-exists
  (is (exists [x (iterate inc 0)]
	(even? x))
      "Testing whether there exists an even number.")
  (is (not (exists [x (range 100)]
	     (>= x 100)))
      "Testing whether there exists a number below 100 being >= 100.")
  (is (not (exists [x []] true))
      "Testing whether exists is false for the empty set."))

(deftest test-set-of
  (is (= #{1 2 3 4 5 6} (set-of x [x (range 1 7)]))
      "Testing simple usage of set-of.")
  (is (= #{4 5 6} (set-of x [x (range 100) :when (<= 4 x 6)]))
      "Testing set-of with simple condition.")
  (is (= #{1 2 3 4 5 6} (set-of x [x (range 7) :when (= 1 (gcd x 7))]))
      "Testing set-of for \\phi(7)."))
