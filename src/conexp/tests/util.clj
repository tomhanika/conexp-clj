;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.tests.util
  (:use conexp.util
	clojure.test
	[clojure.contrib.math :only (gcd)]))

;;;

(deftest test-ensure-length
  (is (= 10 (count (ensure-length "" 10))))
  (is (every? #(= \- %) (ensure-length "" 10 "-"))))

(deftest test-flatten
  (is (every? #(not (seq? %)) (flatten [1 2 3 [4 [5 [6 7] [8]] 9 [10]] '(1 2)]))))

(deftest test-with-str-out
  (is (= "Hallo, ich bin ein Test."
	 (with-str-out
	   "Hallo" [[[[","]] [" ich" '(" ") "bin"]] " "] "ein Test."))))

(deftest test-zip
  (let [zipped (zip [1 2 3 4] '[a b c])]
    (is (= 3 (count zipped)))
    (is (= [1 2 3] (map first zipped)))
    (is (= '[a b c] (map second zipped)))))

(deftest test-first-non-nil
  (is (= 3 (first-non-nil [3 2 1])))
  (is (= nil (first-non-nil [nil nil])))
  (is (= nil (first-non-nil [])))
  (is (= 3 (first-non-nil [nil nil 3 2 nil])))
  (is (= nil (first-non-nil ())))
  (is (= 3 (first-non-nil '(nil 3 nil 2)))))

(deftest test-split-at-first
  (is (= [[1 3 5] [4 7 3 0]] (split-at-first even? [1 3 5 4 7 3 0])))
  (is (= [[1 3 5] []] (split-at-first even? [1 3 5]))))

(deftest test-split-at-last
  (is (= [[1 3 5 4 7 3 0] [1]] (split-at-last even? [1 3 5 4 7 3 0 1])))
  (is (= [[] [1 3 5 7 3]] (split-at-last even? [1 3 5 7 3]))))

(deftest test-illegal-argument
  (is (thrown-with-msg? IllegalArgumentException #"Dies ist eine Testnachricht."
			(illegal-argument "Dies " "ist eine " "Testnachricht" "."))))

(deftest test-=>
  (is (=> true true))
  (is (=> false true))
  (is (=> false false))
  (is (not (=> true false)))
  (is (=> 1 2))
  (is (=> nil false))
  (is (not (=> 1 nil))))

(deftest test-<=>
 (is (<=> true true))
 (is (<=> false false))
 (is (not (<=> true false)))
 (is (not (<=> false true)))
 (is (<=> 1 2))
 (is (<=> nil false)))

(deftest test-forall
  (is (not (forall [x (iterate inc 0)]
	     (even? x))))
  (is (forall [x (range 100)]
        (< x 100)))
  (is (forall [x []] false)))

(deftest test-exists
  (is (exists [x (iterate inc 0)]
	(even? x)))
  (is (not (exists [x (range 100)]
	     (>= x 100))))
  (is (not (exists [x []] true))))

(deftest test-set-of
  (is (= #{1 2 3 4 5 6} (set-of x [x (range 1 7)])))
  (is (= #{4 5 6} (set-of x [x (range 100) :when (<= 4 x 6)])))
  (is (= #{1 2 3 4 5 6} (set-of x [x (range 7) :when (= 1 (gcd x 7))]))))

(deftest test-distinct-by-key
  (are [sqn key rslt] (= (distinct-by-key sqn key) rslt)
       #{1 2 3 4} identity (seq #{1 2 3 4})
       [1 2 3 4]  #(< % 3) (seq [1 3])
       #{}        identity (list)))

;;;

nil