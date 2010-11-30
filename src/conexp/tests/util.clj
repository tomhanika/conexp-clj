;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(in-ns 'conexp.tests.base)

;;;

(deftest test-singleton?
  (are [x] (singleton? x)
       #{1}
       #{nil}
       [1]
       [nil]
       ['a]
       [[[]]])
  (are [x] (not (singleton? x))
       #{}
       []
       nil
       [[][]]))

(deftest test-ensure-length
  (is (= 10 (count (ensure-length "" 10))))
  (is (every? #(= \- %) (ensure-length "" 10 "-"))))

(deftest test-with-str-out
  (is (= "Hallo, ich bin ein Test."
	 (with-str-out
	   "Hallo" [[[[","]] [" ich" '(" ") "bin"]] " "] "ein Test."))))

(deftest test-sort-by-first
  (are [x y] (sort-by-first x y)
       [1 2] [2 1]
       ['a 2] ['b 1]
       [nil 2] [2 nil]
       1 2)
  (are [x y] (not (sort-by-first x y))
       2 1
       'a ['b 2]))

(deftest test-sort-by-second
  (are [x y] (sort-by-second x y)
       1 2
       [nil 1] [nil 2]
       [2 1] [1 2])
  (are [x y] (not (sort-by-second x y))
       2 1
       [nil 2] [1 nil]
       'b 'a))

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

(deftest test-unsupported-operation
  (is (thrown-with-msg? UnsupportedOperationException #"Und wieder eine Testnachricht."
        (unsupported-operation "Und" " wieder " "eine" " Testnachricht."))))

(deftest test-illegal-state
  (is (thrown-with-msg? IllegalStateException #"und nochmal"
        (illegal-state "und" " nochmal"))))

(defvar- a 1)
(defvar- b 2)

(deftest test-with-altered-vars
  (is (and (= 1 a) (= 2 b)))
  (with-altered-vars [a (constantly 5)]
    (is (and (= 5 a) (= 2 b))))
  (is (and (= 1 a) (= 2 b))))

(deftest test-with-var-bindings
  (is (and (= 1 a) (= 2 b)))
  (with-var-bindings [a 3]
    (is (and (= 3 a) (= 2 b))))
  (is (and (= 1 a) (= 2 b))))

(defvar- f (fn [x] (swap! x inc)))

(deftest test-with-memoized-fns
  (let [counter (atom 0)]
    (is (= 0 @counter))
    (f counter)
    (is (= 1 @counter))
    (with-memoized-fns [f]
      (dotimes [_ 10]
        (f counter)))
    (is (= 2 @counter))
    (f counter)
    (is (= 3 @counter))))

(deftest test-memo-fn
  (is (= 1 (let [counter (atom 0),
                 f (memo-fn my-fn [x] (swap! counter inc) x)]
             (f 1)
             (f 1)
             (f 1)))))

(deftest test-inits
  (is (= (list [] [1] [1 2]) (inits [1 2])))
  (is (= (list []) (inits []))))

(deftest test-tails
  (is (= (list [1 2] [2] []) (tails [1 2])))
  (is (= (list []) (tails []))))

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
  (is (forall [x []] false))
  (is (forall [a [2 3 4 5]
               b [30 60]]
        (not= 1 (gcd a b)))))

(deftest test-exists
  (is (exists [x (iterate inc 0)]
	(even? x)))
  (is (not (exists [x (range 100)]
	     (>= x 100))))
  (is (not (exists [x []] true)))
  (is (exists [x [1 2 3 4],
               y [5 6 7 8]]
        (= 1 (gcd x y)))))

(deftest test-set-of
  (is (= #{1 2 3 4 5 6} (set-of x [x (range 1 7)])))
  (is (= #{4 5 6} (set-of x [x (range 100) :when (<= 4 x 6)])))
  (is (= #{1 2 3 4 5 6} (set-of x [x (range 7) :when (= 1 (gcd x 7))])))
  (is (= #{1 2 3 5 7 11 13 17 19} (set-of x | x (range 1 20),
                                              :when (forall [y (range 1 x)]
                                                      (= 1 (gcd x y)))))))

(deftest test-div
  (is (= 1 (div 3 2)))
  (is (= 0 (div 1 2)))
  (is (= 2 (div 4 2))))

(deftest test-distinct-by-key
  (are [sqn key rslt] (= (distinct-by-key sqn key) rslt)
       #{1 2 3 4} identity (seq #{1 2 3 4})
       [1 2 3 4]  #(< % 3) (seq [1 3])
       #{}        identity (list)))

(deftest test-map-by-fn
  (is (= {1 2 2 3 3 4} (map-by-fn inc [1 2 3])))
  (is (= {} (map-by-fn identity []))))

(deftest test-with-printed-result
  (is (= (with-out-str
           (with-printed-result "Result:"
             (inc 1)))
         "Result: 2\n")))

;;;

nil