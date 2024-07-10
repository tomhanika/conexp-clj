;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.base-test
  (:use clojure.test
        conexp.base)
  (:require [clojure.set :refer [difference union subset? superset? intersection]]
            [clojure.math.numeric-tower :refer [gcd]]))

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

(deftest test-first-position-if
  (is (= 0 (first-position-if odd? [1 2 3 4])))
  (is (= 1 (first-position-if even? [1 2 3 4])))
  (is (nil? (first-position-if (every-pred odd? even?) [1 2 3 4]))))

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

(deftest test-warn
  (= "WARNING: Huhu!" (with-out-str (warn "Huhu!"))))

(deftest test-illegal-argument
  (is (thrown-with-msg? IllegalArgumentException #"Dies ist eine Testnachricht."
        (illegal-argument "Dies " "ist eine " "Testnachricht" "."))))

(deftest test-unsupported-operation
  (is (thrown-with-msg? UnsupportedOperationException #"Und wieder eine Testnachricht."
        (unsupported-operation "Und" " wieder " "eine" " Testnachricht."))))

(deftest test-not-yet-implemented
  (is (thrown-with-msg? UnsupportedOperationException #"Not yet implemented"
        (not-yet-implemented))))

(deftest test-illegal-state
  (is (thrown-with-msg? IllegalStateException #"und nochmal"
        (illegal-state "und" " nochmal"))))

(def- a 1)
(def- b 2)

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

(def- f (fn [x] (swap! x inc)))

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

(deftest test-sum
  (is (= 55 (sum i 0 10 i)))
  (is (= 0 (sum i 10 0 i))))

(deftest test-prod
  (is (= 720 (prod i 1 6 i)))
  (is (= 1 (prod i 6 1 i))))

(deftest test-expt
  (is (= (expt 123131231231231212387123781263871263876123 2)
         15161300104518928707165873054809555514386223330959122109726946661708534874289511129N))
  (is (= (expt 1.0 2.0) 1.0))
  (is (= (expt 1.0 -2.0) 1.0))
  (is (= (expt 2.0 -2.0) 0.25))
  (is (= (expt 2 -2) 1/4)))

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

(deftest test-ensure-seq
  (is (= () (ensure-seq nil)))
  (is (= '(1 2 3)) (ensure-seq [1 2 3]))
  (is (= '(0 1 2) (ensure-seq 3)))
  (is (thrown? IllegalArgumentException
               (ensure-seq 'a))))

(deftest test-topological-sort
  (let [sorted (topological-sort subset?
                                 [#{2} #{2 3 4} #{1 2 3} #{2 3} #{1 4}
                                  #{1 2 3 4} #{1 3 4} #{3 4} #{4} #{1}
                                  #{1 3} #{2 4} #{3} #{} #{1 2 4} #{1 2}])]
    (is (forall [tail (tails sorted),
                 x    (rest tail)]
          (not (superset? (first tail) x))))))

;;;

(deftest test-minimum-set-covers
  (are [n k] (= k (count (minimum-set-covers (set-of-range n)
                                             (subsets (set-of-range n)))))
    1 1
    2 2
    3 8
    4 49
    5 462
    6 6424
    7 129425
    )
  (is (= (set (minimum-set-covers #{1 2 3 4} [#{1 2 3} #{2 3 4} #{1 0} #{5 6 7} #{1 2 3 4 5 6 7 8}]))
         (set [#{#{1 2 3 4 5 6 7 8}} #{#{1 2 3} #{2 3 4}} #{#{0 1} #{2 3 4}}]))))

;;;

(deftest test-cross-product
  (is (= #{[1 1] [1 2] [2 1] [2 2]} (cross-product #{1 2} #{1 2})))
  (is (and (= #{} (cross-product #{} #{1 2}))
           (= #{} (cross-product #{1 2} #{}))
           (= #{} (cross-product #{} #{}))))
  (is (= 12 (count (cross-product #{1 2 3} #{1 2 3 4}))))
  (is (= (* 2 3 4) (count (cross-product #{1 2} #{3 4 5} #{6 7 8 9}))))
  (is (= #{[]} (cross-product))))

(deftest test-disjoint-union
  (is (= 11 (count (disjoint-union #{1 2 3 4 5 6} #{7 8 9 10 11}))))
  (is (= (+ 1 2 3 4) (count (disjoint-union #{1} #{1 2} #{1 2 3} #{1 2 3 4}))))
  (are [set-1 set-2 set-3] (let [du (disjoint-union set-1 set-2 set-3)]
                             (and (= set-1 (set-of x [[x z] du :when (= z 0)]))
                                  (= set-2 (set-of x [[x z] du :when (= z 1)]))
                                  (= set-3 (set-of x [[x z] du :when (= z 2)]))))
       #{1 2 3} #{3 4 5} #{7 8 9}
       #{} #{'a *} #{}))

(deftest test-set-of-range
  (is (= 100 (count (set-of-range 0 100))))
  (is (= (set (range 0 10 2)) (set-of-range 0 10 2)))
  (is (= #{} (set-of-range 23 11))))

(deftest test-to-set
  (is (= (to-set 2) #{0 1}))
  (is (= (to-set ['a 'b]) '#{a b}))
  (is (thrown? IllegalArgumentException (to-set 'a))))

(deftest test-minimal-hypergraph-transversals
  (are [sets minimal-sets] (= (set minimal-sets)
                              (set (minimal-hypergraph-transversals (reduce union sets) sets)))
       [#{2} #{2 4}] [#{2}]
       [#{1 2 3} #{2 3 4} #{2 3 5}] [#{1 4 5} #{2} #{3}]
       [#{1 2} #{1 3} #{2 3}] [#{1 2} #{1 3} #{2 3}])
  (forall [selection (map (partial take 20)
                          (take 100 (iterate shuffle (subsets (set-of-range 10)))))]
    (let [mhts (minimal-hypergraph-transversals (set-of-range 10) selection)]
      (is (every? (fn [mht]
                    (forall [x selection]
                      (not-empty (intersection x mht))))
                  mhts))
      (is (every? (fn [mht]
                    (every? (fn [sub-mht]
                              (exists [x selection]
                                (empty? (intersection x sub-mht))))
                            (map #(disj mht %)
                                 mht)))
                  mhts))))
  (doseq [selection (map (partial take 20)
                         (take 100 (iterate shuffle (subsets (set-of-range 10)))))]
    (let [base-set (reduce union selection),
          mscs     (minimum-set-covers base-set selection),
          mhts     (minimal-hypergraph-transversals
                    selection
                    (set-of (set-of S | S selection :when (contains? S m))
                            | m base-set))]
      (is (= (set mscs) (set mhts))))))

;;;

nil
