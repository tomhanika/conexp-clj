;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.many-valued-contexts-test
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.many-valued-contexts)
  (:use clojure.test))

;;;

(deftest test-make-mv-context
  (is (make-mv-context #{1 2 3 4} #{1 2 3 4} +))
  (is (make-mv-context #{} #{} (constantly true)))
  (is (make-mv-context [1 2 3] [4 5 6] /))
  (is (make-mv-context 10 10 +))
  (is (thrown? IllegalArgumentException (make-mv-context 1 2 3)))
  (is (make-mv-context [1] '#{a} (constantly false)))
  (is (= {[1 1] 2, [1 2] 3, [2 1] 3, [2 2] 4}
         (incidence (make-mv-context [1 2] [1 2]
                                     {[1 1] 2, [1 2] 3, [2 1] 3, [2 2] 4}))))
  (is (thrown? IllegalArgumentException
               (make-mv-context [] [] {[1 1] 2})))
  (is (make-mv-context-nc 2 2 {[0 0] 0, [0 1] 1, [1 0] 0, [1 1] 1})))

(deftest test-make-mv-context-from-matrix
  (is (let [mv-ctx (make-mv-context-from-matrix 2 2 [1 2 nil 3])]
        (is (= #{0 1} (objects mv-ctx)))
        (is (= #{0 1} (attributes mv-ctx)))
        (is (= {[0 0] 1, [0 1] 2, [1 0] nil, [1 1] 3}
               (incidence mv-ctx)))))
  (is (= #{'a 'b} (objects (make-mv-context-from-matrix '[a b] [] []))))
  (is (= #{nil =} (attributes (make-mv-context-from-matrix [] [nil =] [])))))

(deftest test-Many-Valued-Context-equals
  (is (= (make-mv-context [] [] (constantly true))
         (make-mv-context [] [] (constantly true))))
  (is (= (make-mv-context [1 2 3 4] [1 2 3 4] +)
         (make-mv-context [4 3 2 1] [3 2 4 1] +)))
  (is (not= (make-mv-context [1 2 3 4] [1 2 3 4] -)
            (make-mv-context [1 2 3 4] [1 2 3 4] +)))
  (is (= (make-mv-context [1] [1] +)
         (make-mv-context [1] [1] [[1 1 2] [1 2 3] [2 3 4]]))))

;;;

(def- testing-data
  [(make-mv-context #{} #{} +),
   (make-mv-context [1 2 3] [1 2 3] =),
   (make-mv-context (range 100) (range 100) *),
   (make-mv-context (range 100) (range 100) (fn [& _] (rand)))])

(deftest test-Many-Valued-Context-hashCode
  (with-testing-data [mv-ctx-1 testing-data,
                      mv-ctx-2 testing-data]
    (=> (= mv-ctx-1 mv-ctx-2)
        (= (hash mv-ctx-1) (hash mv-ctx-2)))))

(deftest test-mv-context-getter
  (is (let [mv-context (make-mv-context [1 2 3] '[a b c] #(str %1 %2))]
        (and (= (objects mv-context) #{1 2 3})
             (= (attributes mv-context) '#{a b c})
             (= (incidence mv-context)
                {[1 'a] "1a",
                 [1 'b] "1b",
                 [1 'c] "1c",
                 [2 'a] "2a",
                 [2 'b] "2b",
                 [2 'c] "2c",
                 [3 'a] "3a",
                 [3 'b] "3b",
                 [3 'c] "3c"}))))
  (is (let [mv-context (make-mv-context-from-matrix [1 2] [1 2] [true 1
                                                                 nil "Hallo"])]
        (and (= (objects mv-context) (attributes mv-context) #{1 2})
             (= (incidence mv-context)
                {[1 1] true,
                 [1 2] 1,
                 [2 1] nil,
                 [2 2] "Hallo"}))))
  (is (let [mv-context (make-mv-context 10 10 +)]
        (and (= (objects mv-context) (set-of-range 10))
             (= (attributes mv-context) (set-of-range 10))
             (= 100 (count (incidence mv-context)))
             (forall [a (range 10), b (range 10)]
               (= (+ a b)
                  (get (incidence mv-context) [a b]))))))
  (is (let [mv-context (make-mv-context-nc 2
                                           2
                                           (map-by-fn identity
                                                      (cross-product [0 1] [0 1])))]
        (and (= (objects mv-context) #{0 1})
             (= (attributes mv-context) #{0 1})
             (= (incidence mv-context)
                {[0 0] [0 0],
                 [1 0] [1 0],
                 [0 1] [0 1],
                 [1 1] [1 1]})))))

;;;

(deftest test-values-of-attribute
  (with-testing-data [mv-ctx testing-data]
    (forall [m (attributes mv-ctx)]
      (= (set (values-of-attribute mv-ctx m))
         (set (map #(get (incidence mv-ctx) %)
                   (map #(vector % m) (objects mv-ctx))))))))

(deftest test-values-of-object
  (with-testing-data [mv-ctx testing-data]
    (forall [g (objects mv-ctx)]
      (= (set (values-of-object mv-ctx g))
         (set (map #(get (incidence mv-ctx) %)
                   (map #(vector g %) (attributes mv-ctx))))))))

(deftest test-nominal-scale
  (is (= (nominal-scale (range 10))
         (make-context (range 10) (range 10) =)))
  (is (= (nominal-scale (range 10) (range 5))
         (make-context (range 10) (range 5) =))))

(deftest test-ordinal-scale
  (is (= (ordinal-scale [1 2])
         (make-context-from-matrix [1 2] [['<= 1] ['<= 2]] [1 1 0 1])))
  (is (= (ordinal-scale [1 2] >=)
         (make-context-from-matrix [1 2] [['<= 1] ['<= 2]] [1 0 1 1])))
  (is (= (ordinal-scale [1 2] [2 3] >=)
         (make-context-from-matrix [1 2] [['<= 2] ['<= 3]] [0 0 1 0]))))

(deftest test-interordinal-scale
  (is (= (interordinal-scale [1 2])
         (make-context-from-matrix [1 2]
                                   '[[<= 1] [<= 2] [>= 1] [>= 2]]
                                   [1 1 1 0 0 1 1 1])))
  (is (= (interordinal-scale [1 2])
         (interordinal-scale [1 2] <= >=)))
  (is (= (interordinal-scale [1 2] [2 3] > <)
         (make-context-from-matrix [1 2]
                                   '[[<= 2] [<= 3] [>= 2] [>= 3]]
                                   [0 0 1 1
                                    0 0 0 1]))))

(deftest test-biordinal-scale
  (is (= (biordinal-scale [1 2] 1)
         (make-context-from-matrix [1 2]
                                   '[[<= 1] [>= 2]]
                                   [1 0 0 1])))
  (is (= (biordinal-scale [1 2] 2)
         (ordinal-scale [1 2])))
  (is (= (biordinal-scale [1 2] [2 3] 1 >= <=)
         (make-context-from-matrix [1 2]
                                   '[[<= 2] [>= 3]]
                                   [0 1 1 1]))))

(deftest test-dichotomic-scale
  (is (= (dichotomic-scale [1 2])
         (make-context [1 2] [1 2] =)))
  (is (thrown? AssertionError
               (dichotomic-scale [1 2 3])))
  (is (thrown? AssertionError
               (dichotomic-scale [1]))))

(deftest test-interval-scale
  (is (thrown? AssertionError
               (interval-scale [1 2 3] #{1 5 6})))
  (is (= (interval-scale [1 2 3 4 5 6 7 8] [1 4 8])
         (make-context [1 2 3 4 5 6 7 8]
                       '[[∈ [1, 4]], [∈ [4, 8]]]
                       '[[1, [∈ [1, 4]]],
                         [2, [∈ [1, 4]]],
                         [3, [∈ [1, 4]]],
                         [4, [∈ [4, 8]]],
                         [5, [∈ [4, 8]]],
                         [6, [∈ [4, 8]]],
                         [7, [∈ [4, 8]]]]))))

;;;

(deftest test-scale-mv-context
  (let [mv-ctx (make-mv-context-from-matrix '[a b c] ['x 2 3] [true  1 4,
                                                               false 2 2,
                                                               true  3 0]),
        sc-ctx (make-context-from-matrix '[a b c]
                                         '[[x false]
                                           [x true]
                                           [2 [<= 1]]
                                           [2 [<= 2]]
                                           [2 [<= 3]]
                                           [3 [<= 0]]
                                           [3 [<= 2]]
                                           [3 [<= 4]]
                                           [3 [>= 0]]
                                           [3 [>= 2]]
                                           [3 [>= 4]]]
                                         [0 1 1 1 1 0 0 1 1 1 1,
                                          1 0 0 1 1 0 1 1 1 1 0,
                                          0 1 0 0 1 1 1 1 1 0 0])]
    (is (= (scale-mv-context-with mv-ctx
                                  [x] (nominal-scale values),
                                  [2] (ordinal-scale values <=),
                                  [3] (interordinal-scale values <= >=))
           sc-ctx))
    (is (= (scale-mv-context-with mv-ctx
                                  [2] (ordinal-scale values <=),
                                  [3] (interordinal-scale values <= >=)
                                  (nominal-scale values)) ;default scale
           sc-ctx))))

;;;

nil
