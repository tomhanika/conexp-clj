(ns conexp.fca.metric-context-test
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.lattices
        conexp.fca.metric-contexts)
  (:use clojure.test))

(defn object-metric-1 [x y] identity)
(defn object-metric-2 [x y] identity)
(defn object-metric-3 [x y] identity)

(defn attribute-metric-1 [x y] identity)
(defn attribute-metric-2 [x y] identity)
(defn attribute-metric-3 [x y] identity)

(def test-objs #{1 2 3 4 5 6})
(def test-attrs #{"A" "B" "C" "D" "E"})
(def test-inc #{[1 "A"] [1 "B"] [1 "D"] [1 "E"]
                             [2 "A"] [2 "B"]
                             [3 "B"] [3 "C"] [3 "D"] [3 "E"]
                             [4 "A"] [4 "C"] [4 "D"]
                             [5 "C"] [5 "D"] [5 "E"]
                             [6 "A"] [6 "B"] [6 "C"] [6 "D"] [6 "E"]})

(def testctx (make-context test-objs
                           test-attrs
                           test-inc))

(def testmctx (make-metric-context test-objs test-attrs test-inc))


(deftest test-create-metric-context

  (let [mctx (make-metric-context test-objs test-attrs test-inc)
        mctx-with-metrics (make-metric-context test-objs 
                                                     test-attrs 
                                                     test-inc
                                                     {:o-metric-1 object-metric-1 :o-metric-2 object-metric-2} 
                                                     {:a-metric-1 attribute-metric-1})]
     (is (= (context mctx) testctx))
     (is (= (context mctx-with-metrics) testctx))
     
     (is (contains? (object-metrics mctx) :o-hamm))
     (is (contains? (attribute-metrics mctx) :a-hamm))

     (is (contains? (object-metrics mctx-with-metrics) :o-metric-1))
     (is (contains? (object-metrics mctx-with-metrics) :o-metric-2))
     (is (contains? (attribute-metrics mctx-with-metrics) :a-metric-1)))
)

(deftest test-convert-to-metric-context

  (let [mctx (convert-to-metric-context testctx)
        mctx-with-metrics (convert-to-metric-context testctx 
                                                     {:o-metric-1 object-metric-1 :o-metric-2 object-metric-2} 
                                                     {:a-metric-1 attribute-metric-1})]
     (is (= (context mctx) testctx))
     (is (= (context mctx-with-metrics) testctx))
     
     (is (contains? (object-metrics mctx) :o-hamm))
     (is (contains? (attribute-metrics mctx) :a-hamm))

     (is (contains? (object-metrics mctx-with-metrics) :o-metric-1))
     (is (contains? (object-metrics mctx-with-metrics) :o-metric-2))
     (is (contains? (attribute-metrics mctx-with-metrics) :a-metric-1)))
)

(deftest test-add-metrics

  (let [mctx (convert-to-metric-context testctx 
                                        {:o-metric-1 object-metric-1 :o-metric-2 object-metric-2} 
                                        {:a-metric-1 attribute-metric-1})
        mctx2 (add-object-metrics mctx {:o-metric-2 object-metric-2 :o-metric-3 object-metric-3})
        mctx3 (add-attribute-metrics mctx2 {:a-metric-2 attribute-metric-2 :a-metric-3 attribute-metric-3})]

        (is (contains? (object-metrics mctx3) :o-metric-1))  
        (is (contains? (object-metrics mctx3) :o-metric-2)) 
        (is (contains? (object-metrics mctx3) :o-metric-3))
        (is (= (count (object-metrics mctx3)) 4)) 

        (is (contains? (attribute-metrics mctx3) :a-metric-1)) 
        (is (contains? (attribute-metrics mctx3) :a-metric-2)) 
        (is (contains? (attribute-metrics mctx3) :a-metric-3))
        (is (= (count (attribute-metrics mctx3)) 4)))
)

(deftest test-remove-metrics

  (let [mctx (convert-to-metric-context testctx 
                                        {:o-metric-1 object-metric-1 :o-metric-2 object-metric-2} 
                                        {:a-metric-1 attribute-metric-1})
        mctx2 (remove-object-metric mctx :o-metric-1)
        mctx3 (remove-object-metric mctx2 :o-metric-3)
        mctx4 (remove-attribute-metric mctx3 :a-metric-1)]

(println (object-metrics mctx4))
        (is (not (contains? (object-metrics mctx4) :o-metric-1)))  
        (is (contains? (object-metrics mctx4) :o-metric-2)) 
        (is (= (count (object-metrics mctx4)) 2)) 

        (is (not (contains? (attribute-metrics mctx4) :a-metric-1))) 
        (is (= (count (attribute-metrics mctx4)) 1)))
)

(deftest test-object-dist-hamming

  (is (= (object-distance testmctx :o-hamm 1 2) 2))  
  (is (= (object-distance testmctx :o-hamm 2 6) 3)) 

  (is (= (max-object-distance testmctx :o-hamm) 5))
  (is (= (max-object-distance testmctx :o-hamm #{1 3 4 5 6}) 3))
  
  (is (= (min-object-distance testmctx :o-hamm) 1))
  (is (= (max-object-distance testmctx :o-hamm #{1 3}) 2))

  (is (= (average-object-distance testmctx :o-hamm)37/15 ))
  (is (= (average-object-distance testmctx :o-hamm #{1 2 3}) 8/3))

)

(deftest test-attribute-dist-hamming

  (is (= (attribute-distance testmctx :a-hamm "A" "C") 4))  
  (is (= (attribute-distance testmctx :a-hamm "B" "D") 3)) 

  (is (= (max-attribute-distance testmctx :a-hamm) 4))
  (is (= (max-attribute-distance testmctx :a-hamm #{"A" "B" "D"}) 3))
  
  (is (= (min-attribute-distance testmctx :a-hamm) 1))
  (is (= (max-attribute-distance testmctx :a-hamm #{"A" "B"}) 2))

  (is (= (average-attribute-distance testmctx :a-hamm) 13/5 ))
  (is (= (average-attribute-distance testmctx :a-hamm #{"A" "B" "C"}) 10/3))

)

(deftest test-confusion-matrices

  (object-confusion-matrix testmctx :o-hamm)
  (attribute-confusion-matrix testmctx :a-hamm)

  (object-confusion-matrix testmctx :o-hamm :norm)
  (attribute-confusion-matrix testmctx :a-hamm :norm)
)
