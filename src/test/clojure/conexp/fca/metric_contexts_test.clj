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



(def cities-ctx (make-context #{"Washington, D.C." "Berlin" "Beijing" "Cairo" "Canberra" "Brasilia"}
                              #{"Population > 1M" "Population > 3M" "Population > 10M"
                                "Area > 100km^2" "Area > 1000km^2" "Area > 10000km^2"}

                              #{["Washington, D.C." "Area > 100km^2"]

                                ["Berlin" "Population > 1M"] ["Berlin" "Population > 3M"] ["Berlin" "Area > 100km^2"]

                                ["Beijing" "Population > 1M"] ["Beijing" "Population > 3M"] ["Beijing" "Population > 10M"]
                                ["Beijing" "Area > 100km^2"] ["Beijing" "Area > 1000km^2"] ["Beijing" "Area > 10000km^2"]

                                ["Cairo" "Population > 1M"] ["Cairo" "Population > 3M"] ["Cairo" "Population > 10M"] 
                                ["Cairo"  "Area > 100km^2"] ["Cairo" "Area > 1000km^2"]

                                ["Canberra" "Area > 100km^2"]

                                ["Brasilia" "Population > 1M"] ["Brasilia" "Area > 100km^2"] ["Brasilia" "Area > 1000km^2"]}))

(def cities-mctx (convert-to-metric-context cities-ctx))

(def distance-map {"Washington, D.C." {"Washington, D.C." 0
                                       "Berlin" 7611
                                       "Beijing" 11145
                                       "Cairo" 9348
                                       "Canberra" 15945
                                       "Brasilia" 6791} 
                   "Berlin" {"Washington, D.C." 7611
                             "Berlin" 0
                             "Beijing" 3754
                             "Cairo" 2892
                             "Canberra" 16066
                             "Brasilia" 9593}
                   "Beijing" {"Washington, D.C." 11145
                              "Berlin" 3754
                              "Beijing" 0
                              "Cairo" 7542
                              "Canberra" 9011
                              "Brasilia" 16929}
                   "Cairo" {"Washington, D.C." 9348
                            "Berlin" 2892
                            "Beijing" 7542
                            "Cairo" 0
                            "Canberra" 14266
                            "Brasilia" 9877}
                   "Canberra" {"Washington, D.C." 15945
                               "Berlin" 16066
                               "Beijing" 9011
                               "Cairo" 14266
                               "Canberra" 0
                               "Brasilia" 14059}
                   "Brasilia" {"Washington, D.C." 6791
                               "Berlin" 9593
                               "Beijing" 16929
                               "Cairo" 9877
                               "Canberra" 14059
                               "Brasilia" 0}})

(defn distance-metric [a b]((distance-map a) b))



(deftest test-create-metric-context

    (let [mctx (make-metric-context test-objs test-attrs test-inc)
          mctx-with-metrics (make-metric-context test-objs 
                                                 test-attrs 
                                                 test-inc
                                                 {:o-metric-1 object-metric-1 :o-metric-2 object-metric-2} 
                                                 {:a-metric-1 attribute-metric-1})]
     (is (= (context mctx) testctx))
     (is (= (context mctx-with-metrics) testctx))

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
        (is (= (count (object-metrics mctx3)) 3)) 

        (is (contains? (attribute-metrics mctx3) :a-metric-1)) 
        (is (contains? (attribute-metrics mctx3) :a-metric-2)) 
        (is (contains? (attribute-metrics mctx3) :a-metric-3))
        (is (= (count (attribute-metrics mctx3)) 3)))
)

(deftest test-remove-metrics

  (let [mctx (convert-to-metric-context testctx 
                                        {:o-metric-1 object-metric-1 :o-metric-2 object-metric-2} 
                                        {:a-metric-1 attribute-metric-1})
        mctx2 (remove-object-metric mctx :o-metric-1)
        mctx3 (remove-object-metric mctx2 :o-metric-3)
        mctx4 (remove-attribute-metric mctx3 :a-metric-1)]

        (is (not (contains? (object-metrics mctx4) :o-metric-1)))  
        (is (contains? (object-metrics mctx4) :o-metric-2)) 
        (is (= (count (object-metrics mctx4)) 1)) 

        (is (not (contains? (attribute-metrics mctx4) :a-metric-1))) 
        (is (= (count (attribute-metrics mctx4)) 0)))
)

(deftest test-object-dist-hamming

  (is (= (object-distance testmctx (object-hamming testmctx) 1 2) 2))  
  (is (= (object-distance testmctx (object-hamming testmctx) 2 6) 3)) 

  (is (= (max-object-distance testmctx (object-hamming testmctx)) 5))
  (is (= (max-object-distance testmctx (object-hamming testmctx) #{1 3 4 5 6}) 3))
  
  (is (= (min-object-distance testmctx (object-hamming testmctx)) 1))
  (is (= (max-object-distance testmctx (object-hamming testmctx) #{1 3}) 2))

  (is (= (average-object-distance testmctx (object-hamming testmctx))37/15 ))
  (is (= (average-object-distance testmctx (object-hamming testmctx) #{1 2 3}) 8/3))

)

(deftest test-attribute-dist-hamming

  (is (= (attribute-distance testmctx (attribute-hamming testmctx) "A" "C") 4))  
  (is (= (attribute-distance testmctx (attribute-hamming testmctx) "B" "D") 3)) 

  (is (= (max-attribute-distance testmctx (attribute-hamming testmctx)) 4))
  (is (= (max-attribute-distance testmctx (attribute-hamming testmctx) #{"A" "B" "D"}) 3))
  
  (is (= (min-attribute-distance testmctx (attribute-hamming testmctx)) 1))
  (is (= (max-attribute-distance testmctx (attribute-hamming testmctx) #{"A" "B"}) 2))

  (is (= (average-attribute-distance testmctx (attribute-hamming testmctx)) 13/5 ))
  (is (= (average-attribute-distance testmctx (attribute-hamming testmctx) #{"A" "B" "C"}) 10/3))

)

(deftest test-confusion-matrices

  (object-confusion-matrix testmctx (object-hamming testmctx))
  (attribute-confusion-matrix testmctx (attribute-hamming testmctx))

  (object-confusion-matrix testmctx (object-hamming testmctx) :norm)
  (attribute-confusion-matrix testmctx (attribute-hamming testmctx) :norm)
)
