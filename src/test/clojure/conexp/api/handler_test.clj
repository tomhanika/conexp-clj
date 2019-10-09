;; Copyright ⓒ the conexp-clj developers; all rights reserved.                  
;; The use and distribution terms for this software are covered by the          
;; Eclipse Public Lic:ense 1.0 (http://opensource.org/licenses/eclipse-1.0.php)  
;; which can be found in the file LICENSE at the root of this distribution.     
;; By using this software in any fashion, you are agreeing to be bound by       
;; the terms of this license.                                                   
;; You must not remove this notice, or any other, from this software.           

(ns conexp.api.handler-test
	(:use conexp.main
        conexp.layouts.base
        conexp.api.handler
        conexp.api.util-test)
	(:use clojure.test))

(apply use conexp-clj-namespaces)

;;; Generic tests

(deftest test-empty-request 
  (is (= (mock-request {}) {})))

(deftest test-generic-request 
  (is (= (mock-request {:function {:type "function"
                                   :name "+" 
                                   :args ["eins", "zwei"]} 
                        :eins {:type "int" :data 1}
                        :zwei {:type "int" :data 2}}) 
         {:function {:status 0
                     :result 3}})))

(deftest test-generic-request-multiple-independent-functions 
  (is (= (mock-request {:function1 {:type "function"
                                    :name "+" 
                                    :args ["eins", "zwei"]}
                        :function2 {:type "function"
                                    :name "-" 
                                    :args ["eins", "zwei"]} 
                        :eins {:type "int" :data 1}
                        :zwei {:type "int" :data 2}}) 
         {:function1 {:status 0
              :result 3}
          :function2 {:status 0
              :result -1}})))

(deftest test-generic-request-multiple-dependent-functions 
  (is (= (mock-request {:function1 {:type "function"
                                    :name "+" 
                                    :args ["eins", "zwei"]}
                        :function2 {:type "function"
                                    :name "-" 
                                    :args ["function1", "zwei"]} 
                        :eins {:type "int" :data 1}
                        :zwei {:type "int" :data 2}}) 
         {:function1 {:status 0
              :result 3}
          :function2 {:status 0
              :result 1}})))

(deftest test-generic-request-id
  (is (= (mock-request {:function {:type "function"
                                   :name "+" 
                                   :args ["eins", "zwei"]} 
                        :id 42
                        :eins {:type "int" :data 1}
                        :zwei {:type "int" :data 2}}) 
         {:function {:status 0
                     :result 3}
          :id 42})))

(deftest test-generic-request-error
  (is (= (mock-request {:function {:type "function"
                                   :name "+" 
                                   :args ["eins"]} 
                        :eins {:type "int" :data "a"}}) 
         {:function {:status 1
                     :msg (try (+ "a") 
                           (catch Exception e (.getMessage e)))}})))

(deftest test-whitelist-error
  (is (= (mock-request {:function {:type "function"
                                   :name "map"
                                   :args []}})
         {:function {:status 1
                     :msg "Function name not allowed."}})))

;;; Conexp data types
;; if one conexp-clj operation works with a data type it's assumed all do

;;Context
(deftest test-context-file-read
  (is (= (mock-request {:function {:type "function"
                                   :name "concepts"
                                   :args ["ctx1"]}
                        :ctx1 {:type "context_file"
                               :data (slurp "testing-data/Animals.ctx")}})
         {:function {:status 0
                     :result (mapv 
                              #(mapv vec %) 
                              (concepts (read-context 
                                         "testing-data/Animals.ctx")))}})))

(deftest test-context-write
  (let [result (mock-request {:function {:type "function"
                                         :name "make-context"
                                         :args ["objs" "atts" "inc"]}
                              :objs {:type "list"
                                     :data ["a" "b"]} 
                              :atts {:type "list"
                                     :data ["1" "2"]} 
                              :inc {:type "list"
                                     :data [["a" "1"]["b" "2"]]}})
        ctx (:result (:function result))]
    (is (= (make-context (:objects ctx)(:attributes ctx)(:incidence ctx))
           (make-context ["a" "b"]["1" "2"][["a" "1"]["b" "2"]])))))

(deftest test-context-read
  (let [result (mock-request {:function {:type "function"
                                         :name "concepts"
                                         :args ["ctx1"]}
                              :ctx1 {:type "context"
                                     :data {:objects ["a" "b"]
                                            :attributes ["1" "2"]
                                            :incidence [["a" "1"]["b" "2"]]}}})
        ctx (:result (:function result))]
    (is (= (map #(mapv set %) ctx)
           (concepts (make-context ["a" "b"]["1" "2"][["a" "1"]["b" "2"]]))))))

;;Many valued context
(deftest test-mv-context-file-read
  (is (= (mock-request {:function {:type "function"
                                   :name "values-of-attribute"
                                   :args ["ctx1" "zwei"]}
                        :ctx1 {:type "mv_context_file"
                               :data (slurp "testing-data/house-votes-84.data")}
                        :zwei {:type "int"
                               :data 10}})
         {:function {:status 0
                     :result (map str (values-of-attribute 
                                       (read-mv-context 
                                       "testing-data/house-votes-84.data")
                                       10))}})))

(deftest test-mv-context-write
  (let [result (mock-request {:function {:type "function"
                                         :name "make-mv-context"
                                         :args ["objs" "atts" "inc"]}
                              :objs {:type "list"
                                     :data ["a"]} 
                              :atts {:type "list"
                                     :data ["1" "2"]} 
                              :inc {:type "map"
                                     :data {["a" "1"] 2 ["a" "2"] 5}}})
        ctx (:result (:function result))]
    (is (= ctx 
           {:objects ["a"]
            :attributes ["1" "2"]
            :incidence {(keyword (str ["a" "1"])) 2 
                        (keyword (str ["a" "2"])) 5}}))))

(deftest test-mv-context-read
  (let [result (mock-request {:function {:type "function"
                                         :name "values-of-object"
                                         :args ["ctx1" "zwei"]}
                              :ctx1 {:type "mv_context"
                                     :data {:objects ["a"]
                                            :attributes ["1" "2"]
                                            :incidence
                                             {["a" "1"] 2 ["a" "2"] 5}}}
                              :zwei {:type "string"
                                     :data "a"}})
        values (:result (:function result))]
    (is (= values
           (write-data 
            (values-of-object 
             (make-mv-context 
               ["a"]["1" "2"]{["a" "1"] 2 ["a" "2"] 5})
             "a"))))))

;Lattice
(deftest test-lattice-write
  (let [result (mock-request {:function {:type "function"
                                         :name "concept-lattice"
                                         :args ["ctx1"]}
                              :ctx1 {:type "context_file"
                                     :data (slurp "testing-data/myctx.cxt")}})
        lat (:result (:function result))]
    (is (= (make-lattice-nc (map #(mapv set %) (:nodes lat)) 
                            (map 
                             (fn [a] (map (fn [b] (mapv set b))a))
                             (:edges lat)))
           (concept-lattice (read-context "testing-data/myctx.cxt"))))))

(deftest test-lattice-read
  (let [data (concept-lattice (read-context "testing-data/myctx.cxt"))
        result (mock-request {:function {:type "function"
                                         :name "dual-lattice"
                                         :args ["lat1"]}
                              :lat1 {:type "lattice"
                                     :data (write-data data)}})
        lat (:result (:function result))]
    (is (= (make-lattice-nc (map #(mapv set %) (:nodes lat)) 
                            (map 
                             (fn [a] (map (fn [b] (mapv set b))a))
                             (:edges lat)))
           (dual-lattice data)))))

;;Implications
(deftest test-implication-write
  (let [result (mock-request {:function {:type "function"
                                         :name "make-implication"
                                         :args ["premise" "conclusion"]}
                              :premise {:type "list"
                                        :data ["a" "b"]} 
                              :conclusion {:type "list"
                                           :data ["2" "1"]}})
        impl (:result (:function result))]
    (is (= (make-implication (first impl)(second impl))
           (make-implication ["a" "b"]["2" "1"])))))

(deftest test-implication-read
  (let [result (mock-request {:function {:type "function"
                                         :name "premise"
                                         :args ["impl1"]}
                              :impl1 {:type "implication"
                                     :data [["a"] ["b"]]}})
        premise (:result (:function result))]
    (is (= premise
           ["a"]))))


(deftest test-implications-write
  (let [context (read-context "testing-data/myctx.cxt")
        result (mock-request {:function {:type "function"
                                         :name "canonical-base"
                                         :args ["ctx1"]}
                              :ctx1 {:type "context"
                                     :data (write-data context)}})
        impls (:result (:function result))]
    (is (= (map #(make-implication (first %) (last %)) impls)
           (canonical-base context)))))

(deftest test-implications-read
  (let [result (mock-request {:function {:type "function"
                                         :name "minimal-implication-set?"
                                         :args ["impl1"]}
                              :impl1 {:type "implications"
                                     :data [[["a"] ["b"]]
                                            [["1"] ["2"]]]}})
        impls (:result (:function result))]
    (is (= impls
           (minimal-implication-set? 
            #{(make-implication ["a"] ["b"]) 
              (make-implication ["1"] ["2"])})))))

;Layout
(deftest test-layout-write
  (let [lat (make-lattice #{1 2 3 4}
                          #{[1 2][1 3][2 4][3 4][1 4][1 1][2 2][3 3][4 4]})
        pos (hash-map 1 [0 0] 2 [-1 1] 3 [1 1] 4 [0 2])
        edge #{[1 2][1 3][2 4][3 4]}
        result (mock-request {:function {:type "function"
                                         :name "make-layout"
                                         :args ["lattice" "positions" "edges"]}
                              :lattice {:type "lattice"
                                        :data (write-data lat)}
                              :positions {:type "map"
                                          :data pos}
                              :edges {:type "list"
                                      :data edge}})
        layout (:result (:function result))]
    (is (= (make-layout-nc 
               ;; Lattice object
               (make-lattice-nc (:nodes (:lattice layout))
                                (:edges (:lattice layout))) 
               ;; remove colons from keys
               (read-data {:type "map" :data (:positions layout)})
               ;; cast vector to set, as JSON only supports lists
               (into #{} (:connections layout)))
           (make-layout lat pos edge)))))

(deftest test-layout-read
  (let [lat (make-lattice #{1 2 3 4}
                          #{[1 2][1 3][2 4][3 4][1 4][1 1][2 2][3 3][4 4]})
        pos (hash-map 1 [0 0] 2 [-1 1] 3 [1 1] 4 [0 2])
        new-pos (hash-map 1 [0 0] 2 [-1 1] 3 [1 1] 4 [0 3])
        edge #{[1 2][1 3][2 4][3 4]}
        result (mock-request {:function {:type "function"
                                         :name "update-positions"
                                         :args ["layout" "new-pos"]}
                              :layout {:type "layout"
                                       :data (write-data 
                                              (make-layout lat pos edge))}
                              :new-pos {:type "map"
                                        :data new-pos}})
        layout (:result (:function result))]
    (is (= (make-layout-nc 
               ;; Lattice object
               (make-lattice-nc (:nodes (:lattice layout))
                                (:edges (:lattice layout))) 
               ;; remove colons from keys
               (read-data {:type "map" :data (:positions layout)})
               ;; cast vector to set, as JSON only supports lists
               (into #{} (:connections layout)))
           (make-layout lat new-pos edge)))))

(deftest test-layout-read-write-label
  (let [lat (make-lattice #{1 2 3 4}
                          #{[1 2][1 3][2 4][3 4][1 4][1 1][2 2][3 3][4 4]})
        pos (hash-map 1 [0 0] 2 [-1 1] 3 [1 1] 4 [0 2])
        new-pos (hash-map 1 [0 0] 2 [-2 1] 3 [1 1] 4 [0 2])
        edge #{[1 2][1 3][2 4][3 4]}
        up (hash-map 1 ["a" nil] 2 ["b" nil] 3 ["c" nil] 4 ["d" nil])
        lo (hash-map 1 ["e" nil] 2 ["f" nil] 3 ["g" nil] 4 ["h" nil])
        result (mock-request {:function {:type "function"
                                         :name "update-positions"
                                         :args ["lay" "new-pos"]}
                              :lay {:type "layout"
                                    :data (write-data 
                                           (make-layout lat pos edge up lo))}
                              :new-pos {:type "map"
                                        :data new-pos}})
        layout (:result (:function result))]
    (is (= (make-layout-nc 
               ;; Lattice object
               (make-lattice-nc (:nodes (:lattice layout))
                                (:edges (:lattice layout))) 
               ;; remove colons from keys
               (read-data {:type "map" :data (:positions layout)})
               ;; cast vector to set, as JSON only supports lists
               (into #{} (:connections layout))
               (read-data {:type "map" :data (:upper-labels layout)})
               (read-data {:type "map" :data (:lower-labels layout)}))
           (make-layout lat new-pos edge up lo)))))

;;;

nil
