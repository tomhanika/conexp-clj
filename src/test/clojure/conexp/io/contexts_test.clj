;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.contexts-test
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.many-valued-contexts
        conexp.io.contexts
        conexp.io.util-test)
  (:use clojure.test))

;;;

(def- contexts-oi
  "Context to use for out-in testing"
  [(make-context #{"a" "b" "c"}
                 #{"1" "2" "3"}
                 #{["a" "1"] ["a" "3"]
                   ["b" "2"] ["c" "3"]}),
   (null-context #{})])

(def- contexts-oi-fcalgs
  [(make-context #{0 1}
                 #{0 1}
                 #{[0 1] [1 0] [1 1]})])

(deftest test-context-out-in
  (with-testing-data [ctx contexts-oi,
                      fmt (remove #{:binary-csv :anonymous-burmeister :fcalgs :named-binary-csv}
                                  (list-context-formats))]
    (= ctx (out-in ctx 'context fmt)))
  ;; named-binary-csv cannot export empty context
  (with-testing-data [ctx [(first contexts-oi)],
                      fmt #{:named-binary-csv}]
    (= ctx (out-in ctx 'context fmt)))
  ;; fcalgs can only store contexts with integral objects and attributes >=0,
  ;; and thus needs to be tested with another context
  (with-testing-data [ctx contexts-oi-fcalgs,
                      fmt #{:fcalgs}]
    (= ctx (out-in ctx 'context fmt))))

(defn- possible-isomorphic?
  "Test for equality of some criteria for context isomorphy.  Namely, number of objects and attributes, the size of the incidence relation, and the number of concepts.  Only use with small contexts."
  [ctx1 ctx2]
  (are [x y] (= (count x) (count y))
     (objects ctx1)  (objects ctx2)
     (attributes ctx1) (attributes ctx2)
     (incidence-relation ctx1) (incidence-relation ctx2)
     (concepts ctx1) (concepts ctx2)))

(deftest test-isomorphic-out-in
  (with-testing-data [ctx contexts-oi
                      fmt #{:anonymous-burmeister}]
    (possible-isomorphic? ctx (out-in ctx 'context fmt)))
  ;; binary-csv cannot export empty context
  (with-testing-data [ctx [(first contexts-oi)]
                      fmt #{ :binary-csv}]
    (possible-isomorphic? ctx (out-in ctx 'context fmt))))
;;

(def- contexts-oioi
  "Contexts to use for out-in-out-in testing"
  [(make-context #{1 2 3} #{4 5 6} <),
   (make-context #{'a} #{'+} #{['a '+]})])

(deftest test-context-out-in-out-in
  (with-testing-data [ctx contexts-oioi,
                      fmt (remove #{:anonymous-burmeister :fcalgs} (list-context-formats))]
    (out-in-out-in-test ctx 'context fmt))
  ;; fcalgs can only store contexts with integral objects and attributes >=0,
  ;; and thus needs to be tested with another context
  (with-testing-data [ctx contexts-oi-fcalgs,
                      fmt #{:fcalgs}]
    (out-in-out-in-test ctx 'context fmt)))

(deftest test-anonymous-burmeister-out-in-out-in
  (with-testing-data [ctx contexts-oioi
                      fmt #{:anonymous-burmeister}]
    (let [ctx1 (out-in ctx 'context fmt)
          ctx2 (out-in ctx1 'context fmt)]
      (possible-isomorphic? ctx1 ctx2 ))))

;;

(def- contexts-with-empty-columns
  "Context with empty columns, to test for corner cases"
  [(null-context #{}),
   (null-context #{1 2 3 4}),
   (null-context #{1 2 3}),
   (make-context #{1 2 3} #{1 2 3} #{[1 2] [2 3] [3 2]})])

(deftest test-empty-columns
  (with-testing-data [ctx contexts-with-empty-columns,
                      ;; colibri and fcalgs cannot handle empty rows or columns
                      ;; empty contexts cannot be exported to named-binary-csv or binary-csv
                      fmt (remove #{:colibri :fcalgs :named-binary-csv :binary-csv} (list-context-formats))]
    (out-in-out-in-test ctx 'context fmt))
  (with-testing-data [ctx (rest contexts-with-empty-columns),
                      fmt #{:named-binary-csv :binary-csv}]
    (out-in-out-in-test ctx 'context fmt)))

;;

(def- random-test-contexts
  (vec (random-contexts 20 50)))

(deftest test-for-random-contexts
  (with-testing-data [ctx random-test-contexts,
                      ;; colibri and fcalgs cannot handle empty rows or columns
                      ;; binary-csv and named-binary-csv cannot handle empty contexts
                      fmt (remove #{:anonymous-burmeister :colibri :fcalgs :binary-csv :named-binary-csv}
                                  (list-context-formats))]
    (out-in-out-in-test ctx 'context fmt))
  (with-testing-data [ctx (filter #(not (empty? (incidence-relation %))) 
                                  random-test-contexts),
                      fmt #{:binary-csv :named-binary-csv}]
    (out-in-out-in-test ctx 'context fmt)))

(deftest test-anonymous-burmeister-out-in-out-in-for-random-contexts
  (with-testing-data [ctx (random-contexts 20 10),
                      fmt #{:anonymous-burmeister}]
    (let [ctx1 (out-in ctx 'context fmt)
          ctx2 (out-in ctx1 'context fmt)]
      (possible-isomorphic? ctx1 ctx2 ))))

;;; GraphML (seperate testing as it can only be read but not written)

(deftest test-graphml
  (is (thrown-with-msg? 
        IllegalArgumentException
        #"Specified file does not contain valid XML."
        (read-context "testing-data/Booth.cxt" :graphml)))
  (is (thrown-with-msg? 
        IllegalArgumentException
        #"Specified file not found."
        (read-context "testing-data/NotExisting.nope" :graphml)))
  (is (thrown-with-msg? 
        IllegalArgumentException
        #"All edges and hyperedges of an hypergraph need ids."
        (read-context "testing-data/graphml/hyper.graphml" :graphml)))
  (is (thrown-with-msg? 
        IllegalArgumentException
        #"XML file does not contain GraphML."
        (read-context "testing-data/graphml/fake.graphml" :graphml)))
  (is (= (first (read-context "testing-data/graphml/simple.graphml" :graphml))
         (make-context #{"n0" "n1" "n2" "n3" "n4" "n5"} 
                       #{"n0" "n1" "n2" "n3" "n4" "n5"}
                       #{["n0" "n2"]["n1" "n2"]["n2" "n3"]
                         ["n2" "n0"]["n2" "n1"]["n3" "n2"]})))
  (is (= (first
           (read-context "testing-data/graphml/hyperids.graphml" :graphml))
         (make-context #{"n0" "n1" "n2" "n3"} 
                       #{"1" "3" "4"} 
                       #{["n0" "1"]["n1" "1"]["n2" "1"]
                         ["n1" "3"]["n3" "3"]
                         ["n0" "4"]["n4" "4"]})))
  (is (= (first
           (read-context "testing-data/graphml/fuzzy.graphml" :graphml))
         (make-mv-context #{"n0" "n1" "n2"} 
                          #{"n0" "n1" "n2"} 
                          #{["n0" "n1" "1.0"]["n0" "n2" "1.0"]
                            ["n1" "n2" "2.0"]
                            ["n1" "n0" "1.0"]["n2" "n0" "1.0"]
                            ["n2" "n1" "2.0"]})))
  (is (thrown-with-msg? 
        IllegalArgumentException
        #"Multiple data values for edges are not supported."
        (read-context "testing-data/graphml/fuzzymulti.graphml" :graphml)))
  (is (thrown-with-msg? 
        IllegalArgumentException
        #"Only single values are supported as edge data."
        (read-context "testing-data/graphml/svg.graphml" :graphml)))
  (is (= (first
           (read-context "testing-data/graphml/port.graphml" :graphml))
         (make-context #{"n0" "n1"} 
                       #{"0" "1"} 
                       #{["n0" "0"]["n0" "1"]
                         ["n1" "0"]["n1" "1"]}))))

;;;

(deftest test-automatically-identify-input-format-galicia
  "Test if other input formats throw an error when searching for an input format matching the input file."
  (if-not (.exists (java.io.File. "testing-data/galicia2.bin.xml"))
    (warn "Could not verify identifying :galicia input format. Testing file not found.")
    (let [ctx (read-context "testing-data/galicia2.bin.xml")]
      (is (= 10 (count (attributes ctx))))
      (is (= 10 (count (objects ctx))))
      (is (= 27 (count (incidence-relation ctx)))))))

(deftest test-json-not-matching-schema
  "Read a json format that does not match the given schema."
  (if-not (.exists (java.io.File. "testing-data/digits-lattice.json"))
    (warn "Could not verify failing validation of context schema. Testing file not found.") 
    (is (thrown?
         AssertionError
         (read-context "testing-data/digits-lattice.json" :json)))))

(deftest test-json-matching-schema
  "Read a json format that matches the given schema."
  (let [file "testing-data/digits-context1.1.json"]
    (if-not (.exists (java.io.File. file))
      (warn "Could not verify validation of context schema. Testing file not found.")
      (let [ctx (read-context file :json)]
        (is (= 7 (count (attributes ctx))))
        (is (= 10 (count (objects ctx))))
        (is (= 47 (count (incidence-relation ctx))))))))

;;;

(deftest test-bug-001
  (if-not (.exists (java.io.File. "testing-data/nn_5.half.cex"))
    (warn "Could not verify bug 001, testing file not found.")
    (let [ctx (read-context "testing-data/nn_5.half.cex")]
      (is (= 42 (count (attributes ctx))))
      (is (= 10 (count (objects ctx))))
      (is (= 120 (count (incidence-relation ctx)))))))

;;;

(deftest test-identify-input-format
  "Test if the automatic identification of the file format works correctly."
  (with-testing-data [ctx contexts-oi,
                      fmt (remove #{:named-binary-csv :anonymous-burmeister 
                                    :binary-csv :fcalgs} 
                                  (list-context-formats))]
    (= ctx (out-in-without-format ctx 'context fmt)))
  
  ;; The null-context in contexts-io cannot be exported to :named-binary-csv 
  ;; format.
  (with-testing-data [ctx [(first contexts-oi)],
                      fmt #{:named-binary-csv}]
    (= ctx (out-in-without-format ctx 'context fmt)))
  
  ;; During writing / reading in :anonymous-burmeister format, object and 
  ;; attribute names get lost and equality cannot be tested any more.
  (with-testing-data [ctx contexts-oi,
                      fmt #{:anonymous-burmeister}]
    (possible-isomorphic? ctx (out-in-without-format ctx 'context fmt)))
  
  ;; The null-context in contexts-io cannot be exported to :binary-csv format.
  ;; During writing / reading in :binary-csv format, object and attribute names 
  ;; get lost and equality cannot be tested any more.
  (with-testing-data [ctx [(first contexts-oi)],
                      fmt #{:binary-csv}]
    (possible-isomorphic? ctx (out-in-without-format ctx 'context fmt)))
  
  ;; fcalgs test with another context
  (with-testing-data [ctx contexts-oi-fcalgs,
                      fmt #{:fcalgs}]
    (= ctx (out-in-without-format ctx 'context fmt))))

(deftest test-ctx->json
  ;; test that attributes with empty column are not dropped
  (let [K (make-context-from-matrix [1 2] [:a :b] [1 0 0 0])]
    (is (= (ctx->json K)
           {:attributes #{:a :b}
            :objects #{1 2}
            :incidence #{[1 :a]}}))))

nil
