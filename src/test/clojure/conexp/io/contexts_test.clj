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

(deftest test-context-out-in
  (with-testing-data [ctx contexts-oi,
                      fmt (remove #{:binary-csv :anonymous-burmeister}
                                  (list-context-formats))]
    (try (= ctx (out-in ctx 'context fmt))
         (catch UnsupportedOperationException _ true))))

;;

(def- contexts-oioi
  "Contexts to use for out-in-out-in testing"
  [(make-context #{1 2 3} #{4 5 6} <),
   (make-context #{'a} #{'+} #{['a '+]})])

(deftest test-context-out-in-out-in
  (with-testing-data [ctx contexts-oioi,
                      fmt (remove #{:anonymous-burmeister} (list-context-formats))]
    (try (out-in-out-in-test ctx 'context fmt)
         (catch UnsupportedOperationException _ true))))

;;

(def- contexts-with-empty-columns
  "Context with empty columns, to test for corner cases"
  [(null-context #{1 2 3 4}),
   (null-context #{1 2 3}),
   (null-context #{}),
   (make-context #{1 2 3} #{1 2 3} #{[1 2] [2 3] [3 2]})])

(deftest test-empty-columns
  (with-testing-data [ctx contexts-with-empty-columns,
                      fmt (list-context-formats)]
    (try (out-in-out-in-test ctx 'context fmt)
         (catch UnsupportedOperationException _ true))))

;;

(deftest test-for-random-contexts
  (with-testing-data [ctx (random-contexts 20 50),
                      fmt (remove #{:anonymous-burmeister} (list-context-formats))]
    (try (out-in-out-in-test ctx 'context fmt)
         (catch UnsupportedOperationException _ true))))

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
                         ["n1" "0"]["n1" "1"]})))
  )

;;;

(deftest test-bug-001
  (if-not (.exists (java.io.File. "testing-data/nn_5.half.cex"))
    (warn "Could not verify bug 001, testing file not found.")
    (let [ctx (read-context "testing-data/nn_5.half.cex")]
      (is (= 42 (count (attributes ctx))))
      (is (= 10 (count (objects ctx))))
      (is (= 120 (count (incidence-relation ctx)))))))

;;;

nil
