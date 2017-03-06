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
                      fmt (remove #{:binary-csv}
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
                      fmt (list-context-formats)]
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
                      fmt (list-context-formats)]
    (try (out-in-out-in-test ctx 'context fmt)
         (catch UnsupportedOperationException _ true))))

;;;

(deftest test-bug-001
  (if-not (.exists (java.io.File. "stuff/testing-data/nn_5.half.cex"))
    (warn "Could not verify bug 001, testing file not found.")
    (let [ctx (read-context "stuff/testing-data/nn_5.half.cex")]
      (is (= 42 (count (attributes ctx))))
      (is (= 10 (count (objects ctx))))
      (is (= 120 (count (incidence-relation ctx)))))))

;;;

nil
