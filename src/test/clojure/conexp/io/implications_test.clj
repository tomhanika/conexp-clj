(ns conexp.io.implications-test
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications
        conexp.io.implications
        conexp.io.util-test)
  (:use clojure.test))

;;;

(def- contexts-oi
  "Context to use for out-in testing"
  (make-context #{"a" "b" "c"}
                  #{"1" "2" "3"}
                  #{["a" "1"] ["a" "3"]
                    ["b" "2"] ["c" "3"]}))

(def- implications-oi
  "Implications to use for out-in testing"
  [(canonical-base contexts-oi)])

(deftest test-implications-out-in
  (with-testing-data [impl implications-oi,
                      fmt (list-implication-formats)]
    (try (= impl (out-in impl 'implication fmt))
         (catch UnsupportedOperationException _ true))))
  
(deftest test-implications-out-in-out-in
  (with-testing-data [impl implications-oi,
                      fmt (list-implication-formats)]
    (try (out-in-out-in-test impl 'implication fmt)
         (catch UnsupportedOperationException _ true))))
