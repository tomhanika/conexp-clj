;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

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

(def- contexts-oioi
  "Contexts to use for out-in-out-in testing"
  [(make-context #{1 2 3} #{4 5 6} <),
   (make-context #{'a} #{'+} #{['a '+]})])

(def- implications-oioi
  (mapv canonical-base contexts-oioi))
  
(deftest test-implications-out-in-out-in
  (with-testing-data [impl implications-oioi,
                      fmt (list-implication-formats)]
    (try (out-in-out-in-test impl 'implication fmt)
         (catch UnsupportedOperationException _ true))))
