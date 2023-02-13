;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.many-valued-contexts-test
  (:use conexp.base
        conexp.fca.many-valued-contexts
        conexp.io.many-valued-contexts
        conexp.io.util-test)
  (:use clojure.test))

;;;

(def- mv-contexts-oi
  "Context to use for out-in testing"
  [(make-mv-context ["1" "2" "3"] ["4" "5" "6"] str),
   (make-mv-context ["a" "b" "c"] [] str)
   (make-mv-context [] ["1" "2" "3"] str)])

(deftest test-mv-context-out-in
  (with-testing-data [mv-ctx mv-contexts-oi,
                      fmt (remove #{:data-table} (list-mv-context-formats))]
    (= mv-ctx (out-in mv-ctx 'many-valued-context fmt)))
  ;; Cannot store many-valued contexts with less than 2 attributes in format :data-table.
  ;; Cannot store many-valued context without objects in format :data-table.
  (with-testing-data [mv-ctx (mapv mv-contexts-oi [0]),
                      fmt #{:data-table}]
    (= mv-ctx (out-in mv-ctx 'many-valued-context fmt))))

(def- mv-contexts-oioi
  "Contexts to use for out-in-out-in testing"
  [(make-mv-context (range 10) (range 10) +),
   (make-mv-context (range 10) (range 10) (fn [_ _] (rand)))
   (make-mv-context [] [] +)
   (make-mv-context [1 2 3]
                    '[color size]
                    '#{[1 color blue] [1 size large]
                       [2 color green] [2 size very-large]
                       [3 color red] [3 size small]})])

(deftest test-mv-context-out-in-out-in
  (with-testing-data [mv-ctx mv-contexts-oioi,
                      fmt (remove #{:data-table} (list-mv-context-formats))]
    (out-in-out-in-test mv-ctx 'many-valued-context fmt))
  ;; Cannot store many-valued contexts with less than 2 attributes in format :data-table.
  (with-testing-data [mv-ctx (mapv mv-contexts-oioi [0 1]),
                      fmt #{:data-table}]
    (out-in-out-in-test mv-ctx 'many-valued-context fmt)))

;;;

nil
