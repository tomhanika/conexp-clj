;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.tests.io.many-valued-contexts
  (:use conexp.base
        conexp.fca.many-valued-contexts
        conexp.io.many-valued-contexts
        conexp.tests.io.util)
  (:use clojure.test))

;;;

(defvar- mv-contexts-oi
  [(make-mv-context ["1" "2" "3"] ["4" "5" "6"] str),
   (make-mv-context ["a" "b" "c"] [] str)
   (make-mv-context [] ["1" "2" "3"] str)]
  "Context to use for out-in testing")

(deftest test-mv-context-out-in
  (with-testing-data [mv-ctx mv-contexts-oi,
                      fmt (list-mv-context-formats)]
    (try (= mv-ctx (out-in mv-ctx 'mv-context fmt))
         (catch UnsupportedOperationException _ true))))

(defvar- mv-contexts-oioi
  [(make-mv-context (range 10) (range 10) +),
   (make-mv-context (range 10) (range 10) (fn [_ _] (rand)))
   (make-mv-context [] [] +)]
  "Contexts to use for out-in-out-in testing")

(deftest test-mv-context-out-in-out-in
  (with-testing-data [mv-ctx mv-contexts-oioi,
                      fmt (list-mv-context-formats)]
    (try (out-in-out-in-test mv-ctx 'mv-context fmt)
         (catch UnsupportedOperationException _ true))))

;;;

nil
