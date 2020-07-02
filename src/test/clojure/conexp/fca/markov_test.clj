;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.markov-test
  (:use clojure.test)
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.more
        conexp.fca.markov)
  (:require [conexp.fca.contexts-test :as contexts]))

;;; base10

(deftest test-base10
  (let [bases (list 2 4 7 9)
        numbers (list 0 1 10 999 0.001 3.14159)]
    (doall
      (for [base bases number numbers]
        (is (= (float number) 
               (float (to-base10 (from-base10 number base 100) base))))))))

;;; stability

(deftest test-mcs-stability-approximation
  (let [ctx          contexts/test-ctx-01
        concept-list (concepts ctx)
        positions    (list 0 1 2 3 4)]
    (doall 
      (for [position positions]
        (is (= (float (concept-stability ctx (nth concept-list position)))
               (float (mcs-stability-approximation ctx 
                                            (nth concept-list position)
                                            10000))))))))
