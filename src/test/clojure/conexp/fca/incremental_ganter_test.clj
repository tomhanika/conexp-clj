;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.incremental-ganter-test
  (:use clojure.test)
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications
        conexp.fca.incremental-ganter)
  (:require [conexp.fca.contexts-test :as contexts]))

;;;

(deftest test-incremental-ganter
  (is (= (set (stem-base contexts/test-ctx-01)) 
         (set (incremental-ganter contexts/test-ctx-01))))
  (is (= (set (stem-base contexts/test-ctx-04)) 
         (set (incremental-ganter contexts/test-ctx-04))))
  (is (= (set (stem-base contexts/test-ctx-07)) 
         (set (incremental-ganter contexts/test-ctx-07))))
  (is (= (set (stem-base contexts/test-ctx-08))  
         (set (incremental-ganter contexts/test-ctx-08)))))
