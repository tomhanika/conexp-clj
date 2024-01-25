(ns conexp.fca.factorization-test
    (:require [conexp.fca.contexts :refer :all] 
              [conexp.fca.factorization :refer :all]
              )
    (:use clojure.test))

(def water-context   (make-context-from-matrix 8 9 [1 1 0 0 0 0 1 0 0 1 1 0 0 0 0 1 1 0 1 1 1 0 0 0 1 1 0 1 0 1 0 0 0 1 1 1 1 1 0 1 0 1 0 0 0 1 1 1 1 0 1 0 0 0 1 0 1 1 1 0 0 0 0 1 0 1 1 0 1 0 0 0]))
(def water-panda     (make-context-from-matrix 8 9 [1 1 0 0 0 0 1 1 0 1 1 0 0 0 0 1 1 0 1 1 1 0 0 0 1 1 0 1 1 1 0 0 0 1 1 1 1 1 1 1 0 1 0 0 0 1 1 1 1 0 1 0 0 0 1 0 1 1 1 0 0 0 0 1 1 1 1 0 1 0 0 0]))
(def water-grecond   (make-context-from-matrix 8 9 [1 1 0 0 0 0 1 0 0 1 1 0 0 0 0 1 0 0 1 1 1 0 0 0 1 0 0 1 0 1 0 0 0 0 0 0 1 1 0 1 0 1 0 0 0 1 1 1 1 0 1 0 0 0 1 0 1 0 0 0 0 0 0 1 0 1 1 0 1 0 0 0]))
(def water-hyper     (make-context-from-matrix 8 9 [1 1 0 0 0 0 0 0 0 1 1 0 0 0 0 1 1 0 1 1 1 0 0 0 1 1 0 1 0 1 0 0 0 1 1 0 1 1 0 1 0 1 0 0 0 1 1 1 1 0 1 0 0 0 1 0 1 1 0 0 0 0 0 1 0 1 1 0 1 0 0 0]))
(def water-greess    (make-context-from-matrix 8 9 [1 1 0 0 0 0 0 0 0 1 1 0 0 0 0 1 1 0 1 1 1 0 0 0 1 1 0 1 0 1 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 1 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 1 0 0 0]))
(def water-tiling    (make-context-from-matrix 8 9 [1 1 0 0 0 0 1 0 0 1 1 0 0 0 0 1 0 0 1 1 1 0 0 0 1 0 0 1 0 1 0 0 0 1 1 1 1 0 0 1 0 1 0 0 0 1 0 1 1 0 1 0 0 0 1 0 1 0 0 0 0 0 0 1 0 1 1 0 1 0 0 0]))
(def water-topfiberm (make-context-from-matrix 8 9 [1 1 0 0 0 0 1 0 0 1 1 0 0 0 0 1 0 0 1 1 1 0 0 0 1 0 0 1 0 1 0 0 0 1 0 0 1 1 0 1 0 0 0 0 0 1 1 1 1 0 0 0 0 0 1 0 1 1 0 0 0 0 0 1 0 1 1 0 0 0 0 0]))

(def water-panda-test (apply ->factorization-record (panda water-context 5)))
(def water-grecond-test (apply ->factorization-record (grecond water-context 5)))
(def water-hyper-test (apply ->factorization-record (hyper water-context 5)))
(def water-greess-test (apply ->factorization-record (greess water-context 5)))
(def water-tiling-test (apply ->factorization-record (tiling water-context 5)))
(def water-topfiberm-test (apply ->factorization-record (topfiberm water-context 5 1 1)))

(deftest test-panda
    (is (= water-panda (context water-panda-test))))

(deftest test-grecond
    (is (= water-grecond (context water-grecond-test))))

(deftest test-hyper
    (is (= water-hyper (context water-hyper-test))))

(deftest test-greess
    (is (= water-greess (context water-greess-test))))

(deftest test-tiling
    (is (= water-tiling (context water-tiling-test))))
    
(deftest test-topfiberm
    (is (= water-test-topfiberm (context water-test-topfiberm-test))))