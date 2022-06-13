(ns conexp.fca.factorization-test
    (:require [conexp.fca.contexts :refer :all] 
              [conexp.fca.factorization :refer :all]
              )
    (:use clojure.test)
)

(def waterContext (make-context-from-matrix 8 9 [1 1 0 0 0 0 1 0 0 1 1 0 0 0 0 1 1 0 1 1 1 0 0 0 1 1 0 1 0 1 0 0 0 1 1 1 1 1 0 1 0 1 0 0 0 1 1 1 1 0 1 0 0 0 1 0 1 1 1 0 0 0 0 1 0 1 1 0 1 0 0 0]))
(def waterPanda   (make-context-from-matrix 8 9 [1 1 0 0 0 0 1 1 0 1 1 0 0 0 0 1 1 0 1 1 1 0 0 0 1 1 0 1 1 1 0 0 0 1 1 1 1 1 1 1 0 1 0 0 0 1 1 1 1 0 1 0 0 0 1 0 1 1 1 0 0 0 0 1 1 1 1 0 1 0 0 0]))
(def waterGrecond (make-context-from-matrix 8 9 [1 1 0 0 0 0 1 0 0 1 1 0 0 0 0 1 0 0 1 1 1 0 0 0 1 0 0 1 0 1 0 0 0 0 0 0 1 1 0 1 0 1 0 0 0 1 1 1 1 0 1 0 0 0 1 0 1 0 0 0 0 0 0 1 0 1 1 0 1 0 0 0]))
(def waterHyper   (make-context-from-matrix 8 9 [1 1 0 0 0 0 0 0 0 1 1 0 0 0 0 1 1 0 1 1 1 0 0 0 1 1 0 1 0 1 0 0 0 1 1 0 1 1 0 1 0 1 0 0 0 1 1 1 1 0 1 0 0 0 1 0 1 1 0 0 0 0 0 1 0 1 1 0 1 0 0 0]))
(def waterGrees   (make-context-from-matrix 8 9 [1 1 0 0 0 0 0 0 0 1 1 0 0 0 0 1 1 0 1 1 1 0 0 0 1 1 0 1 0 1 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 1 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 1 0 0 0]))

(deftest test-panda
    (is (= waterPanda (get (panda waterContext 5) 2))))

(deftest test-grecond
    (is (= waterGrecond (calcGrecondContext (grecond waterContext 5) 8 9))))

(deftest test-hyper
    (is (= waterHyper (calcHyperContext (hyper waterContext 5)))))

(deftest test-greess
    (is (= waterGrees (calcGreessContext (greess waterContext 5) 9 8))))
