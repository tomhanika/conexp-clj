(ns conexp.fca.factorization-test
    (:require [conexp.fca.contexts :refer :all] 
              [conexp.fca.factorization :refer :all]
              )
    (:use clojure.test)
)

(def waterContext  (make-context-from-matrix 8 9 [1 1 0 0 0 0 1 0 0 1 1 0 0 0 0 1 1 0 1 1 1 0 0 0 1 1 0 1 0 1 0 0 0 1 1 1 1 1 0 1 0 1 0 0 0 1 1 1 1 0 1 0 0 0 1 0 1 1 1 0 0 0 0 1 0 1 1 0 1 0 0 0]))
(def waterPanda    (make-context-from-matrix 8 9 [1 1 0 0 0 0 1 1 0 1 1 0 0 0 0 1 1 0 1 1 1 0 0 0 1 1 0 1 1 1 0 0 0 1 1 1 1 1 1 1 0 1 0 0 0 1 1 1 1 0 1 0 0 0 1 0 1 1 1 0 0 0 0 1 1 1 1 0 1 0 0 0]))
(def waterGrecond  (make-context-from-matrix 8 9 [1 1 0 0 0 0 1 0 0 1 1 0 0 0 0 1 0 0 1 1 1 0 0 0 1 0 0 1 0 1 0 0 0 0 0 0 1 1 0 1 0 1 0 0 0 1 1 1 1 0 1 0 0 0 1 0 1 0 0 0 0 0 0 1 0 1 1 0 1 0 0 0]))
(def waterHyper    (make-context-from-matrix 8 9 [1 1 0 0 0 0 0 0 0 1 1 0 0 0 0 1 1 0 1 1 1 0 0 0 1 1 0 1 0 1 0 0 0 1 1 0 1 1 0 1 0 1 0 0 0 1 1 1 1 0 1 0 0 0 1 0 1 1 0 0 0 0 0 1 0 1 1 0 1 0 0 0]))
(def waterGreess   (make-context-from-matrix 8 9 [1 1 0 0 0 0 0 0 0 1 1 0 0 0 0 1 1 0 1 1 1 0 0 0 1 1 0 1 0 1 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 1 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 1 0 0 0]))

(def waterPandaTest (apply ->factorization-record (panda waterContext 5)))
(def waterGrecondTest (apply ->factorization-record (grecond waterContext 5)))
(def waterHyperTest (apply ->factorization-record (hyper waterContext 5)))
(def waterGreessTest (apply ->factorization-record (greess waterContext 5)))

(deftest test-panda
    (is (= waterPanda (context waterPandaTest))))

(deftest test-grecond
    (is (= waterGrecond (context waterGrecondTest))))

(deftest test-hyper
    (is (= waterHyper (context waterHyperTest))))

(deftest test-greess
    (is (= waterGreess (context waterGreessTest))))
