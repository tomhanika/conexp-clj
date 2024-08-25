(ns conexp.fca.matrix-factorizations-test
    (:require [clojure.string :as str]
              [clojure.set :as set]
              [conexp.base :refer :all]
              [conexp.fca.contexts :refer :all]
              [conexp.fca.lattices :refer :all]
              [conexp.fca.matrix-factorizations :refer :all]
              [conexp.fca.implications :refer [support frequent-itemsets]]
              [conexp.io.contexts :refer [read-context]])
    (:use clojure.test))

(def forum (read-context "testing-data/Forum-Romanum.ctx"))
(def water (read-context "testing-data/Living-Beings-and-Water.ctx"))
(def wood (read-context "testing-data/Wood-Properties.ctx"))

(defn overcoverage [ctx approx]
  (count (set/difference (incidence-relation approx) (incidence-relation ctx)))
)

(defn undercoverage [ctx approx]
  (count (set/difference (incidence-relation ctx) (incidence-relation approx)))
)

(deftest test-hyper
  
  (def fac1 (hyper forum 0.7))
  (def fac2 (hyper water 0.5))
  (def fac3 (hyper wood 0.3))

  (is (= (overcoverage forum (context fac1)) 0))
  (is (= (undercoverage forum (context fac1)) 0))

  (is (= (overcoverage water (context fac2)) 0))
  (is (= (undercoverage water (context fac2)) 0))

  (is (= (overcoverage wood (context fac3)) 0))
  (is (= (undercoverage wood (context fac3)) 0))
  
)


(deftest test-topfiberm
  
  (def fac1 (topFiberM forum 3 0.7 5))
  (def fac2 (topFiberM water 5 0.7 5))
  (def fac3 (topFiberM wood 3 0.5 5))

  (is (= (overcoverage forum (context fac1)) 0))
  (is (= (undercoverage forum (context fac1)) 21))

  (is (= (overcoverage water (context fac2)) 0))
  (is (= (undercoverage water (context fac2)) 10))

  (is (= (overcoverage wood (context fac3)) 5))
  (is (= (undercoverage wood (context fac3)) 192))
  
)


(deftest test-panda
  
  (def fac1 (PaNDa forum 3))
  (def fac2 (PaNDa water 5))
  (def fac3 (PaNDa wood 8))

  (is (= (overcoverage forum (context fac1)) 4))
  (is (= (undercoverage forum (context fac1)) 11))

  (is (= (overcoverage water (context fac2)) 4))
  (is (= (undercoverage water (context fac2)) 2))

  (is (= (overcoverage wood (context fac3)) 31))
  (is (= (undercoverage wood (context fac3)) 79))
  
)


(deftest test-tiling
  
  (def fac1 (tiling forum 5))
  (def fac2 (tiling water 8))
  (def fac3 (tiling wood 15))

  (is (= (overcoverage forum (context fac1)) 0))
  (is (= (undercoverage forum (context fac1)) 6))

  (is (= (overcoverage water (context fac2)) 0))
  (is (= (undercoverage water (context fac2)) 0))

  (is (= (overcoverage wood (context fac3)) 0))
  (is (= (undercoverage wood (context fac3)) 51))
  
)


(deftest test-grecond
  
  (def fac1 (grecond forum))
  (def fac2 (grecond water))
  (def fac3 (grecond wood))

  (is (= (overcoverage forum (context fac1)) 0))
  (is (= (undercoverage forum (context fac1)) 0))

  (is (= (overcoverage water (context fac2)) 0))
  (is (= (undercoverage water (context fac2)) 0))

  (is (= (overcoverage wood (context fac3)) 0))
  (is (= (undercoverage wood (context fac3)) 0))
  
)


(deftest test-grees
  
  (def fac1 (GreEss forum 5))
  (def fac2 (GreEss water 0))
  (def fac3 (GreEss wood 20))

  (is (= (overcoverage forum (context fac1)) 0))
  (is (<= (undercoverage forum (context fac1)) 5))

  (is (= (overcoverage water (context fac2)) 0))
  (is (<= (undercoverage water (context fac2)) 0))

  (is (= (overcoverage wood (context fac3)) 0))
  (is (<= (undercoverage wood (context fac3)) 20))
  
)


(deftest test-asso
  
  (def fac1 (ASSO forum 8 0.7 1 1))
  (def fac2 (ASSO water 5 0.7 1 1))
  (def fac3 (ASSO wood 3 0.7 1 1))

  (is (= (overcoverage forum (context fac1)) 4))
  (is (<= (undercoverage forum (context fac1)) 8))

  (is (= (overcoverage water (context fac2)) 4))
  (is (<= (undercoverage water (context fac2)) 1))

  (is (= (overcoverage wood (context fac3)) 24))
  (is (<= (undercoverage wood (context fac3)) 128))
  
)
