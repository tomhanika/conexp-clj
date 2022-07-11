(ns conexp.fca.evaluation
    (:require [conexp.base :refer :all]))

;; This file contains function used to evaluate different factorization algorithms
(defn hamming-distance
    "Computes Hamming distance between context A and B"
    [A B]
    (count (filter #{1} (map (fn [a b] (if (not= a b) 1 0)) A B))))
  
  (defn- helper
    "Helper Function for Biclustermatch"
    [m1 M2]
      (for [m2 M2]
        (double (/ (count (clojure.set/intersection (set (:g m1)) (set (:g m2)))) (count (clojure.set/union (set (:g m1)) (set (:g m2))))))))
  
  (defn import-file [file] 
    "Helper function to import Bimax biclustering files"
    (let [raw (rest
               (clojure.string/split (slurp file)
                                     #"\n"))]
      (map
       (fn [line]
         (let [[g c] (clojure.string/split line #"\;")]
           {:g (map read-string (clojure.string/split g #","))
            :c (map read-string (clojure.string/split c #","))}))
       raw)))
  
  (defn bi-cluster-match
    "Computes Biclustering Score for M1 and M2, both being Bicluster Sets of Contexts."
    [M1 M2]
    (loop [result (list) i 0]
      (if (>= i (count M1))
        (* (/ 1 (count M1)) (reduce + result))
        (recur 
          (conj result (apply max (helper (nth M1 i) M2)))
          (inc i)))))