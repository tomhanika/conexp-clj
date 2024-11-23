;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.decompositions-test
  (:require [conexp.base :refer :all]
            [conexp.math.algebra :refer :all]
            [conexp.fca 
             [contexts :refer :all]
             [metrics :refer :all]
             [lattices :refer :all]
             [posets :refer :all]
             [decompositions :refer :all]]
            [clojure.set :as set])
  (:use clojure.test))




;cube context
(def ctx1 (make-context #{1 2 3 4 5 6 7} 
                          #{"A" "B" "C"} 
                          #{[1 "A"] [2 "B"] [3 "C"]
                            [4 "A"] [4 "B"] [5 "B"] [5 "C"] [6 "A"] [6 "C"]
                            [7 "A"] [7 "B"] [7 "C"]}))
(def lat1 (concept-lattice ctx1))

(def decomp-pairs1 (seq [[[#{7 6} #{"C" "A"}] [#{7 4 2 5} #{"B"}]]
                                    [[#{7 4 2 5} #{"B"}] [#{7 6} #{"C" "A"}]]
                                    [[#{7 1 4 6} #{"A"}] [#{7 5} #{"C" "B"}]]
                                    [[#{7 5} #{"C" "B"}] [#{7 1 4 6} #{"A"}]]
                                    [[#{7 6 3 5} #{"C"}] [#{7 4} #{"B" "A"}]]
                                    [[#{7 1 4 6 3 2 5} #{}] [#{7} #{"C" "B" "A"}]]
                                    [[#{7} #{"C" "B" "A"}] [#{7 1 4 6 3 2 5} #{}]]
                                    [[#{7 4} #{"B" "A"}] [#{7 6 3 5} #{"C"}]]]))
(def ctx1-decomp-1 (concept-lattice (make-context #{6 7}
                                                  #{"A" "B" "C"}
                                                  #{[6 "A"] [6 "C"] [7 "A"] [7 "B"] [7 "C"]})))
(def ctx1-decomp-2 (concept-lattice (make-context #{2 4 5 7}
                                                  #{"A" "B" "C"}
                                                  #{[2 "B"] [4 "A"] [4 "B"] [5 "B"] [5 "C"] [7 "A"] [7 "B"] [7 "C"]})))

;no decompositions
(def ctx2 (make-context #{1 2 3 4 5}
                        #{1 2 3 4 5}
                        #{[1 2] [1 3] [1 5]
                          [2 3]
                          [3 1] [3 2] [3 3] [3 4]
                          [4 3] [4 4]
                          [5 3] [5 5]}))
(def lat2 (concept-lattice ctx2))

(def decomp-pairs-2 (seq [[[#{1 4 3 2 5} #{3}] [#{} #{1 4 3 2 5}]]
                          [[#{} #{1 4 3 2 5}] [#{1 4 3 2 5} #{3}]]]))


(def bigctx (make-context #{1 2 3 4 5} #{"A" "B" "C" "D" "E"} #{[1 "A"] [1 "D"] [1 "E"]
                                                                [2 "B"] [2 "D"] [2 "E"]
                                                                [3 "C"] [3 "D"] [3 "E"]
                                                                [4 "A"] [4 "B"] [4 "C"] [4 "D"]
                                                                [5 "A"] [5 "B"] [5 "C"] [5 "E"]}))

(def biglat (concept-lattice bigctx))

(deftest test-decomp

  (is (= (decomposition-pairs lat1) 
         decomp-pairs1))
  (is (= (downset-decomposition-lattices lat1 [[#{7 6} #{"C" "A"}] [#{7 4 2 5} #{"B"}]]) 
         [ctx1-decomp-1 ctx1-decomp-2]))

  (is (= (decomposition-pairs lat2)
         decomp-pairs-2))

)


(deftest test-decomp-lattice
  ;Verifies whether all meets and joins in the decomposition lattice can be represented by 
  ;attribute-union and attribute-intersection.
  (let [dlat (direct-decomposition-lattice biglat)
        base-set (base-set dlat)
        join (sup dlat)
        meet (inf dlat)]
    (doseq [lat1 base-set lat2 base-set] (is (and (= (join lat1 lat2) 
                                                     (concept-lattice (attr-union (context-from-lattice lat1)
                                                                                  (context-from-lattice lat2))))
                                                  (= (meet lat1 lat2) 
                                                     (concept-lattice (attr-intersection (context-from-lattice lat1)
                                                                                         (context-from-lattice lat2))))))))
  (let [clat (ctx-decomposition-lattice biglat)
        base-set (base-set clat)
        join (sup clat)
        meet (inf clat)]
    (doseq [ctx1 base-set xtx2 base-set] (is (and (= (join ctx1 ctx2) 
                                                     (attr-union ctx1 ctx2))
                                                  (= (meet ctx1 ctx2) 
                                                     (attr-intersection ctx1 ctx2))))))
)

(deftest test-prime.factors

  (is (= (prime-factorization biglat) ))

)
