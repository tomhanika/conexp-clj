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
             [decompositions :refer :all]])
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

;no libkin decompositions
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

;only decomposable by top and bot
(def ctx3 (make-context #{1 2 3 4 5 6}
                        #{1 2 3 4 5 6}
                        #{[1 1] [1 4] [1 5]
                          [2 1] [2 5] [2 6]
                          [3 2] [3 6]
                          [4 1] [4 5]
                          [5 2] [5 4]
                          [6 2] [6 3] [6 4] [6 5]}))
(def lat3 (concept-lattice ctx3))
(def decomp-pairs3 (seq [[[#{} #{1 4 6 3 2 5}] [#{1 4 6 3 2 5} #{}]]
                         [[#{1 4 6 3 2 5} #{}] [#{} #{1 4 6 3 2 5}]]]))


(deftest test-libkin

  (is (= (libkin-decomposition-pairs lat1) 
         decomp-pairs1))
  (is (= (libkin-decomposition-lattices lat1 [[#{7 6} #{"C" "A"}] [#{7 4 2 5} #{"B"}]]) 
         [ctx1-decomp-1 ctx1-decomp-2]))

  (is (= (libkin-decomposition-pairs lat2)
         decomp-pairs-2))

  (is (= (libkin-decomposition-pairs lat3) 
         decomp-pairs3))
)
