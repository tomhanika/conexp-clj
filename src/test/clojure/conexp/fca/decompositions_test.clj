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


(deftest test-libkin
  (let [ctx (make-context #{1 2 3 4 5 6 7} 
                          #{"A" "B" "C"} 
                          #{[1 "A"] [2 "B"] [3 "C"]
                            [4 "A"] [4 "B"] [5 "B"] [5 "C"] [6 "A"] [6 "C"]
                            [7 "A"] [7 "B"] [7 "C"]})
        cube-lat (concept-lattice ctx)
        cube-lat-decomp-pairs (seq [[[#{7 6} #{"C" "A"}] [#{7 4 2 5} #{"B"}]]
                                    [[#{7 4 2 5} #{"B"}] [#{7 6} #{"C" "A"}]]
                                    [[#{7 1 4 6} #{"A"}] [#{7 5} #{"C" "B"}]]
                                    [[#{7 5} #{"C" "B"}] [#{7 1 4 6} #{"A"}]]
                                    [[#{7 6 3 5} #{"C"}] [#{7 4} #{"B" "A"}]]
                                    [[#{7 1 4 6 3 2 5} #{}] [#{7} #{"C" "B" "A"}]]
                                    [[#{7} #{"C" "B" "A"}] [#{7 1 4 6 3 2 5} #{}]]
                                    [[#{7 4} #{"B" "A"}] [#{7 6 3 5} #{"C"}]]])
        decomp-ctx1 (make-context #{6 7}
                                  #{"A" "B" "C"}
                                  #{[6 "A"] [6 "C"] [7 "A"] [7 "B"] [7 "C"]})
        decomp-lat1 (concept-lattice decomp-ctx1)
        decomp-ctx2 (make-context #{2 4 5 7}
                                  #{"A" "B" "C"}
                                  #{[2 "B"] [4 "A"] [4 "B"] [5 "B"] [5 "C"] [7 "A"] [7 "B"] [7 "C"]})
        decomp-lat2 (concept-lattice decomp-ctx2)]

      (is (= (libkin-decomposition-pairs cube-lat) cube-lat-decomp-pairs))
      (is (= (libkin-decomposition-lattices cube-lat [[#{7 6} #{"C" "A"}] [#{7 4 2 5} #{"B"}]]) [decomp-lat1 decomp-lat2]))))
