(ns conexp.fca.decompositions
  (:require [conexp.base :refer :all]
            [conexp.fca 
             [contexts :refer :all]
             [metrics :refer :all]
             [lattices :refer :all]
             [posets :refer :all]]
            ))


;returns all decompositions pair of "lat" for Libkin decompositions
(defn libkin-decomposition-pairs [lat]
  (let [neutral-elements (neutral-concepts lat)]
      (for [n neutral-elements c (element-complement n lat)] [n c])
))


(defn lattice-ideal [lat concept]
  (let [base-set (lattice-base-set lat)
        order (lattice-order lat)
        new-base-set (filter #(order % concept) base-set)]

        (make-lattice-nc new-base-set
                         order
                         (inf lat)
                         (sup lat))
    
)
)


(defn libkin-decomposition-lattices [lat decomp-pair]
  [(lattice-ideal lat (first decomp-pair))
   (lattice-ideal lat (last decomp-pair))]
)




;test
(def ctx (make-context #{1 2 3 4 5} 
                       #{"A" "B" "C" "D" "E"} 
                       #{[1 "A"] [2 "A"] [2 "B"] [3 "B"] [3 "C"] [4 "A"] [4 "B"] [4 "C"] [5 "C"] [5 "D"] [5 "E"]}))
(def lat (concept-lattice ctx))


(def ctx2 (make-context #{1 2 3 4 5} 
                       #{"A" "B" "C" "D" "E"} 
                       #{[1 "A"] [2 "A"]   [4 "A"] [4 "B"] [4 "C"] [5 "C"] [5 "D"] [5 "E"]}))
(def lat2 (concept-lattice ctx))


(def ctx3 (make-context #{1 2 3 4 5 6 7} #{"A" "B" "C"} #{[1 "A"] [2 "B"] [3 "C"]
                                                          [4 "A"] [4 "B"] [5 "B"] [5 "C"] [6 "A"] [6 "C"]
                                                          [7 "A"] [7 "B"] [7 "C"]}))
(def lat3 (concept-lattice ctx3))
