;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.decompositions
  (:require [conexp.base :refer :all]
            [conexp.math.algebra :refer :all]
            [conexp.gui.draw :refer :all]
            [conexp.io.contexts :refer :all]
            [conexp.fca 
             [contexts :refer :all]
             [metrics :refer :all]
             [lattices :refer :all]
             [posets :refer :all]]
            [clojure.set :as set]))


(defn interval [lat upper lower]
  (let [order (lattice-order lat)
        new-base-set (filter #(and (order % upper) (order lower %)) (base-set lat))]
    (make-lattice-nc new-base-set order))
)

(defn context-from-lattice [lat]
  "Computes the underlying context of a lattice by reading the incidence of all objects."
  "Does not work with the *lattice-product* method."
  (let [concepts (base-set lat)
        unions (reduce #(vector  (set/union (first %1) (first %2)) (set/union (second %1) (second %2))) concepts)
        objects (first unions)
        attributes (second unions)
        incidence (for [c concepts obj (first c) attr (second c)] [obj attr])]
    (make-context objects attributes incidence)
 ))

(defn libkin-decomposition-pairs [lat]
  "Returns all decompositions pair of *lat* for Libkin decompositions"
  (let [neutral-elements (neutral-concepts lat)]
      (for [n neutral-elements c (element-complement n lat)] [n c])))


(defn libkin-decomposition-lattices [lat decomp-pair]
  "Computes the Lattices Resulting from the Libkin-Decomposition on the 
  Provided Decomposition Pair."
  (let [set1 (order-ideal lat (conj #{} (first decomp-pair)))
        set2 (order-ideal lat (conj #{} (last decomp-pair)))
        order (lattice-order lat)]

      [(make-lattice-nc set1
                        order
                        (inf lat)
                        (sup lat))
       (make-lattice-nc set2
                        order
                        (inf lat)
                        (sup lat))]))

(defn combinatorial-decomposition-lattices [lat decomp-pair]
  "Computes the Lattices Resulting from the Libkin-Decomposition on the 
  Provided Decomposition Pair."
  (let [set1 (order-filter lat (conj #{} (first decomp-pair)))
        set2 (order-filter lat (conj #{} (last decomp-pair)))
        order (lattice-order lat)]

      [(make-lattice-nc set1
                        order
                        (inf lat)
                        (sup lat))
       (make-lattice-nc set2
                        order
                        (inf lat)
                        (sup lat))]))

(defn combinatorial-product [a b]
  "Computes the combinatoial produce of two lattices. 
   May fail if *a* and *b* are not complemented neutral ideals of the same lattice"
  (let [con1 (base-set a)
        con2 (base-set b)
        new-base-set (for [x con1 y con2]
                       [(set/intersection (first x) (first y))
                        (set/union (second x) (second y))])]
    (make-lattice new-base-set #(set/subset? (first %1) (first %2))))
)

(defn decomposable? [lat]
  (let [top (lattice-one lat)
        bot (lattice-zero lat)
        decomp-pairs (into #{} (libkin-decomposition-pairs lat))
        non-trivial (disj (disj  decomp-pairs [top bot]) [bot top])]

        (not (empty? non-trivial)))
)


(defn maximally-decomposable-filters [lat]

  (loop [queue [(lattice-zero lat)]
         visited #{}
         filters #{}]

      (if (empty? queue)
        filters

        (let [current-element  (first queue)
              current-filter (order-filter lat #{current-element})
              filter-lat (make-lattice-nc current-filter (inf lat) (sup lat))] 
          (if (decomposable? filter-lat)
            (recur (into [] (remove #(.contains current-filter %) queue))
                   (set/union visited current-filter)
                   (conj filters filter-lat))
            (recur (into [] (distinct (concat (subvec queue 1) ;remove first element
                                              (set/difference (lattice-upper-neighbours lat current-element) 
                                                              queue ;discard elements already in queue
                                                              visited)))) ;discard elements already visited
                   (conj visited current-element)
                   filters))))
))

(defn lattice-sort [a b lat]
  (let [order (lattice-order lat)]
    (if (= a b) 0
                (if (order a b) -1
                                 1)))
)


(defn intervals-with-upper [lat upper]

  (loop [queue [(lattice-zero lat)]
         visited #{}
         intervals #{}]


      (if (empty? queue)
        intervals

        (let [current-element  (first queue)
              current-interval (interval lat upper current-element)]

          (if (not ((lattice-order lat) current-element upper))
            (recur (into [] (remove #(.contains (order-filter lat #{current-element}) %) queue))
                   (set/union visited (order-filter lat #{current-element}))
                   intervals)

            (if (decomposable? current-interval)
              (recur (into [] (remove #(.contains (order-filter lat #{current-element}) %) queue))
                     (set/union visited (order-filter lat #{current-element}))
                     (conj intervals current-interval))
              (recur (into [] (distinct (concat (subvec queue 1) ;remove first element
                                                (set/difference (lattice-upper-neighbours lat current-element) 
                                                                queue ;discard elements already in queue
                                                                visited)))) ;discard elements already visited
                     (conj visited current-element)
                     intervals))))))
)


(defn maximally-decomposable-intervals [lat]
  (loop [queue (into [] (base-set lat))
         intervals #{}]

      (if (empty? queue)

        intervals

        (let [current-upper (first queue)
              new-intervals (maximally-decomposable-filters (make-lattice-nc (order-ideal lat #{current-upper}) (inf lat) (sup lat)))]

          (recur (into [] (rest queue))
                 (set/union intervals new-intervals)))))
)


(defn filter-maximal [intervals]
  (into [] (filter #(not (some (fn [x] (and (set/subset? (base-set %) (base-set x))
                                            (not= % x))) intervals)) intervals))

)




(defn hierarchy [lat]
  "Computes the hierarchy lattice of all combinatorial decompositions."
  (let [pairs (libkin-decomposition-pairs lat)
        sublats  (set (flatten (for [p pairs] (combinatorial-decomposition-lattices lat p))))]
    (make-lattice sublats #(set/subset? (base-set %1) (base-set %2))))
)

(defn prime-factorization [lat]
  (lattice-atoms (hierarchy lat))

)


(defn attr-union [ctx1 ctx2]
  "Unifies the attributes of *ctx1* and *ctx2*.
   *ctx1* and *ctx2* must have the same objects."
  (if (not (= (objects ctx1) (objects ctx2)))
    (println "Contexts do not have the same objects!")
    (make-context (objects ctx1) 
                  (set/union (attributes ctx1) (attributes ctx2)) 
                  (set/union (incidence ctx1) (incidence ctx2))))

)

(defn attr-intersection [ctx1 ctx2]
  "Intersects the attributes of *ctx1* and *ctx2*.
  *ctx1* and *ctx2* must have the same objects."
  (if (not (= (objects ctx1) (objects ctx2)))
    (println "Contexts do not have the same objects!")
    (make-context (objects ctx1) 
                  (set/intersection (attributes ctx1) (attributes ctx2)) 
                  (set/intersection (incidence ctx1) (incidence ctx2))))

)

(defn ctx-hierarchy [lat]
  "Computes the hierarchy lattice of the contexts of all combinatoiral decompositions."
  (let [pairs (libkin-decomposition-pairs lat)
        sublats  (set (flatten (for [p pairs] (combinatorial-decomposition-lattices lat p))))
        subctxs (map context-from-lattice sublats)]

    (make-lattice subctxs attr-intersection attr-union))
)

(defn test-hierarchy [hlat]
  "Verifies whether all meets and joins in the hierarchy lattice can be represented by attribute-union
   and attribute-intersection."
  (let [base (base-set hlat)
        join (sup hlat)
        meet (inf hlat)]
    (for [lat1 base lat2 base] (if (and (= (join lat1 lat2) 
                                           (concept-lattice (attr-union (context-from-lattice lat1)
                                                                        (context-from-lattice lat2))))
                                        (= (meet lat1 lat2) 
                                           (concept-lattice (attr-intersection (context-from-lattice lat1)
                                                                               (context-from-lattice lat2)))))
                                :OK
                                (do (println "FALSE##################################")
                                    (println (context-from-lattice hlat))
                                    (println (context-from-lattice lat1))
                                    (println (context-from-lattice lat2))
                                   )))
))

(defn test-combi-decomp [lat]
  "Verifies whether all combinatorial decompositions of *lat* are reversed by the combinatorial product."
  (let [pairs (libkin-decomposition-pairs lat)]
    (for [pair pairs] (if (= (combinatorial-product (first (combinatorial-decomposition-lattices lat pair))
                                                    (second (combinatorial-decomposition-lattices lat pair)))
                             lat)
                         :OK
                         (println "FALSE#####################################")))
))

(defn test-prime-factorization [hlat]

  (let [original (lattice-one hlat)
        primes (lattice-atoms hlat)]

    (if (= (reduce combinatorial-product primes) original) :OK
                                                           (println "FALSE###########################"))
))


(defn add-obj [ctx obj incidence] 
  "Returns a new context with *obj* added."
  (make-context (set/union (objects ctx) #{obj}) (attributes ctx) incidence))


(defn add-attr [ctx attr incidence] 
  "Returns a new context with *attr* added."
  (make-context (objects ctx) (set/union (attributes ctx) #{attr}) incidence))
  


(defn check-combinatorial-decomps [ctx-list]
  "Checks whether all combinatorial decompositions of the contexts in *ctx-list* are reversed by the combinatorial product."
  (for [ctx ctx-list]
    (let [lat (concept-lattice (reduce-context (read-context ctx)))
          decomposition-pairs (libkin-decomposition-pairs lat)]
      (for [pair decomposition-pairs]
        (let [decomp (combinatorial-decomposition-lattices lat pair)]
          (println (str ctx ":  " (= (combinatorial-product (first decomp) (second decomp)) lat))))

))))


(defn test-cycle [number]
  (loop [counter number]
    (let [ctx  (reduce-attributes (random-context #{1 2 3 4 5 6} #{"A" "B" "C" "D" "E" "F"} 0.5))
          lat (concept-lattice ctx)
          hlat (hierarchy lat)]
     
      (doall (test-hierarchy hlat))
      (doall (test-combi-decomp lat))
       (test-prime-factorization hlat)
      (if (not (= counter 0))
        (recur (- counter 1))))))



(def ctx-list #{"animals-d.ctx"
                "bird-diet-d.ctx"
                "bodiesofwater-d.ctx"
                "booth-d.ctx"
                "brunson-club-d.ctx"
                "cointoss-2-d.ctx"
                "diagnosis-d.ctx"
                "digits-d.ctx"
                "dirichlet-1-d.ctx"
                "dolphins-d.ctx"
                "drive_concepts_for_motorcars-d.ctx"
                "forum-romanum-d.ctx"
                "gewuerzplaner-d.ctx"
                "living-beings-and-water-d.ctx"
                "myctx-d.ctx"
                "olympic-disciplines-d.ctx"
                "seasoning-planner-d.ctx"
                "southern-woman-d.ctx"
                "testcontext-d.ctx"
                "triangles-d.ctx"
                "wood-properties-d.ctx"
                "zoo-d.ctx"})

(def ctx-list2 #{"testing-data/Animals.ctx"
                 "testing-data/Bird-Diet.ctx"
                 "testing-data/Brunson-Club.ctx"
                 "testing-data/Forum-Romanum.ctx"
                 "testing-data/Living-Beings-and-Water.ctx"
                 "testing-data/Southern-Woman.ctx"
                 "testing-data/Wood-Properties.ctx"
                 "testing-data/bodiesofwater.cxt"
                 "testing-data/digits.cxt"
                 "testing-data/drive_concepts_for_motorcars.cxt"
                 "testing-data/living_beings_and_water.cxt"
                 "testing-data/myctx.cxt"
                 "testing-data/testcontext.cxt"
                 "testing-data/triangles.cxt"
                 "testing-data/california.ctx"
})


(def ctx (make-context #{1 2 3 4} #{"A" "B" "C" "D"} #{[1 "A"] [1 "D"]
                                                       [2 "B"] [2 "D"]
                                                       [3 "C"] [3 "D"]
                                                       [4 "A"] [4 "B"] [4 "C"]}))

(def diamond (make-context  #{1 2 3 4} #{"A" "B" "C"} #{[1 "A"]
                                                        [2 "B"]
                                                        [3 "C"]
                                                        [4 "A"] [4 "B"] [4 "C"]}))

(def chain (make-context #{1 2 3 4} #{"E"} #{[1 "E"] [2 "E"] [3 "E"]}))

(def bigctx (make-context #{1 2 3 4 5} #{"A" "B" "C" "D" "E"} #{[1 "A"] [1 "D"] [1 "E"]
                                                                [2 "B"] [2 "D"] [2 "E"]
                                                                [3 "C"] [3 "D"] [3 "E"]
                                                                [4 "A"] [4 "B"] [4 "C"] [4 "D"]
                                                                [5 "A"] [5 "B"] [5 "C"] [5 "E"]}))

(def testctx (make-context #{1 2 3} #{"A" "B"} #{[1 "A"] [2 "B"]}))

(def testctx2 (make-context #{1 2 3} #{"C"} #{}))

(def bodiesofwater (make-context #{"puddle" "channel" "river" "canal" "lake" "reservoir" "sea"}
                                 #{"temporary" "running" "inland" "natural" "stagant" "constant" "artificial" "maritime"}

                                 #{["puddle" "temporary"] ["puddle" "inland"] ["puddle" "natural"] ["puddle" "stagant"]
                                   ["channel" "running"] ["channel" "inland"] ["channel" "constant"]
                                   ["river" "running"] ["river" "inland"] ["river" "natural"] ["river" "constant"]
                                   ["canal" "running"] ["canal" "inland"] ["canal" "constant"] ["canal" "artificial"]
                                   ["lake" "inland"] ["lake" "natural"] ["lake" "stagant"] ["lake" "constant"]
                                   ["reservoir" "inland"] ["reservoir" "stagant"] ["reservoir"  "constant"] ["reservoir" "artificial"]
                                   ["sea" "natural"] ["sea" "stagant"] ["sea" "constant"] ["sea" "maritime"]}))


(def ctx (read-context "testing-data/Bird-Diet.ctx"))
(def bc (birkhoff-upset-completion ctx))
(def lat (concept-lattice ctx))
(def bclat (concept-lattice bc))
bclat
(decomposable? bclat)



(def ctx (read-context "testing-data/Animals.cxt"))


(def ctx (context-from-fcatools "animals_en.cxt"))
(def lat (concept-lattice ctx))
(def max-int (filter-maximal (maximally-decomposable-intervals lat)))

(sort #(< (count (base-set %1)) (count (base-set %2))) max-int)
