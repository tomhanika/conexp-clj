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


(defn context-from-lattice [lat]
  (let [concepts (base-set lat)
        unions (reduce #(vector (set/union (first %1) (first %2)) (set/union (second %1) (second %2))) concepts)
        objects (first unions)
        attributes (second unions)
        incidence (for [c concepts obj (first c) attr (second c)] [obj attr])]
    (make-context objects attributes incidence)
 )

)




(defn libkin-decomposition-pairs 
  "Returns all decompositions pair of *lat* for Libkin decompositions"
  [lat]
  (let [neutral-elements (neutral-concepts lat)]
      (for [n neutral-elements c (element-complement n lat)] [n c])))


(defn libkin-decomposition-lattices 
  "Computes the Lattices Resulting from the Libkin-Decomposition on the 
  Provided Decomposition Pair."
  [lat decomp-pair]
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

(defn combinatorial-decomposition-lattices 
  "Computes the Lattices Resulting from the Libkin-Decomposition on the 
  Provided Decomposition Pair."
  [lat decomp-pair]
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

  (let [con1 (base-set a)
        con2 (base-set b)
        new-base-set (for [x con1 y con2]
                       [(set/intersection (first x) (first y))
                        (set/union (second x) (second y))])]
    (make-lattice new-base-set #(subset? (first %1) (first %2))))
)


(defn hierarchy [lat]
  (let [pairs (libkin-decomposition-pairs lat)
        sublats  (set (flatten (for [p pairs] (combinatorial-decomposition-lattices lat p))))]
    (make-lattice sublats #(subset? (base-set %1) (base-set %2))))
)

(defn attr-union [ctx1 ctx2]
  "ctx1 and ctx2 must have the same set of objects"
  (if (not (= (objects ctx1) (objects ctx2)))
    (println "Contexts do not have the same objects!")
    (make-context (objects ctx1) 
                  (set/union (attributes ctx1) (attributes ctx2)) 
                  (set/union (incidence ctx1) (incidence ctx2))))

)

(defn attr-intersection [ctx1 ctx2]
  "ctx1 and ctx2 must have the same set of objects"
  (if (not (= (objects ctx1) (objects ctx2)))
    (println "Contexts do not have the same objects!")
    (make-context (objects ctx1) 
                  (set/intersection (attributes ctx1) (attributes ctx2)) 
                  (set/intersection (incidence ctx1) (incidence ctx2))))

)

(defn ctx-hierarchy [lat]
  (let [pairs (libkin-decomposition-pairs lat)
        sublats  (set (flatten (for [p pairs] (combinatorial-decomposition-lattices lat p))))
        subctxs (map context-from-lattice sublats)]

    (make-lattice subctxs attr-intersection attr-union))
)

(defn test-hierarchy [lat]
  (let [hlat (hierarchy lat)
        base (base-set hlat)
        join (sup hlat)
        meet (inf hlat)]
    (for [lat1 base lat2 base] (if (and (= (join lat1 lat2) 
                                           (concept-lattice (attr-union (context-from-lattice lat1)
                                                                       (context-from-lattice lat2))))
                                        (= (meet lat1 lat2) 
                                           (concept-lattice (attr-intersection (context-from-lattice lat1)
                                                                              (context-from-lattice lat2)))))
                                (println "TRUE")
                                (do (println "FALSE")
                                    (println (context-from-lattice lat))
                                    (println (context-from-lattice lat1))
                                    (println (context-from-lattice lat2))
                                   )))
))

(defn test-combi-decomp [lat]
  (let [pairs (libkin-decomposition-pairs lat)]
    (for [pair pairs] (if (= (combinatorial-product (first (combinatorial-decomposition-lattices lat pair))
                                                    (second (combinatorial-decomposition-lattices lat pair)))
                             lat)
                         (println "TRUE")
                         (println "FALSE")))
)
)


(defn add-obj [ctx obj incidence] 
  "Returns a new context with obj added."
  (make-context (union (objects ctx) #{obj}) (attributes ctx) incidence))


(defn add-attr [ctx attr incidence] 
  (make-context (objects ctx) (union (attributes ctx) #{attr}) incidence))
  


(defn distributive-exploration [ctx empty-ctx]
  (let [objs (objects ctx) 
        attr (attributes ctx)
        incidence (incidence ctx)
        start empty-ctx]

      (loop [subctx start
             rest-objs objs
             rest-attr attr
             object-next true]

(println (count (objects subctx)))

         (let [new-obj (first (filter #(distributive? (concept-lattice (add-obj subctx % incidence))) rest-objs))
               new-attr (first (filter #(distributive? (concept-lattice (add-attr subctx % incidence))) rest-attr))]

            (if (not (or new-obj new-attr))
               subctx
               (if (and object-next new-obj)
                  (recur (add-obj subctx new-obj incidence) 
                         (difference rest-objs #{new-obj})
                         rest-attr
                         false)
               (if (and (not object-next) new-attr)
                  (recur (add-attr subctx new-attr incidence) 
                         rest-objs
                         (difference rest-attr #{new-attr})
                         true)
                  (recur subctx
                         rest-objs
                         rest-attr
                         (not object-next)))))
         )
      )))

(for [ctx ctx-list] (println (concept-lattice (read-context ctx))))

(def pair-list (for [ctx ctx-list] (libkin-decomposition-pairs (concept-lattice (reduce-context (read-context ctx))))))

(defn check-combinatorial-decomps [ctx-list]
  (for [ctx ctx-list]
    (let [lat (concept-lattice (reduce-context (read-context ctx)))
          decomposition-pairs (libkin-decomposition-pairs lat)]
      (for [pair decomposition-pairs]
        (let [decomp (combinatorial-decomposition-lattices lat pair)]
          (println (str ctx ":  " (= (combinatorial-product (first decomp) (second decomp)) lat))))

))))


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

(defn list-all-decomps [ctx-list]
  (for [ctx ctx-list
        pair (libkin-decomposition-pairs (concept-lattice (read-context ctx)))]
     (println (str ctx ": " (libkin-decomposition-lattices (concept-lattice (read-context ctx)) pair)))))

;(use 'conexp.io.contexts)
;(def ctx (read-context "testing-data/Bird-Diet.ctx"))
;(make-context #{} #{} (incidence ctx))
;(write-context :burmeister (distributive-exploration (read-context "testing-data/Bird-Diet.ctx") ectx) "ird-diet-d.ctx")

(def ctx (read-context "drive_concepts_for_motorcars-d.ctx"))

(def drive-objs #{"All-wheel" "Conventional" "Front-wheel" "Mid-engine" "Rear-wheel"})

(def drive-attrs #{"C-h" "C-l" "C-vl" "De+" "De++" "Dl+"
                   "Dl++" "Dl-" "E+" "E++" "E-" "E--"
                   "M-" "M--" "R+" "R++" "S-n" "S-u/n"})

(def drive-ctx (make-context drive-objs drive-attrs (incidence ctx)))


(def drive-ctx1 (make-context drive-objs #{"C-h" "C-l"} (incidence ctx)))

(def drive-ctx2 (make-context drive-objs #{"C-vl" "De+" "De++" "Dl+"
                   "Dl++" "Dl-" "E+" "E++" "E-" "E--"
                   "M-" "M--" "R+" "R++" "S-n" "S-u/n"} (incidence ctx)))



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
