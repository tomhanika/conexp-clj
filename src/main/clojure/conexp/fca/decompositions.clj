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
            [conexp.io.contexts :refer :all]
            [conexp.fca 
             [contexts :refer :all]
             [metrics :refer :all]
             [lattices :refer :all]
             [posets :refer :all]]))


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

(def pair-list (for [ctx ctx-list] (libkin-decomposition-pairs (concept-lattice (read-context ctx)))))

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
