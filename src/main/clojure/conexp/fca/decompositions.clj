;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

#(ns conexp.fca.decompositions
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




;(def ctx (rad-context "testing-data/Bird-Diet.ctx"))
;(make-context #{} #{} (incidence ctx))
