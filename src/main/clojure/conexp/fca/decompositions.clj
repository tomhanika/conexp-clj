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
  "Computes the underlying context of a lattice by reading the incidence of all objects."
  "Does not work with the *lattice-product* method."
  (let [concepts (base-set lat)
        unions (reduce #(vector  (set/union (first %1) (first %2)) (set/union (second %1) (second %2))) concepts)
        objects (first unions)
        attributes (second unions)
        incidence (for [c concepts obj (first c) attr (second c)] [obj attr])]
    (make-context objects attributes incidence))
)

(defn interval [lat upper lower]
  "Returns a new Lattice Consisting of all Elements of *lat* that are Larger or Equal than *lower*
   and are Lesser or Equal then *upper*."
  (let [order (lattice-order lat)
        new-base-set (filter #(and (order % upper) (order lower %)) (base-set lat))]
    (make-lattice-nc new-base-set order))
)


(defn decomposition-pairs [lat]
  "Returns all Decomposition Pairs / Neutral Complemented Elements in *lat*."
  (let [neutral-elements (neutral-concepts lat)]
      (for [n neutral-elements c (element-complement n lat)] [n c])))


(defn downset-decomposition-lattices [lat decomp-pair]
  "Computes the Lattices Resulting from the Downset Decomposition on the 
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

(defn upset-decomposition-lattices [lat decomp-pair]
  "Computes the Lattices Resulting from the Upset Decomposition on the 
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

(defn downset-product [a b]
  "Computes the Downset Product of two Lattices that are Complemented Neutral Ideals is Another Lattice."
  (let [con1 (base-set a)
        con2 (base-set b)
        new-base-set (for [x con1 y con2]
                       [(set/union (first x) (first y))
                        (set/intersection (second x) (second y))])]
    (make-lattice new-base-set #(set/subset? (first %1) (first %2))))
)

(defn upset-product [a b]
  "Computes the Upset Product of two Lattices that are Complemented Neutral Ideals is Another Lattice."
  (let [con1 (base-set a)
        con2 (base-set b)
        new-base-set (for [x con1 y con2]
                       [(set/intersection (first x) (first y))
                        (set/union (second x) (second y))])]
    (make-lattice new-base-set #(set/subset? (first %1) (first %2))))
)

(defn decomposable? [lat]
  "Verifies Whether the Supplied Lattice has non-trivial Decomposition Pairs."
  (let [top (lattice-one lat)
        bot (lattice-zero lat)
        decomp-pairs (into #{} (decomposition-pairs lat))
        non-trivial (disj (disj decomp-pairs [top bot]) [bot top])]

        (not (empty? non-trivial)))
)


(defn maximally-decomposable-filters [lat]
  "Returns all Maximally Decomposable Principal Filters of the Supplied Lattice."
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


(defn- discard-non-maximal [intervals]
  "Receives a Set of Concept Lattices and Discards all that are not Maximal in Respect to Set Inclusion of the Base Set."
  (into [] (filter #(not (some (fn [x] (and (set/subset? (base-set %) (base-set x))
                                            (not= % x))) intervals)) intervals))
)

(defn maximally-decomposable-intervals [lat]
  "Returns all Maximally Decomposable Intervals of the Supplied Lattice."
  (loop [queue (into [] (base-set lat))
         intervals #{}]

      (if (empty? queue)

        (discard-non-maximal intervals)

        (let [current-upper (first queue)
              new-intervals (maximally-decomposable-filters (make-lattice-nc (order-ideal lat #{current-upper}) (inf lat) (sup lat)))]

          (recur (into [] (rest queue))
                 (set/union intervals new-intervals)))))
)




(defn direct-decomposition-lattice [lat]
  "Computes the (Upset) Direct Decomposition Lattice of the Supplied Concept Lattice."
  (let [pairs (decomposition-pairs lat)
        sublats  (set (flatten (for [p pairs] (upset-decomposition-lattices lat p))))]
    (make-lattice sublats #(set/subset? (base-set %1) (base-set %2))))
)

(defn prime-factorization [lat]
  "Returs a Prime Factorization of the Supplied Lattice."
  (lattice-atoms (direct-decomposition-lattice lat))
)


(defn obj-union [ctx1 ctx2]
  "Unifies the objects of *ctx1* and *ctx2*.
   *ctx1* and *ctx2* must have the same attributes."
  (if (not (= (attributes ctx1) (attributes ctx2)))
    (println "Contexts do not have the same attributes!")
    (make-context (set/union (objects ctx1) (objects ctx2)) 
                  (attributes ctx1) 
                  (set/union (incidence ctx1) (incidence ctx2))))

)

(defn obj-intersection [ctx1 ctx2]
  "Intersects the objects of *ctx1* and *ctx2*.
   *ctx1* and *ctx2* must have the same attributes."
  (if (not (= (attributes ctx1) (attributes ctx2)))
    (println "Contexts do not have the same attributes!")
    (make-context (set/intersection (objects ctx1) (objects ctx2)) 
                  (attributes ctx1) 
                  (set/intersection (incidence ctx1) (incidence ctx2))))

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

(defn ctx-decomposition-lattice [lat]
  "Computes the (Upset) Direct Decomposition Lattice in Terms of each Elements Underlying Formal Context."
  (let [pairs (decomposition-pairs lat)
        sublats  (set (flatten (for [p pairs] (upset-decomposition-lattices lat p))))
        subctxs (map context-from-lattice sublats)]

    (make-lattice subctxs attr-intersection attr-union))
)

