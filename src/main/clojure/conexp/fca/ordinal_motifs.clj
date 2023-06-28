(ns conexp.fca.ordinal-motifs
  (:require [clojure.core.reducers :as r]
            [conexp.base :refer :all]
            [conexp.fca.contexts :refer :all]
            [conexp.io.contexts :refer :all]
            [conexp.fca.implications :refer :all]
            [clojure.math.combinatorics :as comb]
            [clojure.algo.generic.functor :refer :all]
            [clojure.algo.generic.collection :as generic-col]
            [clojure.set :as set]))

;;;;;;;; ordinal motifs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;FCA Theorem 55

(defn nominally-measurable [ctx]
  (let [lat (concept-lattice ctx)]
     (= (lattice-atoms lat) (lattice-sup-irreducibles lat))
))

(defn ordinally-measurable [ctx] true)

(defn interordinally-measurable [ctx]
  (let [objects (objects ctx) attributes (attributes ctx)]
     (every? identity (concat
                       (for [g objects h objects]
       ;does not contravene {g}' subseteq {h}' => {g}'={h}' 
       (or (not (subset? (object-derivation ctx #{g}) (object-derivation ctx #{h})))
           (= (object-derivation ctx #{g}) (object-derivation ctx #{h}))))
     (for [m attributes]
        ;does not contravene (G\{m}')''=G\{m}'
        (= (attribute-derivation ctx (object-derivation ctx (set/difference objects (attribute-derivation ctx #{m}))))
           (set/difference objects (attribute-derivation ctx #{m}))))))
))

(defn contranominally-measurable [ctx]
  (interordinally-measurable ctx))

(defn dichotomically-measurable [ctx]
  (interordinally-measurable ctx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmulti generate-scale (fn [scale-type & rest] scale-type))

(defmethod generate-scale :nominal
  [_ n]
  (make-context (range 1 (inc n))
                (range 1 (inc n))
                =) )

(defmethod generate-scale :ordinal
  [_ n]
  (make-context (range 1 (inc n))
                (range 1 (inc n))
                <=) )

(defmethod generate-scale :interordinal
  [_ n]
  (let [leq-scale (make-context (range 1 (inc n))
                                (range 1 (inc n))
                                <=)
        geq-scale (make-context (range 1 (inc n))
                                (range 1 (inc n))
                                >=)]
    (context-apposition leq-scale geq-scale)) )

(defmethod generate-scale :contranominal
  [_ n]
  (make-context (range 1 (inc n))
                (range 1 (inc n))
                not=) )

(defmethod generate-scale :crown
  [_ n]
  (make-context (range 1 (inc n))
                (range 1 (inc n))
                (fn [i j] 
                  (or (contains? #{-1 0}  (- i j ))
                      (>= (-  i j) (dec n))) ) ) )

;;;;;;;; verify scale-measure using concepts
(defn isomorphic-closure-systems? 
  "Checks is two closure systems are equivalent with respect to object-map."
  [exts1 object-map exts2]
  (let [mapped-exts (->> exts1
                         (pmap (fn [ext] 
                                (->> ext (map object-map) set)))
                         set)]
    (= mapped-exts exts2)))

;;;;;;;; identify scale-measure using extents

(defn- map-from-tuple
  "For the two base-sets sequences returns a map from base-set to
  base-set2 that maps the elements depending on their position in the
  sequence."
  [base-set-seq base-set2-seq] 
  (->> (zip base-set-seq base-set2-seq)
       flatten 
       (apply hash-map)))

(defn- all-bijective-maps 
  "Returns a lazy sequence of all bijective maps from base-set into
  {1,...,n}"
  [base-set]
  (let [targets (->> base-set count inc (range 1))]
    (pmap (partial map-from-tuple base-set) (comb/permutations targets))))

(defn- one-bijective-map
  "Returns a bijective map from base-set into
  {1,...,n} depending on the position in the sequence."
  [base-set-seq]
  (let [targets (->> base-set-seq count inc (range 1))]
    (map-from-tuple base-set-seq targets)))

;; identify scale-measure using extents
(defn- restrict-extents-to-base-set 
  "For a collection of extents restricts them to the base-set. In case
  the extents are a closure system the result is the closure system on
  the smaller base-set."
  [exts base-set]
  (->> exts
       (pmap (fn [ext] 
               (intersection base-set ext)) )
       set))

(defn- identify-full-scale-measures-check-all
  "Checks if exts2-set is isomorph to the sub-closure system of exts1
  given by the base-set. Checks all bijective maps for this."
  [exts1 base-set exts2-set]
  (let [base-set-seq (seq base-set)
        exts1-restricted-to-base-set (restrict-extents-to-base-set exts1 base-set) 
        object-maps (all-bijective-maps base-set-seq)
        equal-sized (= (count exts1-restricted-to-base-set)
                       (count exts2-set)) ;; for computational speed-up
        ]
    (and equal-sized
         (some
          (fn [object-map]
            (isomorphic-closure-systems? exts1-restricted-to-base-set
                                         object-map
                                         exts2-set))
          object-maps)) ))

(defn- identify-full-scale-measures-check-one
  "Checks if exts2-set is isomorph to the sub-closure system of exts1
  given by the base-set. Checks only one bijective map for this. Used
  in case all bijective maps within exts2 are automorphisms."
  [exts1 base-set exts2-set]
  (let [base-set-seq (seq base-set)
        exts1-restricted-to-base-set (restrict-extents-to-base-set exts1 base-set) 
        object-map (one-bijective-map base-set-seq)
        equal-sized (= (count exts1-restricted-to-base-set)
                       (count exts2-set)) ;; for computational speed-up
        ]
    (and equal-sized
         (isomorphic-closure-systems? exts1-restricted-to-base-set
                                      object-map
                                      exts2-set))))



(defn- extent-chain? 
  "Checks if the extents ordered by setinclusion is a linear order."
  [exts]
  (let [sorted-exts (sort-by exts count)
        sorted-exts-idxs (-> sorted-exts count dec range)]
    (every? (fn [i]
              (subset? (nth sorted-exts i)
                       (nth sorted-exts (inc i))))
            sorted-exts-idxs) ))

(defn- identify-full-scale-measures-check-ordinal
  "Checks if exts2-set is isomorph to the sub-closure system of exts1
  given by the base-set. Used in case exts2 is of ordinal scale."
  [exts1 base-set exts2-set]
  (let [base-set-seq (seq base-set)
        exts1-restricted-to-base-set (restrict-extents-to-base-set exts1 base-set) 
        equal-sized (= (count exts1-restricted-to-base-set)
                       (count exts2-set)) ;; for computational speed-up
        ]
    (and equal-sized
         (extent-chain? exts1-restricted-to-base-set)) ))

(defn- get-two-element-chain
  "Derives chain from extents of size 2. If no such chain exists, returns nil."
  [exts-tuples]
  (let [unique-elements (map first 
                             (filter #(= (second %) 1)
                                     (frequencies
                                      (reduce into '() exts-tuples))))]
    (if (not= (count unique-elements) 2)
      nil
      (loop [current-element (first unique-elements)
             order [current-element]
             remaining-exts (set exts-tuples)
             current-exts (filter #(contains? % current-element) remaining-exts)]
        (if (empty? remaining-exts)
          order
          (if (not= (count current-exts) 1)
            nil
            (let [next-element (first (difference
                                       (first current-exts)
                                       #{current-element}))
                  order (vec (concat order [next-element]))
                  remaining-exts (difference remaining-exts (set current-exts))
                  next-exts (filter #(contains? % next-element) remaining-exts)]
              (recur next-element order remaining-exts next-exts))))))))

(defn- identify-full-scale-measures-check-interordinal
  "Checks if exts2-set is isomorph to the sub-closure system of exts1
  given by the base-set. Used in case exts2 is of interordinal scale."
  [exts1 base-set exts2-set]
  (let [base-set-seq (seq base-set)
        exts1-restricted-to-base-set (restrict-extents-to-base-set exts1 base-set) 
        equal-sized (= (count exts1-restricted-to-base-set)
                       (count exts2-set)) ;; for computational speed-up
        ]
    (and equal-sized
         (let [exts1-two-elements (filter #(= (count %) 2) exts1-restricted-to-base-set)
               exts2-two-elements (filter #(= (count %) 2) exts2-set)
               equal-sized-two-elements (= (count exts1-two-elements)
                                           (count exts2-two-elements))]
           (and equal-sized-two-elements
                (let [two-element-chain-exts1 (get-two-element-chain exts1-two-elements)]
                  (if (nil? two-element-chain-exts1)
                    nil
                    (let [two-element-chain-exts2 (get-two-element-chain exts2-two-elements)
                          two-element-chains-exts1 [two-element-chain-exts1 
                                                    (vec (reverse two-element-chain-exts1))]
                          object-maps (map #(map-from-tuple % two-element-chain-exts2) 
                                          two-element-chains-exts1)]
                      (some
                       (fn [object-map]
                         (isomorphic-closure-systems? exts1-restricted-to-base-set
                                                      object-map
                                                      exts2-set))
                       object-maps)))))))))


(defn- sized-like-a-crown?
  "Checks if a set of extents is sized like a crown scale."
  [exts base-set crown-exts]
  (let [singletons (filter (fn [ext] (= 1 (count ext))) exts)
        pairs (filter (fn [ext] (= 2 (count ext))) exts)]
    (and (>= (count base-set) 2)
         (= (count exts)
            (count crown-exts))
         (= (count singletons) 
            (count base-set))
         (= (count pairs) 
            (count base-set))) ))

(defn- is-of-crown-scale?
  "Checks if a set of extents is sized like a crown scale. Should only be used in combination with sized-like-crown?"
  [exts]
  (let [pairs (filter (fn [ext] (= 2 (count ext))) exts)
        [cur & other] pairs
        [start second] (seq cur)]
    ;; checks if there is a cycle without shortcuts within pairs
    (loop [cur second
           remaining (set other)]
      (let [next-ext (some (fn [ext] (if (contains? ext cur)
                                       ext))
                           remaining)
            next (if next-ext (first (disj next-ext cur)))
            next-remaining (if next-ext 
                             (disj remaining next-ext)
                             remaining)]
        (if next-ext
          (if (and (= next start)
                   (empty? next-remaining)) 
            true
            (recur next next-remaining))
          false)) ) ))

(defn identify-full-scale-measures-check-crown
  "Checks if exts2-set is isomorph to the sub-closure system of exts1
  given by the base-set. Used in case exts2 is of crown scale."
  [exts1 base-set exts2-set]
  (let [base-set-seq (seq base-set)
        exts1-restricted-to-base-set (restrict-extents-to-base-set exts1 base-set)]
    
    (and (sized-like-a-crown? exts1-restricted-to-base-set base-set exts2-set) 
         (is-of-crown-scale? exts1-restricted-to-base-set)) ))

(defmulti identify-full-scale-measures (fn [scale & rest] scale))

(defmethod identify-full-scale-measures :nominal
  [_ exts1 base-set exts2-set]
  (identify-full-scale-measures-check-one exts1 base-set exts2-set))

(defmethod identify-full-scale-measures :contranominal
  [_ exts1 base-set exts2-set]
  (identify-full-scale-measures-check-one exts1 base-set exts2-set))

(defmethod identify-full-scale-measures :ordinal
  [_ exts1 base-set exts2-set]
  (identify-full-scale-measures-check-ordinal exts1 base-set exts2-set))

(defmethod identify-full-scale-measures :interordinal
  [_ exts1 base-set exts2-set]
  (identify-full-scale-measures-check-interordinal exts1 base-set exts2-set))

(defmethod identify-full-scale-measures :crown
  [_ exts1 base-set exts2-set]
  (identify-full-scale-measures-check-crown exts1 base-set exts2-set))


;;;;;;;;;;;;;;;;;;;;;

;; rough pre-selection of candidates to be checked for crown scales
(defn cycles-of-g
  "Returns for an object g all cycles that can be found using the
  neighbors relations. Here neighbors is a map from the objects to all
  other objects that have a shared attribute. Breadth first search."
  [ctx neighbours start-g]
  (let [obj-seq (into [start-g] (seq (disj (objects ctx) start-g)))
        obj-order (fn [obj] (doall (sort-by #(.indexOf obj-seq %) obj)))]
    (loop [[cycles-so-far & other :as debug] [[start-g]] 
           found-cycles #{} ]
      (let [candidates (get neighbours (last cycles-so-far))
            candidates (let [cycles-so-far-set (set cycles-so-far)]
                         (set (filter (fn [g2] 
                                         (or (= g2 start-g)
                                             (and (not (contains? cycles-so-far-set g2))
                                                  (not (some (fn [chain-element]
                                                               (and (not (= start-g chain-element))
                                                                    (not (= (last cycles-so-far) chain-element))
                                                                    (contains? (get neighbours chain-element)
                                                                               g2)) ) 
                                                             cycles-so-far)))) ) 
                                       candidates )))
            new-found-cycles (if (and  (contains? candidates start-g)
                                (<= 3 (count cycles-so-far)))
                        (conj found-cycles (conj cycles-so-far start-g)) 
                        found-cycles)
            candidates-seq (if (contains? candidates start-g)
                             (obj-order (disj candidates start-g))
                             (obj-order candidates))
            next-other (map (fn [next-g] (conj cycles-so-far next-g))
                                         candidates-seq)
            new-other (into other next-other)]
        (if (empty? new-other)
          new-found-cycles
          (recur new-other new-found-cycles))))))

(defn- shares-an-attribute-relation-map 
  "Returns a map given by the 'shares an attribute' relation between the
  objects. Diagonal is excluded."
  [ctx]
  (let [obj (objects ctx)]
    (reduce (fn [neighbours g]
              (let [attr (object-derivation ctx #{g})
                    g-neighbours (set (filter (fn [g2]
                                                (not (empty?
                                                      (intersection (object-derivation ctx #{g2}) 
                                                                    attr))))
                                              (disj obj g)))]
                (assoc neighbours g g-neighbours) )) 
            {}
            obj)))

(defn- crown-candidates 
  "Returns the set of all cycles within the set of objects using the
  'shares an attribute' relation."
  [ctx]
  (let [obj (objects ctx)
        neighbors (shares-an-attribute-relation-map ctx)]
    (->> obj 
         (pmap (fn [g] (cycles-of-g ctx neighbors g))) ;; all cycles per object using the 'shares an attribute relation' 
         (reduce into) ;; all cycles in G using the 'shares an attribute relation' 
         (map set) set) ;; set of sets
    ))



(defmulti scale-complex (fn [scale-type & rest] scale-type))

(defn- candidates-by-subset-heredity
  "Returns only those subsets of obj such that all subsets are in the complex."
  [complex obj subset-size]
  (let [n-1-sized-subsets (filter #(= (count %) (dec subset-size)) complex)
        n-1-sized-subsets (filter #(subset? % obj) n-1-sized-subsets)
        combination-candidates (comb/combinations n-1-sized-subsets 2)
        combination-candidates (filter 
                                #(= (count (intersection (first %) (second %)))
                                    (dec (dec subset-size)))
                                combination-candidates)
        combinations (set (map #(union (first %) (second %)) combination-candidates))
        combinations (filter
                      (fn [objects] 
                        (let [subsets (map set (comb/combinations (seq objects)
                                                                  (dec subset-size)))]
                          (every? #(contains? complex %) 
                                  subsets) ))
                      combinations)]
    combinations))

(defn- objects-sets-of-scale-type 
  "Filters object-sets to those that are of the scale-type"
  [object-sets scale-type exts scale-extents]
  (->> object-sets
       (r/filter (fn [object-set] 
                   (identify-full-scale-measures scale-type
                                                 exts object-set scale-extents)))
       r/foldcat))

(defmethod scale-complex :default
  [scale-type ctx]
    (let [exts (extents ctx)]
      (loop [subset-size 2 
             scale-complex (into #{#{}} (map (fn [g] #{g}) (objects ctx)))]
        (let [candidates (candidates-by-subset-heredity scale-complex (objects ctx) subset-size)
              scale-extents (-> scale-type (generate-scale subset-size) extents set)]
          (if (empty? candidates)
            scale-complex
            (let [found-full-scale-measures (objects-sets-of-scale-type candidates scale-type exts scale-extents)
                  new-scale-complex (into scale-complex found-full-scale-measures)]
              (recur (inc subset-size) new-scale-complex)) )) )) )

(defmethod scale-complex :crown
  [scale-type ctx]
  (let [exts (extents ctx)
        candidates (crown-candidates ctx)]
    (filter (fn [base-set]
              (let [exts2-set (set (extents (generate-scale :crown  (count base-set))))]
                (identify-full-scale-measures :crown exts base-set exts2-set)))
            candidates)) )



;;;;;;;; Scale complex
(defn maximal-sets [sets]
  (let [comparator (fn [s1] 
                     (fn [s2]
                       (and (subset? s1 s2)
                            (not= s1 s2)) ))
        sets (pmap set sets)]
    (r/foldcat (r/filter (fn [s1] (not (some (comparator s1) sets)))
                         sets))))

(defprotocol Complex 
  (context [this])
  (nominal-complex [this])
  (ordinal-complex [this])
  (interordinal-complex [this])
  (contranominal-complex [this])
  (crown-complex [this]))

(defrecord Scale-Complex [ctx nominal-complex 
                              ordinal-complex
                              interordinal-complex
                              contranominal-complex
                              crown-complex]
             Complex
             (context [this] ctx)
             (nominal-complex [this] nominal-complex)
             (ordinal-complex [this] ordinal-complex)
             (interordinal-complex [this] interordinal-complex)
             (contranominal-complex [this] contranominal-complex)
             (crown-complex [this] crown-complex))

(defn make-scale-complex [ctx]
  (let [nominal-complex (delay (scale-complex :nominal ctx))
        ordinal-complex (delay (scale-complex :ordinal ctx))
        interordinal-complex (delay (scale-complex :interordinal ctx))
        contranominal-complex (delay (scale-complex :contranominal ctx))
        crown-complex (delay (scale-complex :crown ctx))]
      (->Scale-Complex ctx nominal-complex
                           ordinal-complex
                           interordinal-complex
                           contranominal-complex
                           crown-complex) ))



(defmulti get-complex  (fn [complex scale-type & rest] scale-type))

(defmethod get-complex :nominal
  [s-complex _ & {:keys [maximal] :or {maximal false}}] 
  (let [complex (force (nominal-complex s-complex))]
    (if maximal 
      (maximal-sets complex)
      complex)))

(defmethod get-complex :ordinal
  [s-complex _ & {:keys [maximal] :or {maximal false}}] 
  (let [complex (force (ordinal-complex s-complex))]
    (if maximal 
      (maximal-sets complex)
      complex)))

(defmethod get-complex :interordinal
  [s-complex _ & {:keys [maximal] :or {maximal false}}] 
  (let [complex (force (interordinal-complex s-complex))]
    (if maximal 
      (maximal-sets complex)
      complex)))

(defmethod get-complex :contranominal
  [s-complex _ & {:keys [maximal] :or {maximal false}}] 
  (let [complex (force (contranominal-complex s-complex))]
    (if maximal 
      (maximal-sets complex)
      complex)))

(defmethod get-complex :crown
  [s-complex _ & {:keys [maximal] :or {maximal false}}] 
  (let [complex (force (crown-complex s-complex))]
    (if maximal 
      (maximal-sets complex)
      complex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defprotocol Ordinal-Motif-Covering-Protocol
  (ncovering [this n] [this n stats]))

(defrecord Ordinal-Motif-Covering [covering-seq]
  Ordinal-Motif-Covering-Protocol
  (ncovering 
    [this n] 
    (ncovering this n false))
  (ncovering [this n stats] 
    (let [covering (clojure.core/take n covering-seq)]
      (if stats
        covering
        (map first covering))))  )


(defn- ordinal-motif-covering-seq
  "Returns a lazy-seq of the "
  ([ordinal-motifs+stats normalized] (ordinal-motif-covering-seq ordinal-motifs+stats normalized #{}))
  ([ordinal-motifs+stats normalized covered-exts]
   (let [;; get max key
         [most-covering-motif {:keys [scale-type extents covering] :as stats}] 
                              (apply max-key (fn [[motif {:keys [covering extents]}]]
                                               (if normalized
                                                 (/ (count covering) (count extents))
                                                 (count covering))) 
                                     ordinal-motifs+stats)
         ;; update motif-concept-map by set difference
         updated-ordinal-motifs+stats (as-> ordinal-motifs+stats $
                                        (dissoc $ most-covering-motif)
                                        (fmap (fn [remaining-stats] 
                                                (update remaining-stats :covering #(difference % covering))) $ ))
         new-covered-exts (clojure.set/union covered-exts covering)]
     (cons [most-covering-motif stats]
           (lazy-seq (ordinal-motif-covering-seq updated-ordinal-motifs+stats normalized new-covered-exts ))))))

(defn- compute-ordinal-motifs+stats 
  "Computes a map for each ordinal motif base set H \\subseteq G to the extents of K[H,M]"
  [scale-complex scale-types]
  (let [ctx (context scale-complex)]
    (reduce merge {}
            (map (fn [scale-type] 
                   (r/fold merge (r/map (fn [base-set]                         
                                          (let [exts (asd-> base-set $
                                                            (make-context $
                                                                          (attributes ctx)
                                                                          (incidence ctx))
                                                            extents
                                                            (map (partial context-object-closure ctx) $ )
                                                            set)]
                                            (hash-map base-set  {:scale-type scale-type
                                                                 :extents exts
                                                                 :covering exts})))
                                        (get-complex scale-complex scale-type
                                                     :maximal true))))
                 scale-types)) ))

(defn greedy-motif-covering 
  [scale-complex & {:keys [scale-types normalized] :or {scale-types [:nominal :ordinal :interordinal :contranominal :crown]
                                                        normalized false}}]
  (let [ctx (context scale-complex)
        exts (extents ctx)
        ordinal-motifs+stats (compute-ordinal-motifs+stats scale-complex scale-types)]
    (->Ordinal-Motif-Covering
     (ordinal-motif-covering-seq ordinal-motifs+stats normalized)) ) )
