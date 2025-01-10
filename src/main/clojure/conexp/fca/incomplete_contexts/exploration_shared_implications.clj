(ns conexp.fca.incomplete-contexts.exploration-shared-implications
  (:gen-class)
  (:require [clojure.set :refer :all]
            [clojure.core.reducers :as r]
            [clojure.math.combinatorics :refer [subsets]]
            [clojure.algo.generic.functor :refer [fmap]]
            [clojure.string :as s]
            [conexp.fca.closure-systems :refer [next-closed-set]]
            [conexp.fca.contexts :as cxts  :refer [Context make-context]]
            [conexp.fca.implications :refer [holds? respects? follows? premise conclusion make-implication clop-by-implications canonical-base parallel-canonical-base-from-clop close-under-implications pseudo-close-under-implications]]
            [conexp.io.contexts]
            [conexp.io.layouts :refer :all]
            [conexp.fca.lattices :refer [concept-lattice]]
            [conexp.gui.draw :refer [draw-concept-lattice]]
            [conexp.layouts.dim-draw :refer :all]
            [conexp.io.incomplete-contexts :as io]
            [conexp.fca.incomplete-contexts.incomplete-contexts :refer :all]
            [conexp.fca.incomplete-contexts.experts :refer :all]
            [conexp.fca.incomplete-contexts.conexp-interop :refer :all]
))




(defn- next-closure-by-implications
  "Given a set of attributes A from the base set M and a set of implications L on M, returns the next closed set for A"
  [A M L]
  (next-closed-set
   M
   (clop-by-implications L)
   A))


(defn- incomplete-context-supremum-generalized
  [cxt1 cxt2]
  (let [o1 (objects cxt1)
        o2 (objects cxt2)
        objs (union o1 o2)
        atts1 (attributes cxt1)
        atts2 (attributes cxt2)
        atts (union atts1 atts2)
        inz0 (into {} (for [g objs m atts] [[g m] unknown]))
        inz1 (incidence cxt1)
        inz2 (incidence cxt2)
        inz (merge inz0 inz1 inz2 (into {} (for [g (intersection o1 o2) m (intersection atts1 atts2)]
                                             [[g m] (information-supremum
                                                     (get inz1 [g m])
                                                     (get inz2 [g m]))])))]
    (make-incomplete-context objs atts inz)))



;;; base version - version below is a lot faster by reusing Ksup
;; (defn explore-shared-implications
;;   "
;;   M attributes #{}
;;   L0 background implications that hold for all experts (can be an empty set) #{}
;;   K maps experts to their example contexts (each expert has at least an empty example context) {E1 K1 E2 K2 ..}
;;   E set of experts (an expert can be asked if an implication holds in their view) #{}
;;   "
;;   [M L0 K E]
;;   (let [L L0
;;         R #{}
;;         C (make-incomplete-context #{} E #{})]
;;     (loop [L L
;;            R R
;;            C C
;;            K K]
;;       (if (= R (into #{} M))
;;         [L K C]
;;         (let [Ksup (subposition (vals K))
;;               RBoxDiamond (possibly-implied-attributes Ksup R)]
;;           (if (= R RBoxDiamond)
;;             (recur L
;;                    (next-closure-by-implications R M L)
;;                    C
;;                    K)
;;             (let [;; only ask about the attributes that are not in the premise
;;                   _ (println R)
;;                   attributes-to-ask-about (difference RBoxDiamond R)
;;                   ;; ask all experts about each of these attributes (i.e. ask about the implication R->m)
;;                   answers  (into {} (for [expert E
;;                                           m attributes-to-ask-about]
;;                                       [[expert m] (holds-in-view? expert (make-implication R #{m}))]))
;;                   ;; group the answers by attribute
;;                   answers-grouped-by-m (group-by (fn [[[e m] v]] m) answers)
;;                   ;; find the attributes that follow from R for all experts
;;                   ;; (i.e., check for each attribute if all experts answered with :yes it follows)
;;                   X ;; shared-attributes-following-from-R
;;                   (->> answers-grouped-by-m
;;                        (map (fn [[m vs]]
;;                               [m (every?
;;                                   identity
;;                                   (into []
;;                                         (map (fn [[k {v :follows}]] (known-true? v)) vs)))]))
;;                        (filter (fn [[k v]] (true? v)))
;;                        (map first)
;;                        (into #{}))
                  
;;                   ;; add R->X if X=/=R otherwise keep L unchanged
;;                   newL (if (= X R)
;;                          L
;;                          (union L #{(make-implication R X)}))
;;                   ;; collect the questions asked and the responses to add to the context C
;;                   new-C-objects (into #{} (for [m attributes-to-ask-about] (make-implication R #{m})))
;;                   new-C-incidences (->> answers
;;                                         (map (fn [[[e m] {v :follows}]] [[(make-implication R #{m}) e] v]))
;;                                         (into {}))
;;                   ;; add the new responses to C (by merging C with an incomplete context that contains the new information)
;;                   newC (incomplete-context-supremum C (make-incomplete-context new-C-objects E new-C-incidences))
;;                   ;; compute the next closure of R, i.e. the next premise
;;                   newR (next-closure-by-implications R M newL)
;;                   ;; group the answers by expert
;;                   answers-grouped-by-expert  (group-by (fn [[[e m] v]] e) answers)
;;                   ;; collect all counterexamples given per expert in a single incomplete context
;;                   counterexamples-per-expert
;;                   (->> answers-grouped-by-expert
;;                        (map (fn [[e vs]]
;;                               [e (->> vs
;;                                       (map (fn [[k {v :counterexample}]] v))
;;                                       (reduce incomplete-context-supremum
;;                                               (make-empty-incomplete-context M)))]))
;;                        (into {}))
;;                   ;; add the newly found counterexamples to the existing examples for each expert
;;                    newK (into {} (for [e E]
;;                                   [e (incomplete-context-supremum
;;                                       (get K e)
;;                                       (get counterexamples-per-expert e))]))]
;;               (recur newL
;;                      newR
;;                      newC
;;                      newK)))
;;           )))))


(defn explore-shared-implications
  "
  M attributes #{}
  L0 background implications that hold for all experts (can be an empty set) #{}
  K maps experts to their example contexts (each expert has at least an empty example context) {E1 K1 E2 K2 ..}
  E set of experts (an expert can be asked if an implication holds in their view) #{}
  "
  ([M E]
   (explore-shared-implications M
                                #{}
                                (into {} (for [expert E] [expert (make-incomplete-context #{} M #{})]))
                                E))
  ([M L0 K E]
   (let [L L0
         R #{}
         C (make-incomplete-context #{} E #{})]
     (loop [L L
            R R
            C C
            K K]
       (let [Ksup (subposition (vals K))
             Ratom (atom R)]
         ;; compute the next set R that should be asked about without recomputing Ksup
         (while (and (= @Ratom (possibly-implied-attributes Ksup @Ratom))
                     (not= @Ratom (into #{} M)))
           (swap! Ratom (fn [X] (next-closure-by-implications X M L))))
         ;; here @Ratom is the next R that should be asked about
         ;; or @Ratom = M and the algorithm should terminate
         (if (= @Ratom (into #{} M))
           [L K C]
           (let [;; get the attributes that could follow from R to ask about them
                 RBoxDiamond (possibly-implied-attributes Ksup @Ratom)
                 ;; only ask about the attributes that are not in the premise
                 attributes-to-ask-about (difference RBoxDiamond @Ratom)
                 ;; ask all experts about each of these attributes (i.e. ask about the implication R->m)
                 answers  (into {} (for [expert E
                                         m attributes-to-ask-about]
                                     [[expert m] (holds-in-view? expert (make-implication @Ratom #{m}))]))
                 _ (doseq [expert E] (inc-interaction-counter expert))
                 ;; group the answers by attribute
                 answers-grouped-by-m (group-by (fn [[[e m] v]] m) answers)
                 ;; find the attributes that follow from R for all experts
                 ;; (i.e., check for each attribute if all experts answered with :yes it follows)
                 X ;; shared-attributes-following-from-R
                 (->> answers-grouped-by-m
                      (map (fn [[m vs]]
                             [m (every?
                                 identity
                                 (into []
                                       (map (fn [[k {v :follows}]] (known-true? v)) vs)))]))
                      (filter (fn [[k v]] (true? v)))
                      (map first)
                      (into #{}))
                 
                 ;; add R->X if X != R otherwise keep L unchanged
                 newL (if (or (= X @Ratom) (= X #{}))
                        L
                        (union L #{(make-implication @Ratom X)}))
                 ;; collect the questions asked and the responses to add to the context C
                 new-C-objects (into #{} (for [m attributes-to-ask-about] (make-implication @Ratom #{m})))
                 new-C-incidences (->> answers
                                       (map (fn [[[e m] {v :follows}]] [[(make-implication @Ratom #{m}) e] v]))
                                       (into {}))
                 ;; add the new responses to C (by merging C with an incomplete context that contains the new information)
                 newC (incomplete-context-supremum C (make-incomplete-context new-C-objects E new-C-incidences))
                 ;; compute the next closure of R, i.e. the next premise
                 newR (next-closure-by-implications @Ratom M newL)
                 ;; group the answers by expert
                 answers-grouped-by-expert  (group-by (fn [[[e m] v]] e) answers)
                 ;; collect all counterexamples given per expert in a single incomplete context
                 counterexamples-per-expert
                 (->> answers-grouped-by-expert
                      (map (fn [[e vs]]
                             [e (->> vs
                                     (map (fn [[k {v :counterexample}]] v))
                                     (reduce incomplete-context-supremum
                                             (make-empty-incomplete-context M)))]))
                      (into {}))
                 ;; add the newly found counterexamples to the existing examples for each expert
                 newK (into {} (for [e E]
                                 [e (incomplete-context-supremum
                                     (get K e)
                                     (get counterexamples-per-expert e))]))
                 ;; print out question and experts to whom it is posed
                 #_ (do #_(println)
                        (println (latex (make-implication @Ratom attributes-to-ask-about)))
                        #_(println)
                        #_(println answers-grouped-by-expert)
                        (println (->> answers-grouped-by-m
                                      (map second)
                                      (map #(map (fn [[k ans]] (if (= (:follows ans) "x")
                                                                 [k (:follows ans) "-"]
                                                                 [k
                                                                  (:follows ans)
                                                                  (->> (:counterexample ans)
                                                                       (objects)
                                                                       (first))
                                                                  ])) % ))
                                      (reduce into [])
                                      (interpose "\n"))))]
             ;; start again with (possibly) enlarged L and K
             (recur newL
                    newR
                    newC
                    newK)))
         )))))


;;; this version also tests if the experts previous answers during this exploration already answer the question
;;; maybe also: test if the experts answers from all previous explorations already answer the question
;;; i.e. allow to provide the context C
(defn explore-shared-implications-use-C
  "
  M attributes #{}
  L0 background implications that hold for all experts (can be an empty set) #{}
  K maps experts to their example contexts (each expert has at least an empty example context) {E1 K1 E2 K2 ..}
  E set of experts (an expert can be asked if an implication holds in their view) #{}
  "
  ([M E]
   (explore-shared-implications-use-C M
                                      #{}
                                      (into {} (for [expert E] [expert (make-incomplete-context #{} M #{})]))
                                      E
                                      (make-incomplete-context #{} E #{})))
  ([M L0 K E C]
   (let [L L0
         R #{}]
     (loop [L L
            R R
            C C
            K K]
       (let [Ksup (subposition (vals K))
             Ratom (atom R)]
         ;; compute the next set R that should be asked about without recomputing Ksup
         (while (and (= @Ratom (possibly-implied-attributes Ksup @Ratom))
                     (not= @Ratom (into #{} M)))
           (swap! Ratom (fn [X] (next-closure-by-implications X M L))))
         ;; here @Ratom is the next R that should be asked about
         ;; or @Ratom = M and the algorithm should terminate
         (if (= @Ratom (into #{} M))
           [L K C]
           (let [;; get the attributes that could follow from R to ask about them
                 RBoxDiamond (possibly-implied-attributes Ksup @Ratom)
                 ;; only ask about the attributes that are not in the premise
                 attributes-to-ask-about (difference RBoxDiamond @Ratom)
                 ;; ask all experts about each of these attributes (i.e. ask about the implication R->m)
                 answers  (into {} (for [expert E
                                         m attributes-to-ask-about]
                                     [[expert m] (holds-in-view? expert (make-implication @Ratom #{m}))]))
                 ;; add an interaction to the counter of the experts for whom the implication does not already follow from previous answers
                 interactedE (reduce union #{}
                                     (for [e E]
                                       (if (follows? (make-implication @Ratom RBoxDiamond)
                                                     (certain-attribute-derivation C #{e}))
                                         #{}
                                         #{e}
                                         )))
                 _ (doseq [expert interactedE] (inc-interaction-counter expert))
                 ;; group the answers by attribute
                 answers-grouped-by-m (group-by (fn [[[e m] v]] m) answers)
                 ;; find the attributes that follow from R for all experts
                 ;; (i.e., check for each attribute if all experts answered with :yes it follows)
                 X ;; shared-attributes-following-from-R
                 (->> answers-grouped-by-m
                      (map (fn [[m vs]]
                             [m (every?
                                 identity
                                 (into []
                                       (map (fn [[k {v :follows}]] (known-true? v)) vs)))]))
                      (filter (fn [[k v]] (true? v)))
                      (map first)
                      (into #{}))
                 
                 ;; add R->X if X != R and X!= empty otherwise keep L unchanged
                 newL (if (or (= X @Ratom) (= X #{}))
                        L
                        (union L #{(make-implication @Ratom X)}))
                 ;; collect the questions asked and the responses to add to the context C
                 new-C-objects (into #{} (for [m attributes-to-ask-about] (make-implication @Ratom #{m})))
                 new-C-incidences (->> answers
                                       (map (fn [[[e m] {v :follows}]] [[(make-implication @Ratom #{m}) e] v]))
                                       (into {}))
                 ;; add the new responses to C (by merging C with an incomplete context that contains the new information)
                 newC (incomplete-context-supremum C (make-incomplete-context new-C-objects E new-C-incidences))
                 ;; compute the next closure of R, i.e. the next premise
                 newR (next-closure-by-implications @Ratom M newL)
                 ;; group the answers by expert
                 answers-grouped-by-expert  (group-by (fn [[[e m] v]] e) answers)
                 ;; collect all counterexamples given per expert in a single incomplete context
                 counterexamples-per-expert
                 (->> answers-grouped-by-expert
                      (map (fn [[e vs]]
                             [e (->> vs
                                     (map (fn [[k {v :counterexample}]] v))
                                     (reduce incomplete-context-supremum
                                             (make-empty-incomplete-context M)))]))
                      (into {}))
                 ;; add the newly found counterexamples to the existing examples for each expert
                 newK (into {} (for [e E]
                                 [e (incomplete-context-supremum
                                     (get K e)
                                     (get counterexamples-per-expert e))]))
                 ;; print out question and experts to whom it is posed
                 #_ (do #_(println)
                        (println (latex (make-implication @Ratom attributes-to-ask-about)))
                        #_(println)
                        #_(println answers-grouped-by-expert)
                        (println (->> answers-grouped-by-m
                                      (map second)
                                      (map #(map (fn [[k ans]] (if (= (:follows ans) "x")
                                                                 [k (:follows ans) "-"]
                                                                 [k
                                                                  (:follows ans)
                                                                  (->> (:counterexample ans)
                                                                       (objects)
                                                                       (first))
                                                                  ])) % ))
                                      (reduce into [])
                                      (interpose "\n"))))]
             ;; start again with (possibly) enlarged L and K
             (recur newL
                    newR
                    newC
                    newK)))
         )))))




(defn- incomplete-contexts-update-incidences
  "given an incomplete context and a partial map of incidences to update, updates the incidences"
  [cxt incidences]
  (make-incomplete-context (objects cxt) (attributes cxt) (merge (into {} (incidence cxt)) incidences)))


(defn- has-counterexample?
  "given an incomplete context and an implication check if the implication has a counterexample in the context"
  [cxt impl]
  (not (satisfiable? impl cxt)))


(defn- ?-reduce-cxt
  "reduce ? in the resulting context C that occur when simply merging multiple exploration results. Implications can have a ? even though they follow from the accepted implications or are rejected by a counterexample for an expert simply because the experts were not asked about them."
  [C K E]
  (let [f (fn
            [[[impl e] v]]
            (cond
              (follows? impl (certain-extent C #{e}))
              [[impl e] known-true]
              (has-counterexample? (get K e) impl)
              [[impl e] known-false]
              true
              [[impl e] unknown]
              ))]
    (->> (unknown-incidence-set C)
         (map f)
         (into {})
         (incomplete-contexts-update-incidences C)
         )))


(defn explore-all-shared-implications
  ""
  ([attributes experts]
   (explore-all-shared-implications
    attributes
    experts
    (into {} (for [expert experts] [expert (make-incomplete-context #{} attributes #{})]))
    (make-incomplete-context #{} experts #{})))
  ([attributes experts examples conditional-implication-context]
   (let [M attributes
         E experts
         PE (reverse (drop 1 (sort-by count (subsets (vec E)))))
         K examples
         C conditional-implication-context]
     (loop [S (first PE)
            remaining (rest PE)
            K K
            C C]
       (if (nil? S)
         [K C] ; note: actually unknown answers are being represented as false when returning due to artificial counterexamples being part of the context of counterexamples
         (let [L (certain-extent C S)
               Kselected (select-keys K S)
               [Lnew Knew Cnew] (explore-shared-implications M L Kselected S)
               Knext (merge K
                            (into {}
                                  (for [e E
                                        :let [cxt1 (get K e)
                                              cxt2 (get Knew e)]
                                        :when (some? cxt2)]
                                    [e (incomplete-context-supremum cxt1 cxt2)]
                                    )))
               Cnext (?-reduce-cxt (incomplete-context-supremum-generalized C Cnew) Knext E)]
           (recur (first remaining)
                  (rest remaining)
                  Knext
                  Cnext)
           ))))))


(defn explore-all-shared-implications-use-C
  ""
  ([attributes experts]
   (explore-all-shared-implications-use-C
    attributes
    experts
    (into {} (for [expert experts] [expert (make-incomplete-context #{} attributes #{})]))
    (make-incomplete-context #{} experts #{})))
  ([attributes experts examples conditional-implication-context]
   (let [M attributes
         E experts
         PE (reverse (drop 1 (sort-by count (subsets (vec E)))))
         K examples
         C conditional-implication-context]
     (loop [S (first PE)
            remaining (rest PE)
            K K
            C C]
       (if (nil? S)
         [K C] ; note: actually unknown answers are being represented as false when returning due to artificial counterexamples being part of the context of counterexamples
         (let [L (certain-extent C S)
               Kselected (select-keys K S)
               [Lnew Knew Cnew] (explore-shared-implications-use-C M L Kselected S C)
               Knext (merge K
                            (into {}
                                  (for [e E
                                        :let [cxt1 (get K e)
                                              cxt2 (get Knew e)]
                                        :when (some? cxt2)]
                                    [e (incomplete-context-supremum cxt1 cxt2)]
                                    )))
               Cnext (?-reduce-cxt (incomplete-context-supremum-generalized C Cnew) Knext E)]
           (recur (first remaining)
                  (rest remaining)
                  Knext
                  Cnext)
           ))))))


(defn explore-all-shared-implications-no-?-reductions
  ""
  ([attributes experts]
   (explore-all-shared-implications
    attributes
    experts
    (into {} (for [expert experts] [expert (make-incomplete-context #{} attributes #{})]))
    (make-incomplete-context #{} experts #{})))
  ([attributes experts examples conditional-implication-context]
   (let [M attributes
         E experts
         PE (reverse (drop 1 (sort-by count (subsets (vec E)))))
         K examples
         C conditional-implication-context]
     (loop [S (first PE)
            remaining (rest PE)
            K K
            C C]
       (if (nil? S)
         [K C] ; note: actually unknown answers are being represented as false when returning due to artificial counterexamples being part of the context of counterexamples
         (let [L (certain-extent C S)
               Kselected (select-keys K S)
               [Lnew Knew Cnew] (explore-shared-implications M L Kselected S)
               Knext (merge K
                            (into {}
                                  (for [e E
                                        :let [cxt1 (get K e)
                                              cxt2 (get Knew e)]
                                        :when (some? cxt2)]
                                    [e (incomplete-context-supremum cxt1 cxt2)]
                                    )))
               Cnext (incomplete-context-supremum-generalized C Cnew)]
           (recur (first remaining)
                  (rest remaining)
                  Knext
                  Cnext)
           ))))))


(defn explore-all-shared-implications-in-parallel
  ""
  ([attributes experts]
   (explore-all-shared-implications-in-parallel
    attributes
    experts
    (into {} (for [expert experts] [expert (make-incomplete-context #{} attributes #{})]))
    (make-incomplete-context #{} experts #{})))
  ([attributes experts examples conditional-implication-context]
   (let [M  attributes
         E  experts
         K  examples
         C  conditional-implication-context
         redf (fn
                ([] [{} (make-empty-incomplete-context #{})])
                ([[K C] [K2 C2]]
                 (let [Knew (merge K K2)]
                   [Knew (?-reduce-cxt
                          (incomplete-context-supremum-generalized C C2)
                          Knew
                          (keys Knew))])))
         
         mapf  (fn [e] (drop 1 (explore-shared-implications
                                M
                                #{}
                                {e (make-empty-incomplete-context M)}
                                #{e})))
         ]
     (r/fold redf (r/map mapf experts)))))


(defn explore-all-shared-implications-in-parallel-no-?-reductions
  ""
  ([attributes experts]
   (explore-all-shared-implications-in-parallel
    attributes
    experts
    (into {} (for [expert experts] [expert (make-incomplete-context #{} attributes #{})]))
    (make-incomplete-context #{} experts #{})))
  ([attributes experts examples conditional-implication-context]
   (let [M  attributes
         E  experts
         K  examples
         C  conditional-implication-context
         redf (fn
                ([] [{} (make-empty-incomplete-context #{})])
                ([[K C] [K2 C2]]
                 (let [Knew (merge K K2)]
                   [Knew (incomplete-context-supremum-generalized C C2)])))
         mapf  (fn [e] (drop 1 (explore-shared-implications
                                M
                                #{}
                                {e (make-empty-incomplete-context M)}
                                #{e})))
         ]
     (r/fold redf (r/map mapf experts)))))


