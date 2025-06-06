(ns conexp.fca.random-contexts
  "provides random generators for formal contexts"
  (:require [conexp.fca.implications :refer [canonical-base]]
            [conexp.base :refer [set-of exists forall => defalias]]
            [conexp.fca.contexts :as contexts]
            [clojure.set :refer [subset? difference union intersection select]]
            [clojure.math.numeric-tower :refer [expt]]
            [clojure.math.combinatorics :refer [cartesian-product]]
            )
  (:import [org.apache.commons.math3.distribution
            GammaDistribution 
            EnumeratedIntegerDistribution 
            UniformIntegerDistribution 
            EnumeratedDistribution]
           [org.apache.commons.math3.util Pair]
           ))



(defn- normalizeVector
  "given positive(!) vector v return v/||v||_1"
  [v]
  {:pre [(every? #(>= % 0) v)
         (some #(> % 0 ) v)]}
  (let [ sum (reduce + v)] (map #(/ % sum) v)))


(defn- dispatch-makeContext 
  [att obj obj_att_nums]
  (cond    
    (and (pos-int? att) (pos-int? obj))
    :att-num-obj-num
    (and (coll? att) (coll? obj))
    :att-coll-obj-coll
    (and (coll? att) (int? obj))
    :att-coll-obj-num))


(defmulti make-context-from-attribute-numbers-per-object 
  "generate random context for given number of attributes, objects and 
  a vector containing the numbers of attributes for each object"
  dispatch-makeContext
  )

(defalias makeContext make-context-from-attribute-numbers-per-object)

(defmethod make-context-from-attribute-numbers-per-object :att-num-obj-num 
  [att obj obj_att_nums]
  (contexts/make-context-from-matrix
   (vec (range obj))
   (vec (range att))
   (flatten (map #(shuffle (into (vec (take %1 (repeat 1))) (vec (take (- att %1) (repeat 0))))) 
                 obj_att_nums))))


;; (defmethod make-context-from-attribute-numbers-per-object :att-coll-obj-coll
;;   [att obj obj_att_nums]
;;   (let [matrix (flatten (map #(shuffle (into (vec (take % (repeat 1))) 
;;                                              (vec (take (- (count  att) %) (repeat 0))))) obj_att_nums))]
;;     (contexts/make-context-from-matrix
;;      (into [] obj)
;;      (into [] att)
;;      matrix)))

(defmethod make-context-from-attribute-numbers-per-object :att-coll-obj-coll
  [att obj obj_att_nums]
  (let [att-count (count att)
        matrix (mapcat #(shuffle (concat (repeat % 1)
                                    (repeat (- att-count %) 0)))
                        obj_att_nums)]
    (contexts/make-context-from-matrix
     (into [] obj)
     (into [] att)
     (flatten matrix))))


(defmethod make-context-from-attribute-numbers-per-object :att-coll-obj-num
  [att obj obj_att_nums]
  (let [matrix (flatten (map #(shuffle (into (vec (take % (repeat 1))) 
                                             (vec (take (- (count  att) %) (repeat 0))))) obj_att_nums))]
    (contexts/make-context-from-matrix
     (vec (range obj))
     (into [] att)
     matrix)))



(defn- createGammaDistr
   "returns a sample of a gamma distribution with base-measure alpha and prescision-parameter 1"
   ([] (.sample (GammaDistribution. 1 1)))
   ([alpha] (if (> alpha 0) (.sample (GammaDistribution. alpha 1)) 0))
)


(defn- createDirichletDistr
  "
  create a dirichlet distribution for a given list of base-measure-values
  base-measure is expected to be a non-empty vector with some values > 0
  precision-parameter is expected to be a scalar > 0
  "
  [& {base-measure :base-measure 
    precision-parameter :precision-parameter 
    :or {base-measure [1] 
         precision-parameter 1}}]
  {:pre [(some #(> % 0) base-measure)
         (> precision-parameter 0)]}
  (let [alpha (map #(* precision-parameter %) base-measure)
        gamma_rvs (map createGammaDistr alpha) 
        gamma_rvs_sum (reduce + gamma_rvs) 
        dirichlet_rvs (map #(/ % gamma_rvs_sum) gamma_rvs)]
    dirichlet_rvs))


(defn- createCategoricalDistribution
  "given a vector of categories and a probabilities vector returns a categorical distribution"
  ([categories-probabilities-vector]
   (EnumeratedDistribution. (map #(Pair. (first %) (double (second %))) 
                                 categories-probabilities-vector)))
  ([categories probabilities]
   {:pre [(= (count categories) (count probabilities))
          (vector? categories)
          (vector? probabilities)]}
   (EnumeratedDistribution. (map #(Pair. (first %) (double (second %))) 
                                 (map vector categories probabilities)))))


(defn- createCategoricalDistributionFromDirichlet
  "
  create categorical distribution based on a dirichlet distribution with :base-measure @base-measure and :precision-parameter @precision-parameter
  base-measure is expected to be a non-empty vector with values > 0
  precision-parameter is expected to be a scalar > 0  
  "
  [& {base-measure :base-measure 
      precision-parameter :precision-parameter 
      :or {base-measure [1] 
           precision-parameter 1}}] 
  (let [dirichlet_rvs (double-array (createDirichletDistr :base-measure base-measure 
                                                          :precision-parameter precision-parameter))
        values (int-array (range (count dirichlet_rvs)))]
    (EnumeratedIntegerDistribution. values dirichlet_rvs)
    ))


(defn- dispatch-random-dirichlet-context
  "dispatch function for the random dirichlet context generator"
  [& {:keys [attributes objects base-measure precision-parameter]}]
  {:pre [(or (coll? attributes) (pos-int? attributes))
         (or (coll? objects) (pos-int? objects) (= 0 objects) (nil? objects))
         (or (number? precision-parameter) (nil? precision-parameter))
         (or (vector? base-measure) (nil? base-measure))
         (if (vector? base-measure) 
           (= (count base-measure) (+ 1 (if (coll? attributes) (count attributes) attributes)))
           (nil? base-measure))
         ]}
  (cond
    (and (coll? attributes) (coll? objects))
    :attr-coll-obj-coll
    (and (coll? attributes) (or (pos-int? objects) (= 0 objects) (nil? objects) false))
    :attr-coll-obj-num
    (and (pos-int? attributes) (or (pos-int? objects) (= 0 objects) (nil? objects) false))
    :attr-num-obj-num
    true
    :oops))

(defmulti random-dirichlet-context 
  "
  generate a formal context with :attributes attributes.
  defaults to a random number of objects based on a dirichlet distribution with base-measure (a_1 ... a_N)=(1 ... 1) 
  and precision-parameter 0.1*N where N is the number of attributes
  parameters: 
  :attributes - collection of attributes or number of attributes (required)
  :objects - collection of objects or number of objects
  :base-measure - the base-measure parameter vector (will be normalized to ||.||_1 = 1)
  :precision-parameter - the precision-parameter determins how close the probability vectors generated by the dirichlet distribution are to the base-measure; higher precision parameters result in probability vectors closer to the base measure
  "
  dispatch-random-dirichlet-context)


(defn- contains-maximal-contranominal-scale?
  "Given a formal context K, computes whether K contains the largest contranominal scale, i.e., if there are objects such that each set of |M|-1 attributes is an intent."
  [K]
  (let [M (set (contexts/attributes K))
        G (set (contexts/objects K))]
    (loop [g (first G)
           restG (rest G)
           S (set (for [m M] (difference M #{m})))]
      (cond (empty? S)
            true
            (nil? g)
            false
            true
            (recur (first restG)
                   (rest restG)
                   (difference S #{(set (contexts/object-derivation K #{g}))}))))))


(defn random-dirichlet-context-no-maximal-contranominal
  "Try to generate a random dirichlet context that contains no maximal contranominal scale.
  Try at most 10 times.
  The parameters are the same as for random-dirichlet-context."
  [& {:keys [attributes objects base-measure precision-parameter]}]
  (loop [try 10
         generatedK (random-dirichlet-context :attributes attributes
                                        :objects objects
                                        :base-measure base-measure
                                        :precision-parameter precision-parameter)]
    (if (and (contains-maximal-contranominal-scale? generatedK) (< 0 try))
      (recur (dec try)
             (random-dirichlet-context :attributes attributes
                                        :objects objects
                                        :base-measure base-measure
                                        :precision-parameter precision-parameter))
      generatedK)))

(defn- init-base-measure 
  "normalizes base measure"
  [base-measure num_attributes]
  (normalizeVector (if base-measure base-measure (vec (take (+ 1 num_attributes) (repeat 1))))))


(defn- init-precision-parameter
  "initializes the precision parameter as 0.1 * (dimension of the base measure) such that the initial vector is [0.1 ... 0.1]"
  [precision-parameter base-measure]
  (if precision-parameter precision-parameter (* 0.1 (count base-measure))))


(defmethod random-dirichlet-context :attr-coll-obj-coll
  [& {:keys [attributes objects base-measure precision-parameter]}] 
  (let [base-measure (init-base-measure base-measure (count attributes))
        precision-parameter (init-precision-parameter precision-parameter base-measure)
        cat_dist (createCategoricalDistributionFromDirichlet :base-measure base-measure 
                                                             :precision-parameter precision-parameter)
        obj_attr_numbers (for [i (range (count objects))] (.sample cat_dist))] 
    (make-context-from-attribute-numbers-per-object attributes objects obj_attr_numbers)
    ))


(defmethod random-dirichlet-context :attr-coll-obj-num
  [& {:keys [attributes objects base-measure precision-parameter]}] 
  (let [objects (if objects objects 
                    (.sample (UniformIntegerDistribution. (count attributes) 
                                                          (expt 2 (count attributes)))))
        base-measure (init-base-measure base-measure (count attributes))
        precision-parameter (if precision-parameter precision-parameter (* 0.1 (count base-measure)))
        cat_dist (createCategoricalDistributionFromDirichlet :base-measure base-measure 
                                                             :precision-parameter precision-parameter)
        obj_attr_numbers (for [i (range objects)] (.sample cat_dist))
        object-coll (range objects)] 
    (make-context-from-attribute-numbers-per-object attributes object-coll obj_attr_numbers)
    ))


(defmethod random-dirichlet-context :attr-num-obj-num
  [& {:keys [attributes objects base-measure precision-parameter]}] 
  (let [objects (if objects objects 
                    (.sample (UniformIntegerDistribution. attributes (expt 2 attributes))))
        base-measure (init-base-measure base-measure attributes)
        precision-parameter (init-precision-parameter precision-parameter base-measure)
        cat_dist (createCategoricalDistributionFromDirichlet :base-measure base-measure 
                                                             :precision-parameter precision-parameter)
        obj_attr_numbers (for [i (range objects)] (.sample cat_dist))
        attribute-coll (map #(format "att_%s" %) (range attributes))
        object-coll (range objects)]
    (make-context-from-attribute-numbers-per-object attribute-coll object-coll obj_attr_numbers)
    ))


(defn imitate-context-with-dirichlet
  "imitate using dirichlet random contexts with relative frequencies of numbers of attributes as base measure and a high precision parameter"
  [ctx]
  (let [attr  (contexts/attributes ctx) 
        num_attr (count attr)
        objects  (contexts/objects ctx)
        num_objects (count objects)
        freq (frequencies (map #(count (contexts/object-derivation ctx [%])) objects))
        base-measure (->> (range (+ 1 num_attr)) (map #(freq %)) (map #(if % % 0)) (vec))
        precision-parameter (*  1000 num_attr)]
    (random-dirichlet-context :attributes attr 
                              :objects objects 
                              :base-measure base-measure 
                              :precision-parameter precision-parameter)))
    

(defn imitate-context-with-categorical
  "imitate context using relative frequencies as probabilities of categorical distribution"
  [ctx]
  (let [attr  (contexts/attributes ctx) 
        objects  (contexts/objects ctx)
        num_objects (count objects)
        freq (frequencies (pmap #(count (contexts/object-derivation ctx [%])) objects))
        valsum (reduce + (vals freq))
        probabilities (into {} (for [[k v] freq] [k (/ v valsum)]))
        cat_distr (createCategoricalDistribution probabilities)
        samples (for [i (range num_objects)] (.sample cat_distr))]
    (makeContext attr objects samples )))


(defn imitate-context-with-cointoss
  "imitate context based on density using cointosses (bernoulli experiments for each incidence)"
  [ctx]
  (let [attr  (contexts/attributes ctx) 
        num_attr (count attr)
        objects  (contexts/objects ctx)
        num_objects (count objects)
        density (/ (count (contexts/incidence-relation ctx)) (* num_attr num_objects))]
    (contexts/random-context objects attr density)))


(defn imitate-context-with-resampling
  "imitate context using resampling of objects"
  [ctx]
  (let [attr (contexts/attributes ctx)
        num_attr (count attr)
        objects (contexts/objects ctx)
        num_objects (count objects)
        samples (for [i (map inc (range num_objects))] [i (rand-nth (seq objects))])
        new-incidence (reduce into [] (for [[i obj] samples] 
                                        (for [att (contexts/object-derivation ctx #{obj})] 
                                          [i att])))]
    (contexts/make-context (map inc (range num_objects)) attr  new-incidence)))


(defn- filter-full-and-empty-rows-and-columns-once
  "remove all full columns and full rows and return the respective subcontext"
  [K]
  (let [G (contexts/objects K)
        M (contexts/attributes K)
        Gfiltered (select (fn [g] (let [gprime (contexts/object-derivation K [g])]
                                    (and (not= M gprime) (not= #{} gprime))))
                          G)
        Mfiltered (select (fn [m] (let [mprime (contexts/attribute-derivation K [m])]
                                    (and (not= G mprime) (not= #{} mprime))))
                          M)]
    (contexts/make-context Gfiltered Mfiltered (contexts/incidence K))))


(defn- filter-full-and-empty-rows-and-columns
  "remove all full columns/rows repeatedly from the context K until each remaining column/row has at least one 'x' and one '.'.
  return the set of objects and attributes that remain"
  [K]
  (loop [G (contexts/objects K)
         M (contexts/attributes K)
         K K]
    (let [Kfiltered (filter-full-and-empty-rows-and-columns-once K)
          Gfiltered (contexts/objects Kfiltered)
          Mfiltered (contexts/attributes Kfiltered)]
      (if (and (= G Gfiltered) (= M Mfiltered))
        Kfiltered
        (recur Gfiltered
               Mfiltered
               Kfiltered)))))


(defn randomize-context-by-edge-swapping
  "Given K=(G,M,I) swap incidences repeatedly selecting two objects and two attributes and swapping their incidences:
  |x|.|    |.|x| 
  |.|x| -> |x|.|
  i.e., selecting g,h in G and m,n in M such that (g,m),(h,n) in I, (g,n),(h,m) not in I, and then swapping the incidences.
  "
  ([K]
   (randomize-context-by-edge-swapping K (* 10 (count (contexts/incidence-relation K)))))
  ([K iterations]
   (assert (>= iterations 0)
           "need a positive number of iterations")
   (assert (not= (count (contexts/incidence-relation K)) 0)
           "context has empty incidence relation; no swaps possible")
   (assert (not= (count (contexts/incidence-relation K)) (* (count (contexts/objects K))
                                                            (count (contexts/attributes K))))
           "context has full incidence relation; no swaps possible")
   (let [G (contexts/objects K)
         M (contexts/attributes K)
         Kswappable (filter-full-and-empty-rows-and-columns K) ; obtain the subcontext where swapping edges is possible (to reduce the occurrence of failed swap attempts)
         Gfiltered (contexts/objects Kswappable)
         Mfiltered (contexts/attributes Kswappable)
         ]
     (loop [i 0 ;iteration counter
            j 0 ;failed iteration counter
            Kswappable Kswappable 
            I (contexts/incidence-relation K)]
       (if (>= j (+ 100 iterations))
         (do (println "Randomization stopped after " i
                      " successful swaps and " j " failed swap attempts")
             (contexts/make-context G M I)) 
         (if (>= i iterations)
           ;; (do (println "Randomization stopped after " i
           ;;              " successful swaps and " j " failed swap attempts")
           ;;     (contexts/make-context G M I)) 
           (contexts/make-context G M I)
           (let [g (rand-nth (into [] Gfiltered))
                 gprime (contexts/object-derivation Kswappable [g])
                 m (rand-nth (into [] gprime))
                 n (rand-nth (into [] (difference Mfiltered gprime)))
                 mprime (contexts/attribute-derivation Kswappable [m])
                 nprime (contexts/attribute-derivation Kswappable [n])
                 hpool (intersection (difference Gfiltered mprime)
                                     nprime)]
             (if (= #{} hpool) ; if there is no suitable object count the iteration as failed and repeat
               (recur i
                      (inc j)
                      Kswappable
                      I)
               (let [h (rand-nth (into [] hpool))]
                 ;; (println "remove " [g m] [h n])
                 ;; (println "add " [g n] [h m])
                 (let [Inew (union (difference I #{[g m] [h n]}) #{[g n] [h m]})]
                   (recur (inc i)
                          j
                          (contexts/make-context Gfiltered Mfiltered Inew)
                          Inew)))))))))))

(defalias imitate-context-with-edge-swapping randomize-context-by-edge-swapping)


(defn randomize-context-by-edge-rewiring
  "Given K=(G,M,I) randomize the incidence relation by repeatedly doing the following: select (g,m) in I, (h,n) not in I then add (h,n) to I and remove (g,m) from I"
  ([K]
   (randomize-context-by-edge-rewiring K (* 10 (count (contexts/incidence-relation K)))))
  ([K iterations]
   (assert (>= iterations 0)
           "need a positive number of iterations")
   (assert (not= (count (contexts/incidence-relation K)) 0)
           "context has empty incidence relation; no rewiring possible")
   (assert (not= (count (contexts/incidence-relation K)) (* (count (contexts/objects K))
                                                            (count (contexts/attributes K))))
           "context has full incidence relation; no rewiring possible")
   (let [G (contexts/objects K)
         M (contexts/attributes K)]
     (loop [i 0
            I (contexts/incidence-relation K)
            IC (difference (into #{} (map vec (cartesian-product G M)))I)]
       (if (>= i iterations)
         (contexts/make-context G M I)
         (let [[g m] (rand-nth (into [] I))
               [h n] (rand-nth (into [] IC))]
           (recur (inc i)
                  (-> I
                      (disj [g m])
                      (conj [h n]))
                  (-> IC
                      (disj [h n])
                      (conj [g m]))
                  )))))))

(defalias imitate-context-with-edge-rewiring randomize-context-by-edge-rewiring)
nil
