(ns conexp.fca.random-contexts
  "provides random generators for formal contexts"
  (:require [conexp.fca.implications :refer [canonical-base]]
            [conexp.base :refer [set-of exists forall => defalias]]
            [conexp.fca.contexts :as contexts]
            [clojure.set :refer [subset?]]
            [clojure.math.numeric-tower :refer :all]            
            )
  (:import [org.apache.commons.math3.distribution GammaDistribution EnumeratedIntegerDistribution UniformIntegerDistribution EnumeratedDistribution]
           [org.apache.commons.math3.util Pair]
           )
  (:gen-class) 
)



(defn- normalizeVector
  "given positive(!) vector v return v/||v||_1"
  [v]
  {:pre [(every? #(>= % 0) v)
         (some #(> % 0 ) v)]}
  (let [ sum (reduce + v)] (map #(/ % sum) v))
)

(defn- dispatch-makeContext 
  [att obj obj_att_nums]
  (cond    
    (and (pos-int? att) (pos-int? obj))
    :att-num-obj-num
    (and (coll? att) (coll? obj))
    :att-coll-obj-coll
    (and (coll? att) (int? obj))
    :att-coll-obj-num))


(defmulti ^:private  make-context-from-attribute-numbers-per-object 
  "generate random context for given number of attributes, objects and 
  a vector containing the numbers of attributes for each object"
  dispatch-makeContext
  )

(defalias ^:private makeContext make-context-from-attribute-numbers-per-object)

(defmethod ^:private make-context-from-attribute-numbers-per-object :att-num-obj-num 
  [att obj obj_att_nums]
  (contexts/make-context-from-matrix
   (vec (range obj))
   (vec (range att))
   (flatten (map #(shuffle (into (vec (take %1 (repeat 1))) (vec (take (- att %1) (repeat 0))))) 
                 obj_att_nums))))


(defmethod ^:private make-context-from-attribute-numbers-per-object :att-coll-obj-coll
  [att obj obj_att_nums]
  (let [matrix (flatten (map #(shuffle (into (vec (take % (repeat 1))) 
                                             (vec (take (- (count  att) %) (repeat 0))))) obj_att_nums))]
    (contexts/make-context-from-matrix
     (into [] obj)
     (into [] att)
     matrix)))


(defmethod ^:private make-context-from-attribute-numbers-per-object :att-coll-obj-num
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
   (EnumeratedDistribution. (map #(Pair. (first %) (double (second %))) categories-probabilities-vector))
   )

  ([categories probabilities]
   {:pre [(= (count categories) (count probabilities))
          (vector? categories)
          (vector? probabilities)]}
   (EnumeratedDistribution. (map #(Pair. (first %) (double (second %))) (map vector categories probabilities)))))


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
  (let [dirichlet_rvs (double-array (createDirichletDistr :base-measure base-measure :precision-parameter precision-parameter))
        values (int-array (range (count dirichlet_rvs)))]
    (EnumeratedIntegerDistribution. values dirichlet_rvs)
    ))


(defn dispatch-random-dirichlet-context
  ""
  [& {:keys [attributes objects base-measure precision-parameter]}]
  {:pre [(or (coll? attributes) (pos-int? attributes))
         (or (coll? objects) (pos-int? objects) (= 0 objects) (nil? objects))
         (or (number? precision-parameter) (nil? precision-parameter))
         (or (vector? base-measure) (nil? base-measure))
         (if (vector? base-measure) (= (count base-measure) (+ 1 (if (coll? attributes) (count attributes) attributes))) (nil? base-measure))
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


(defn- init-base-measure 
  "normalizes base measure"
  [base-measure num_attributes]
  (normalizeVector (if base-measure base-measure (vec (take (+ 1 num_attributes) (repeat 1))))))


(defn- init-precision-parameter
  ""
  [precision-parameter base-measure]
  (if precision-parameter precision-parameter (* 0.1 (count base-measure))))


(defmethod random-dirichlet-context :attr-coll-obj-coll
  [& {:keys [attributes objects base-measure precision-parameter]}] 
  (let [base-measure (init-base-measure base-measure (count attributes))
        precision-parameter (init-precision-parameter precision-parameter base-measure)
        cat_dist (createCategoricalDistributionFromDirichlet :base-measure base-measure :precision-parameter precision-parameter)
        obj_attr_numbers (for [i (range (count objects))] (.sample cat_dist))] 
    (makeContext attributes objects obj_attr_numbers)
    ))


(defmethod random-dirichlet-context :attr-coll-obj-num
  [& {:keys [attributes objects base-measure precision-parameter]}] 
  (let [objects (if objects objects 
                    (.sample (UniformIntegerDistribution. (count attributes) (expt 2 (count attributes)))))
        base-measure (init-base-measure base-measure (count attributes))
        precision-parameter (if precision-parameter precision-parameter (* 0.1 (count base-measure)))
        cat_dist (createCategoricalDistributionFromDirichlet :base-measure base-measure :precision-parameter precision-parameter)
        obj_attr_numbers (for [i (range objects)] (.sample cat_dist))] 
    (makeContext attributes objects obj_attr_numbers)
    ))


(defmethod random-dirichlet-context :attr-num-obj-num
  [& {:keys [attributes objects base-measure precision-parameter]}] 
  (let [objects (if objects objects 
                    (.sample (UniformIntegerDistribution. attributes (expt 2 attributes))))
        base-measure (init-base-measure base-measure attributes)
        precision-parameter (init-precision-parameter precision-parameter base-measure)
        cat_dist (createCategoricalDistributionFromDirichlet :base-measure base-measure :precision-parameter precision-parameter)
        obj_attr_numbers (for [i (range objects)] (.sample cat_dist))
        attribute-coll (map #(format "att_%s" %) (range attributes))
        object-coll (range objects)]
    (makeContext attribute-coll object-coll obj_attr_numbers)
    ))



