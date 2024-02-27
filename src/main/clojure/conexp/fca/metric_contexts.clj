(ns conexp.fca.metric-contexts
  (:require [conexp.base :refer :all]
            [conexp.fca.contexts :refer :all]
            [clojure.set :as set]))

(defprotocol Metric-Context

  (context [this] "Returns the underlying context.")

  (object-metrics [this] "Returns a map of the metrics on objects and their names/keys.")
  (attribute-metrics [this] "Returns a map of the metrics on attributes and their names/keys.")

  (object-distance [this metric-key obj1 obj2] "Computes the distance between two objects based on the specified metric.")
  (attribute-distance [this metric-key attr1 attr2] "Computes the distance between two attributes based on the specified metric."))

(deftype metric-context [ctx object-metrics attribute-metrics]

  Context
  (objects [this] (objects ctx))
  (attributes [this] (attributes ctx))
  (incidence [this] (incidence ctx))

  Metric-Context
  (context [this] ctx)
  (object-metrics [this] object-metrics)
  (attribute-metrics [this] attribute-metrics)
  (object-distance [this metric-key obj1 obj2] ((metric-key object-metrics) obj1 obj2))
  (attribute-distance [this metric-key attr1 attr2] ((metric-key attribute-metrics) attr1 attr2))

)

(defn object-hamming-template [ctx obj1 obj2]
  "Computes the Hamming distance between objects by comparing the incident attributes."
  (if (and (.contains (objects ctx) obj1)
           (.contains (objects ctx) obj2))
   (count (set/union (set/difference (object-derivation ctx #{obj1}) 
                                     (object-derivation ctx #{obj2}))
                     (set/difference (object-derivation ctx #{obj2}) 
                                     (object-derivation ctx #{obj1}))))))

(defn attribute-hamming-template [ctx attr1 attr2]
  "Computes the Hamming distance between attributes by comparing the incident objects."
  (if (and (.contains (attributes ctx) attr1)
           (.contains (attributes ctx) attr2))
   (count (set/union (set/difference (attribute-derivation ctx #{attr1}) 
                                     (attribute-derivation ctx #{attr2}))
                     (set/difference (attribute-derivation ctx #{attr2}) 
                                     (attribute-derivation ctx #{attr1}))))))


(defn create-object-hamming [ctx]
  "Returns a function that computes the Hamming distance between objects of the specified context."
  #(object-hamming-template ctx %1 %2)
)

(defn create-attribute-hamming [ctx]
  "Returns a function that computes the Hamming distance between attributes of the specified context."
  #(attribute-hamming-template ctx %1 %2)
)


(defn make-object-valuation [mctx dist-fn metric-name]
  "Returns a valuation function that displays the result of dist-fn on each nodes extent."
  #(dist-fn mctx metric-name (first %))
)

(defn make-attribute-valuation [mctx dist-fn metric-name]
  "Returns a valuation function that displays the result of dist-fn on each nodes intent."
  #(dist-fn mctx metric-name (second %))
)

(defn make-combined-valuation [obj-value-fn attr-value-fn]
  "Returns a valuation function that displays a tuple of the results of the specified function.
   The first of those functions is meant to compute valuations on extents, the second one on intents."
  #( identity [(obj-value-fn %) (attr-value-fn %)])
)


(defn add-object-metrics [mctx metrics]
  "Adds metrics to the context's object metrics. 
   The metrics need to be input as a map of names/keys and the corresponding functions."
  (metric-context. (context mctx) 
                   (merge (object-metrics mctx) metrics) 
                   (attribute-metrics mctx))
  )

(defn add-attribute-metrics [mctx metrics]
  "Adds metrics to the context's attribute metrics. 
   The metrics need to be input as a map of names/keys and the corresponding functions."
  (metric-context. (context mctx) 
                   (object-metrics mctx) 
                   (merge (attribute-metrics mctx) metrics))
  )

(defn remove-object-metric [mctx metric-name]
  "Removes the metric with the specified name/key from the context's object metrics."
  (metric-context. (context mctx) 
                   (dissoc (object-metrics mctx) metric-name) 
                   (attribute-metrics mctx))
  )

(defn remove-attribute-metric [mctx metric-name]
  "Removes the metric with the specified name/key from the context's attribute metrics."
  (metric-context. (context mctx) 
                   (object-metrics mctx) 
                   (dissoc (attribute-metrics mctx) metric-name))
  )


(defn convert-to-metric-context 
  (
   [ctx] 
   "Converts a context to a metric context. Adds hamming metrics by default."
   (metric-context. ctx {:o-hamm (create-object-hamming ctx)} {:a-hamm (create-attribute-hamming ctx)}))

  (
   [ctx object-metrics attribute-metrics]
   "Converts a context to a metric context and adds specified metrics. 
    The metrics need to be input as maps of names/keys and corresponding functions for both object and attribute metrics."
   (add-attribute-metrics (add-object-metrics (convert-to-metric-context ctx) object-metrics) attribute-metrics))
  )


(defn make-metric-context
  (
   [objects attributes incidence]
   "Creates a new metric context, based on its objects, attributes and incidence relation."
   (convert-to-metric-context (make-context objects attributes incidence)))
  
  (
   [objects attributes incidence object-metrics attribute-metrics]
   "Creates a new metric context, based on its objects, attributes and incidence relation, and adds metrics to the new context.
    The metrics need to be input as maps of names/keys and corresponding functions for both object and attribute metrics."
   (convert-to-metric-context (make-context objects attributes incidence) object-metrics attribute-metrics))
)



(defn max-object-distance 
  (
   [mctx metric-name]
   "Computes the maximum distance between objects of the context using the specified metric."
   (max-object-distance mctx metric-name (objects mctx))
   )
  
  (
   [mctx metric-name objs]
   "Computes the maximum distance between the specified objects using the specified metric.
    The maximum distance on an empty set of objects is by convention defined to be 0."
   (if (< (count objs) 2) 0
                         (apply max (for [obj1 objs
                                          obj2 (set/difference objs #{obj1})] 
                                            (object-distance mctx metric-name obj1 obj2)))))
)

(defn min-object-distance 
  (
   [mctx metric-name]
   "Computes the minimum distance between objects of the context using the specified metric."
   (min-object-distance mctx metric-name (objects mctx))
   )
  
  (
   [mctx metric-name objs]
   "Computes the minimum distance between the specified objects using the specified metric.
    The minimum distance on an empty set of objects is by convention defined to be 0."
   (if (< (count objs) 2) 0
                         (apply min (for [obj1 objs
                                          obj2 (set/difference objs #{obj1})] 
                                            (object-distance mctx metric-name obj1 obj2)))))
)

(defn average-object-distance 
  (
   [mctx metric-name]
   "Computes the average distance between objects of the context using the specified metric."
   (average-object-distance mctx metric-name (objects mctx))
   )
  
  (
   [mctx metric-name objs]
   "Computes the average distance between the specified objects using the specified metric.
    The average distance on an empty set of objects is by convention defined to be 0."
   (if (< (count objs) 2) 0
                         (apply #(/ (reduce + %) (count %)) [(for [obj1 objs 
                                                                   obj2 (set/difference objs #{obj1})] 
                                                                     (object-distance mctx metric-name obj1 obj2))])))
)


(defn max-attribute-distance 
  (
   [mctx metric-name]
   "Computes the maximum distance between attributess of the context using the specified metric."
   (max-attribute-distance mctx metric-name (attributes mctx))
   )
  
  (
   [mctx metric-name attrs]
   "Computes the maximum distance between the specified attributes using the specified metric.
    The maximum distance on an empty set of attributes is by convention defined to be 0."
   (if (< (count attrs) 2) 0
                          (apply max (for [attr1 attrs
                                           attr2 (set/difference attrs #{attr1})] 
                                             (attribute-distance mctx metric-name attr1 attr2)))))
)

(defn min-attribute-distance 
  (
   [mctx metric-name]
   "Computes the minimum distance between attributes of the context using the specified metric."
   (min-attribute-distance mctx metric-name (attributes mctx))
   )
  
  (
   [mctx metric-name attrs]
   "Computes the minimum distance between the specified attributes using the specified metric.
    The minimum distance on an empty set of attributes is by convention defined to be 0."
   (if (< (count attrs) 2) 0
                          (apply min (for [attr1 attrs
                                           attr2 (set/difference attrs #{attr1})] 
                                             (attribute-distance mctx metric-name attr1 attr2)))))
)

(defn average-attribute-distance 
  (
   [mctx metric-name]
   "Computes the average distance between attributes of the context using the specified metric."
   (average-attribute-distance mctx metric-name (attributes mctx))
   )
  
  (
   [mctx metric-name attrs]
   "Computes the average distance between the specified attributes using the specified metric.
    The average distance on an empty set of attributes is by convention defined to be 0."
   (if (< (count attrs) 2) 0
                     (apply #(/ (reduce + %) (count %)) [(for [attr1 attrs 
                                                               attr2 (set/difference attrs #{attr1})] 
                                                                 (attribute-distance mctx metric-name attr1 attr2))])))
)



(defn object-confusion-matrix [mctx metric-name & opts]
  "Return a matrix of all distances between objects computed using the specified metric.
   Also returns a vector denoting the order of entries.
   Use :norm to mormalize the distances."
  (let [objs (into [] (objects mctx)) 
        divisor (if (and opts (.contains opts :norm)) (max-object-distance mctx metric-name) 1)]
     [objs
     (into [] (for [obj1 objs]
       (into [] (for [obj2 objs]
         (/ (object-distance mctx metric-name obj1 obj2) divisor)))))])
)

(defn attribute-confusion-matrix [mctx metric-name & opts]
  "Return a matrix of all distances between attributes computed using the specified metric.
   Also returns a vector denoting the order of entries.
   Use :norm to mormalize the distances."
  (let [attrs (into [] (attributes mctx))
        divisor (if (and opts (.contains opts :norm)) (max-attribute-distance mctx metric-name) 1)]
     [attrs
     (into [] (for [attr1 attrs]
       (into [] (for [attr2 attrs]
         (/ (attribute-distance mctx metric-name attr1 attr2) divisor)))))])
)


;;;For the functions below, keep in mind that metrics may no longer work as intended after the context has been altered.

(defn dual-metric-context [mctx]
  "Computes the dual context of a metric context. Metrics remain unchanged, but object metrics become 
   attribute metrics and vice versa.."
  (convert-to-metric-context (dual-context (context mctx))
                             (attribute-metrics mctx)
                             (object-metrics mctx))
)

(defn invert-metric-context [mctx]
  "Computes the inverted context of a metric context. Metrics remain unchanged."
  (convert-to-metric-context (invert-context (context mctx))
                             (object-metrics mctx)
                             (attribute-metrics mctx))
)

(defn metric-context-union [mctx1 mctx2]
  "Computes the union of two metric contexts. The resulting metric context contains the metrics of both contexts.
   If metrics have the same name/key, those of the latter context are retained."
  (convert-to-metric-context (context-union (context mctx1) (context mctx2))
                             (merge (object-metrics mctx1) (object-metrics mctx2))
                             (merge (attribute-metrics mctx1) (attribute-metrics mctx2)))
)

(defn metric-context-disjoint-union [mctx1 mctx2]
  "Computes the disjoint union of two metric contexts. The resulting metric context contains the metrics of both contexts.
   If metrics have the same name/key, those of the latter context are retained."
  (convert-to-metric-context (context-disjoint-union (context mctx1) (context mctx2))
                             (merge (object-metrics mctx1) (object-metrics mctx2))
                             (merge (attribute-metrics mctx1) (attribute-metrics mctx2)))
)

(defn metric-context-intersection [mctx1 mctx2]
  "Computes the intersection of two metric contexts. The resulting metric context contains the metrics of both contexts.
   If metrics have the same name/key, those of the latter context are retained."
  (convert-to-metric-context (context-intersection (context mctx1) (context mctx2))
                             (merge (object-metrics mctx1) (object-metrics mctx2))
                             (merge (attribute-metrics mctx1) (attribute-metrics mctx2)))
)

(defn metric-context-composition [mctx1 mctx2]
  "Computes the composition of two metric contexts. The resulting metric context contains the metrics of both contexts.
   If metrics have the same name/key, those of the latter context are retained."
  (convert-to-metric-context (context-composition (context mctx1) (context mctx2))
                             (merge (object-metrics mctx1) (object-metrics mctx2))
                             (merge (attribute-metrics mctx1) (attribute-metrics mctx2)))
)

(defn metric-context-apposition [mctx1 mctx2]
  "Computes the apposition of two metric contexts. The resulting metric context contains the metrics of both contexts.
   If metrics have the same name/key, those of the latter context are retained."
  (convert-to-metric-context (context-apposition (context mctx1) (context mctx2))
                             (merge (object-metrics mctx1) (object-metrics mctx2))
                             (merge (attribute-metrics mctx1) (attribute-metrics mctx2)))
)

(defn metric-context-subposition [mctx1 mctx2]
  "Computes the subposition of two metric contexts. The resulting metric context contains the metrics of both contexts.
   If metrics have the same name/key, those of the latter context are retained."
  (convert-to-metric-context (context-subposition (context mctx1) (context mctx2))
                             (merge (object-metrics mctx1) (object-metrics mctx2))
                             (merge (attribute-metrics mctx1) (attribute-metrics mctx2)))
)

