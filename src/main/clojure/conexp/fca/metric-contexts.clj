(ns conexp.fca.metric-contexts
  (:require [conexp.base :refer :all]
            [conexp.fca.contexts :refer :all]
            [clojure.set :as set]))

(defn object-hamming [ctx obj1 obj2]
  (if (and (.contains (objects ctx) obj1)
           (.contains (objects ctx) obj2))
   (count (set/union (set/difference (object-derivation ctx #{obj1}) 
                                     (object-derivation ctx #{obj2}))
                     (set/difference (object-derivation ctx #{obj2}) 
                                     (object-derivation ctx #{obj1}))))))

(defn attribute-hamming [ctx attr1 attr2]
  (if (and (.contains (attributes ctx) attr1)
           (.contains (attributes ctx) attr2))
   (count (set/union (set/difference (attribute-derivation ctx #{attr1}) 
                                     (attribute-derivation ctx #{attr2}))
                     (set/difference (attribute-derivation ctx #{attr2}) 
                                     (attribute-derivation ctx #{attr1}))))))

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
  (object-distance [this metric-key obj1 obj2] ((metric-key object-metrics) this obj1 obj2))
  (attribute-distance [this metric-key attr1 attr2] ((metric-key attribute-metrics) this attr1 attr2))

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

(defn remove-object-metrics [mctx metric-name]
  "Removes the metric with the specified name/key from the context's object metrics."
  (metric-context. (context mctx) 
                   (dissoc (object-metrics mctx) metric-name) 
                   (attribute-metrics mctx))
  )

(defn remove-attribute-metrics [mctx metric-name]
  "Removes the metric with the specified name/key from the context's attribute metrics."
  (metric-context. (context mctx) 
                   (object-metrics mctx) 
                   (dissoc (attribute-metrics mctx) metric-name))
  )


(defn convert-to-metric-context [ctx & metrics]
  "Converts a context to a metric context. Metrics to add to the new metric context may be specified.
   The metrics need to be input as maps of names/keys and corresponding functions for both object and attribute metrics.
   The first map will be interpreted as the object metrics. The second map for attribute metrics may be omitted. The first 
   map may be empty if only attribute metrics shall be added."
  (let [mctx (metric-context. ctx {:o-hamm object-hamming} {:a-hamm attribute-hamming})]
     (if (second metrics) (add-attribute-metrics (add-object-metrics mctx (first metrics)) (second metrics))
                          (if (first metrics) (add-object-metrics mctx (first metrics))
                                               mctx)))
)

(defn make-metric-context [objects attributes incidence & metrics]
  "Creates a new metric context, based on its objects, attributes and incidence relation.
   Metrics to add to the new metric context may be specified.
   The metrics need to be input as maps of names/keys and corresponding functions for both object and attribute metrics.
   The first map will be interpreted as the object metrics. The second map for attribute metrics may be omitted. The first 
   map may be empty if only attribute metrics shall be added."
  (apply convert-to-metric-context (cons (make-context objects attributes incidence) metrics)))



  
;(def rctx (rand-context #{1 2 3 4} #{"A" "B" "C" "D"} 0.5))
;(def mctx (convert-to-metric-context rctx))
