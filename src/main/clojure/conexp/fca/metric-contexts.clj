(ns conexp.fca.metric-contexts
  (:require [conexp.base :refer :all]
            [conexp.fca.contexts :refer :all]))

(defn object-hamming [ctx obj1 obj2]
  (if (and (.contains (objects ctx) obj1)
           (.contains (objects ctx) obj2))
    (- (count (objects ctx))
       (count (object-derivation ctx #{obj1 obj2}))
       (count (object-derivation (invert-context ctx) #{obj1 obj2})))))

(defn attribute-hamming [ctx attr1 attr2]
  (if (and (.contains (attributes ctx) attr1)
           (.contains (attributes ctx) attr2))
    (- (count (attributes ctx))
       (count (attribute-derivation ctx #{attr1 attr2}))
       (count (attribute-derivation (invert-context ctx) #{attr1 attr2})))))

(defprotocol Context-Metrics

  (context [this])

  (o-metrics [this])
  (a-metrics [this])

  (o-dist [this metric-key obj1 obj2])
  (a-dist [this metric-key attr1 attr2]))

(deftype metric-context [ctx o-metrics a-metrics]

  Context
  (objects [this] (objects ctx))
  (attributes [this] (attributes ctx))
  (incidence [this] (incidence ctx))

  Context-Metrics
  (context [this] ctx)
  (o-metrics [this] o-metrics)
  (a-metrics [this] a-metrics)
  (o-dist [this metric-key obj1 obj2] ((metric-key o-metrics) this obj1 obj2))
  (a-dist [this metric-key attr1 attr2] ((metric-key a-metrics) this attr1 attr2))

)

(defn make-context-metric [ctx]
  (metric-context. ctx {:o-hamm object-hamming} {:a-hamm attribute-hamming}))

(defn make-metric-context [objects attributes incidence]
  (make-context-metric (make-context objects attributes incidence)))


(defn add-object-metric [mctx metric-key metric-func]
  (metric-context. (context mctx) 
                   (assoc (o-metrics mctx) metric-key metric-func) 
                   (a-metrics mctx))
  )

(defn add-attribute-metric [mctx metric-key metric-func]
  (metric-context. (context mctx) 
                   (o-metrics mctx) 
                   (assoc (a-metrics mctx) metric-key metric-func))
  )

(defn remove-object-metric [mctx metric-key]
  (metric-context. (context mctx) 
                   (dissoc (o-metrics mctx) metric-key) 
                   (a-metrics mctx))
  )

(defn remove-attribute-metric [mctx metric-key]
  (metric-context. (context mctx) 
                   (o-metrics mctx) 
                   (dissoc (a-metrics mctx) metric-key))
  )




;(def rctx (rand-context #{1 2 3 4} #{"A" "B" "C" "D"} 0.5))
;(def mctx (make-context-metric rctx))
