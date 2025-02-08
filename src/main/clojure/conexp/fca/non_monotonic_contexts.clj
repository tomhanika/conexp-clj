(ns conexp.fca.non-monotonic-contexts
  (:require [conexp.base :refer :all]
            [conexp.fca.posets :refer :all]
            [conexp.fca.posets :refer :all]
            [conexp.fca.contexts :refer :all]
            [clojure.set :as set]))

;Based on the Publication: 
;Non-monotonic Extensions to Formal Concept Analysis via Object Preferences
;10.48550/arXiv.2410.04184


;Based on Definition 8
(defprotocol Extended-Context

  (context [this] "Returns the underlying context.")

  (object-order [this] "Returns the partial order on the context's objects.")
  (attribute-order [this] "Returns the partial order on the context's attributes.")
  (object-order-explicit [this] "Returns an explicit set representation of the object order.")
  (attribute-order-explicit [this] "Returns an explicit set representation of the attribute order order.")
)

(deftype extended-context [ctx object-order attribute-order]

  Object
  (equals [this other]
    (generic-equals [this other] extended-context [ctx object-order attribute-order]))
  (hashCode [this]
    (hash-combine-hash extended-context ctx object-order attribute-order))

  Context
  (objects [this] (objects ctx))
  (attributes [this] (attributes ctx))
  (incidence [this] (incidence ctx))

  Extended-Context
  (context [this] ctx)
  (object-order [this] object-order)
  (attribute-order [this] attribute-order)
  (object-order-explicit [this] (relation-set (objects this) object-order))
  (attribute-order-explicit [this] (relation-set (attributes this) attribute-order))
)

(defmethod print-method extended-context [ectx out]
  (.write ^java.io.Writer out
          ^String (context-to-string ectx))
)


(defn make-extended-context 
  ([objs attrs incidence obj-order attr-order]
   "Creates Extended Formal Context from Sets of Objects and Attributes, the Incidence Relation
    and a Partial Order on the Objects and on the Attributes."
   (make-extended-context (make-context objs attrs incidence) obj-order attr-order)
   )
  ([ctx obj-order attr-order]
   "Creates Extended Formal Context from a Pre-existing Formal Context Partial Order 
   on the Objects and on the Attributes. Empty Collections and Values of nil are Interpreted
   as the Reflexive, but Otherwise Empty Order Relation."
   (let [objs (objects ctx)
         attrs (attributes ctx)
         obj-order (if (or (nil? obj-order) 
                           (and (coll? obj-order) 
                                (empty? obj-order))) = obj-order);add reflexivity of order is empty
         attr-order (if (or (nil? attr-order) 
                            (and (coll? attr-order) 
                                 (empty? attr-order))) = attr-order);add reflexivity of order is empty
         obj-poset (make-poset objs (relation-function objs obj-order));convert to poset to verify ordering
         attr-poset (make-poset attrs (relation-function attrs  attr-order))];convert to poset to verify ordering

        (extended-context. ctx (relation-function objs obj-order) (relation-function attrs attr-order)))
   )
)

;Based on Definition 9
(defn minimized-object-derivation [ectx objs]
  "Computes the Minimized Object Derivation of the Supplied Set of Objects on the 
   Supplied Extended Formal Context."
  (let [attr-order (attribute-order ectx)
        derivation (object-derivation ectx objs)
        minimal (fn [attr] (not-any? #(attr-order % attr) (disj derivation attr)))]
    (into #{} (filter minimal derivation)))
)

(defn minimized-attribute-derivation [ectx attrs]
  "Computes the Minimized Attribute Derivation of the Supplied Set of Attributes on the 
   Supplied Extended Formal Context."
  (let [obj-order (object-order ectx)
        derivation (attribute-derivation ectx attrs)
        minimal (fn [obj] (not-any? #(obj-order % obj) (disj derivation obj)))]
    (into #{} (filter minimal derivation)))
)

;Definition 12
(defn respects? [ectx A B]
  "Verifies Whether the Supplied Extended Formal Context Respects the Non-monotonic 
   Conditional A -> ¬ B."
  (= (set/intersection (minimized-attribute-derivation ectx A) (attribute-derivation ectx B)) #{})

)

;Definition 14
(defn typical-concepts [ectx]
  "Returns a Set of all Typical Concepts of an Extended Formal Context."
  (set (map #(vector (attribute-derivation ectx (object-derivation ectx (minimized-attribute-derivation ectx (second %))))
                     (object-derivation ectx (minimized-attribute-derivation ectx (second %)))) 
            (concepts ectx)))
)
