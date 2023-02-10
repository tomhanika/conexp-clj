(ns conexp.fca.incomplete-contexts.incomplete-contexts
  "Incomplete-Contexts"
  (:require [clojure.set :refer [subset? intersection difference union]]
            [conexp.base :refer [exists forall illegal-argument <=> hash-combine-hash sort-by-first map-by-fn with-str-out ensure-length ensure-seq defalias clojure-type clojure-coll clojure-fn to-set cross-product set-of disjoint-union]]
            [conexp.fca.implications :refer :all]
            [clojure.algo.generic.functor :refer [fmap]]
            [clojure.core.reducers :as r])
  (:import [conexp.fca.implications Implication]))

 

(defprotocol Incomplete-Context-Protocol
  (objects [ctx]    "Returns the objects of a context.")
  (attributes [ctx] "Returns the attributes of a context.")
  (incidence [ctx] "Returns a map that, given a pair [a b], returns the corresponding truth-value (known-true/known-false/unknown)"))



(def known-trues #{1 \1 "X" \X "x" \x  "true" :true true})
(def known-falses #{0 \0 "O" \O  "o" \o "." \. "false" :false false})
(def unknowns #{"U" \U "u" \u "?" \? "unknown" :unknown})
(def known-true "x")
(defalias ktrue known-true)
(def known-false ".") ;'.
(defalias kfalse known-false)
(def unknown "?")
(defalias ? unknown)
(def incomplete-context-possible-values #{known-true known-false unknown})

(defn map-true-false-unknown-to-x-o-?
  ""
  [in]
  (if (contains? known-trues in) known-true
      (if (contains? known-falses in) known-false
          (if (contains? unknowns in) unknown
              (illegal-argument "Value " in (type in) " can not be parsed as true false or unknown; the possible values are " known-trues known-falses unknowns)))))

(defn- filter-incidences
  ""
  [cxt incidence-filter-values]
  (filter #(contains? incidence-filter-values (second %)) (incidence cxt)))

(defn true-incidence
  "given an incomplete context returns the known true incidences"
  [cxt]
  (filter-incidences cxt known-trues))

(defn false-incidence
  "given an incomplete context returns the known false incidences"
  [cxt]
  (filter-incidences cxt known-falses))

(defn unknown-incidence
  "given an incomplete context returns the unknown incidences"
  [cxt]
  (filter-incidences cxt unknowns))

(defn true-or-unknown-incidences
  "given an incomplete context returns the true or unknown incidences"
  [cxt]
  (filter-incidences cxt (union known-trues unknowns)))

(defalias unknown-incidence-set unknown-incidence)

(defn known-true? [v] (= v known-true))
(defn known-false? [v] (= v known-false))
(defn unknown? [v] (= v unknown))

(defn information-supremum
  [v1 v2]
  (cond
    (= v1 v2)
    (if (nil? v1) unknown v1)
    (and (= v1 known-true) (or (= v2 unknown) (nil? v2)))
    known-true
    (and (= v1 known-false) (or (= v2 unknown) (nil? v2)))
    known-false
    (and (= v2 known-true) (or (= v1 unknown) (nil? v1)))
    known-true
    (and (= v2 known-false) (or (= v1 unknown) (nil? v1)))
    known-false
    (or (nil? v1) (nil? v2))
    unknown
    true
    (throw (IllegalArgumentException. "Input values not x,o,? or nil"))))


(deftype Incomplete-Context [objects attributes incidence]
  Object
  (equals [this other]
    (and (instance? Incomplete-Context other)
         (= objects (.objects ^Incomplete-Context other))
         (= attributes (.attributes ^Incomplete-Context other))
         (let [other-incidence (.incidence ^Incomplete-Context other)]
           (forall [g objects, m attributes]
             (<=> (incidence [g m])
                  (other-incidence [g m]))))))
  (hashCode [this]
    (hash-combine-hash Incomplete-Context objects attributes incidence))
  ;;
  Incomplete-Context-Protocol
  (objects [this] objects)
  (attributes [this] attributes)
  (incidence [this] incidence))


(defn incomplete-context-to-string
  "Returns a string representing the given incomplete context inco-cxt
  as a value-table."
  ([inco-cxt]
     (incomplete-context-to-string inco-cxt sort-by-first sort-by-first))
  ([inco-cxt order-on-objects order-on-attributes]
     (let [objs         (sort order-on-objects (objects inco-cxt))
           atts         (sort order-on-attributes (attributes inco-cxt))
           inz          (incidence inco-cxt)

           str          #(if (nil? %) "nil" (str %))

           max-obj-len  (reduce #(max %1 (count (str %2))) 0 objs)
           max-att-lens (loop [lens   (transient (map-by-fn #(count (str %)) atts))
                               values (seq inz)]
                          (if values
                            (let [[[_ m] w] (first values)
                                  len (count (str w))]
                              (recur (if (> len (lens m))
                                       (assoc! lens m len)
                                       lens)
                                     (next values)))
                            (persistent! lens)))]
       (with-str-out
         (ensure-length "" max-obj-len " ") " |" (for [att atts]
                                                   [(ensure-length (print-str att) (max-att-lens  att) " ") " "])
         "\n"
         (ensure-length "" max-obj-len "-") "-+" (for [att atts]
                                                   (ensure-length "" (inc (max-att-lens att)) "-"))
         "\n"
         (for [obj objs]
           [(ensure-length (str obj) max-obj-len)
            " |"
            (for [att atts]
              [(ensure-length (str (inz [obj att])) (max-att-lens att))
               " "])
"\n"])))))


(defn print-context
  "Prints the result of applying context-to-string to the given
  arguments."
  [ctx & args]
  (print (apply incomplete-context-to-string ctx args)))


(defmethod print-method Incomplete-Context [incomplete-ctx out]
  (.write ^java.io.Writer out ^String (incomplete-context-to-string incomplete-ctx)))


(defmethod print-dup Incomplete-Context [incomplete-cxt out]
  (print-ctor incomplete-cxt (fn [cxt out]
                               (print-dup (objects cxt) out)
                               (.write out " ")
                               (print-dup (attributes cxt) out)
                               (.write out " ")
                               (print-dup (incidence cxt) out)) out))



(defn incomplete-context?
  "Returns true iff thing is an incomplete context."
  [thing]
  (instance? Incomplete-Context thing))

(defalias ?-context? incomplete-context?)


(defn complete-incomplete-context?
  "checks if an incomplete context contains any unknown relations"
  [cxt]
  (not (.contains (vals (incidence cxt))  unknown)))

(defalias is-complete-incomplete-context? complete-incomplete-context?)


(defmulti make-incomplete-context
  "Constructs a many-valued context from a set of objects, a set of
  attributes and an incidence relation, given as set of triples [g m w]
  or as a function from two arguments g and m to values w."
  {:arglists '([objects attributes incidence])}
  (fn [& args] (vec (map clojure-type args))))


(defmethod make-incomplete-context [Object Object clojure-coll]
  [objs atts inz]
  (let [objs (to-set objs)
        atts (to-set atts)
        crossProductObjsAtts (cross-product objs atts)
        ]
    (->Incomplete-Context objs
                         atts
                         (merge (into {} (map #(vector % unknown) crossProductObjsAtts))
                                (if (map? inz)
                                  (do
                                    (when-not (subset? (set (keys inz)) crossProductObjsAtts)
                                      (illegal-argument "Incidence map for incomplete-context"
                                                        "must not contain additional keys."))
                                    
                                    inz)
                                  (loop [hash  (transient {})
                                         items inz]
                                    (if (empty? items)
                                      (persistent! hash)
                                      (let [[g m w] (flatten (first items))]
                                        (recur (if (and (contains? objs g)
                                                        (contains? atts m))
                                                 (assoc! hash [g m] (map-true-false-unknown-to-x-o-? w))
                                                 hash)
                                               (rest items))))))))))


(defmethod make-incomplete-context [Object Object clojure-fn]
  [objs atts inz]
  (let [objs (to-set objs)
        atts (to-set atts)]
    (->Incomplete-Context  objs
                          atts
                          (loop [hash  (transient {})
                                 items (cross-product objs atts)]
                            (if (empty? items)
                              (persistent! hash)
                              (let [[g m] (first items)]
                                (recur (if (and (contains? objs g)
                                                (contains? atts m))
                                         (assoc! hash [g m] (map-true-false-unknown-to-x-o-? (inz [g m])))
                                         hash)
                                       (rest items))))))))


(defn make-incomplete-context-from-matrix
  "Given objects G and attribute M and an incidence matrix constructs
  the corresponding context. G and M may also be numbers where they
  represent (range G) and (range M) respectively."
  [G M bits]
  (let [G (ensure-seq G),
        M (ensure-seq M),
        m (count G),
        n (count M)]
    (assert (= (* m n) (count bits))
            "Number of objects and attributes does not match the number of entries.")
    (->Incomplete-Context (to-set G) (to-set M)
                         (into {} (for [i (range (count G))
                                       j (range (count M))]
                                   [[(nth G i)
                                     (nth M j)]
                                     (map-true-false-unknown-to-x-o-? (nth bits (+ (* n i) j)))])))))


(defn make-single-object-incomplete-context
  "Given a single object, a set of attributes and two subsets of attributes that the object has and does not have make an incomplete context containing this object"
  [object attributes attributes-had attributes-not-had]
  (let [inz1 (for [m attributes-had] [[object m] known-true])
        inz2 (for [m attributes-not-had] [[object m] known-false])
        inz3 (for [m (difference attributes attributes-had attributes-not-had)] [[object m] unknown])
        inz (into {} (union inz1 inz2 inz3))]
    (make-incomplete-context #{object} attributes inz)))


(defn make-empty-incomplete-context
  "Given a set of attributes create an empty incomplete context"
  [attributes]
  (make-incomplete-context #{} attributes #{}))


(defn incomplete-subcontext
  "given an incomplete context a set of objects and a set of attributes returns the corresponding subcontext"
  ([cxt attrs]
   (incomplete-subcontext cxt (objects cxt) attrs))
  ([cxt objs attrs]
   {:pre [[(subset? objs (objects cxt))]
          [(subset? attrs (attributes cxt))]]}
   (let [new-inz (reduce #(into %2 %1) {} (map  #(hash-map  % (get (incidence cxt) %)) (cross-product objs attrs)))]
     (make-incomplete-context objs attrs new-inz))))


(defn incomplete-context-subposition
  "Returns context subposition of ctx-1 and ctx-2"
  ([contexts]
   (if (apply not= (map attributes contexts))
     (illegal-argument "Cannot do context subposition, since attribute sets are not equal."))
   (let [n (count contexts)
         new-objs (apply disjoint-union (map objects contexts))
         new-inz (fn [[[g i] m]]
                   ((incidence (nth contexts i)) [g m]))]
     (make-incomplete-context new-objs (attributes (first contexts)) new-inz))
   )
  ([cxt1 & contexts]
   (incomplete-context-subposition (into [cxt1] contexts))))


(defn subposition
  "computes the subposition of a collection of incomplete formal contexts
  incomplete-contexts: a collection of incomplete contexts"
  [incomplete-contexts]
  (let [M (attributes (first incomplete-contexts))
        [objs inz] (->> incomplete-contexts
                            (map-indexed (fn [i cxt] [i (objects cxt) (incidence cxt)]))
                            (map (fn [[i objs inci]] [i
                                                      (into #{} (map #(vector i %) objs))
                                                      (into {} (map (fn [[[g m] v]] [[[i g] m] v]) inci))]))
                            (map (fn [[i o inz]] [o inz]))
                            (reduce (fn [[o1 inz1] [o2 inz2]] [(into o1 o2) (into inz1 inz2)])))]
    (make-incomplete-context objs M inz)
    ))


(defn incomplete-context-subposition-with-disjoint-objectsets
  "computes the subposition of a collection of incomplete formal contexts
  incomplete-contexts: a collection of incomplete contexts"
  [incomplete-contexts]
  (let [M (attributes (first incomplete-contexts))
        [objs inz] (->> incomplete-contexts
                        (map-indexed (fn [i cxt] [i (objects cxt) (incidence cxt)]))
                        (map (fn [[i o inz]] [o inz]))
                        (reduce (fn [[o1 inz1] [o2 inz2]] [(into o1 o2) (into inz1 inz2)])))]
    (make-incomplete-context objs M inz)
    ))


(defn example-with-attribute-sets->example-with-incidence-map
  "Input: set of all attributes M and example [g #{atts-had} #{atts-not-had}]
  Output: [g inz] where inz = {[g m1] xo?, [g m2] xo?, ...}"
  [M [g atts-had atts-not-had]]
  [g (into {} (for [m M] (cond (contains? atts-had m)
                              [[g m] known-true]
                              (contains? atts-not-had m)
                              [[g m] known-false]
                              true
                              [[g m] unknown])))])


(defn add-row-to-incomplete-context
  "input incomplete context and an object with a map for all object attribute combinations [g {[g m1] value1 [g m2] value2 ...}] or a triple of object, attributes had and attributes not had [g #{atts-had} #{atts-not-had}]"
  [cxt row]
  (cond
    (= (count row) 2)
    (let [[g inz] row]
      (assert (not (contains? (objects cxt) g)) "The object already exists in this context.")
      (assert (map? inz) "The incidence is not a map containing a mapping for each attribute for the object")
      (assert (= (into #{} (attributes cxt)) (into #{} (map second (keys inz)))) "The incidence is not a map containing a mapping for each attribute for the object")
      (make-incomplete-context (conj (objects cxt) g)
                               (attributes cxt)
                               (into (incidence cxt) (fmap map-true-false-unknown-to-x-o-? inz))))
    (= (count row) 3)
    (let [M (attributes cxt)]
      (add-row-to-incomplete-context cxt (example-with-attribute-sets->example-with-incidence-map M row)))
    true
    (throw (AssertionError. "Wrong Input"))))


(defn add-or-update-object-incomplete-context
  "input incomplete context and an object with a map for all object attribute combinations [g {[g m1] value1 [g m2] value2 ...}] or a triple of object, attributes had and attributes not had [g #{atts-had} #{atts-not-had}]"
  [cxt row]
  (cond
    (= (count row) 2)
    (let [[g inz] row]
      (assert (map? inz) "The incidence is not a map containing a mapping for each attribute for the object")
      (assert (= (into #{} (attributes cxt)) (into #{} (map second (keys inz)))) "The incidence is not a map containing a mapping for each attribute for the object")
      (make-incomplete-context (conj (objects cxt) g)
                               (attributes cxt)
                               (into (incidence cxt) (fmap map-true-false-unknown-to-x-o-? inz))))
    (= (count row) 3)
    (let [M (attributes cxt)]
      (add-or-update-object-incomplete-context cxt (example-with-attribute-sets->example-with-incidence-map M row)))
    true
    (throw (AssertionError. "Wrong Input"))))


(defn incomplete-context-union
  "compute the union of two incomplete contexts with disjunct object sets and a common attribute set "
  [cxt1 cxt2]
  (let [o1 (objects cxt1)
        o2 (objects cxt2)
        a1 (attributes cxt1)
        a2 (attributes cxt2)
        i1 (incidence cxt1)
        i2 (incidence cxt2)]
    (assert (= a1 a2) "Attribute sets differ")
    (assert (= #{} (intersection o1 o2)) "Object sets are not disjunct")
    (make-incomplete-context (union o1 o2) a1 (into i1 i2))))


(defn incomplete-context-supremum
  "compute the supremum of two incomplete contexts with respect to the information order, i.e., x > ? and o > ? but x and o are incomparable.
  Fails if the supremum of x and o is constructed"
  [cxt1 cxt2]
  (let [o1 (objects cxt1)
        o2 (objects cxt2)
        a1 (attributes cxt1)
        a2 (attributes cxt2)
        i1 (incidence cxt1)
        i2 (incidence cxt2)
        inew (into {}
                   (for [g (union o1 o2)
                         m (union a1 a2)]
                     [[g m] (information-supremum (get i1 [g m])
                                                  (get i2 [g m]))]))]
    (make-incomplete-context (union o1 o2)
                             (union a1 a2)
                             inew)))



(defn certain-object-derivation
  "Computes set of attributes certainly common to all objects"
  [ctx objects]
  (let [inz  (incidence ctx)
        atts (attributes ctx)]
    (set-of m [m atts :when (forall [g objects] (= (inz [g m]) known-true))])))

(defalias obox certain-object-derivation)
(defalias certain-intent certain-object-derivation)
(defalias attributes-had certain-object-derivation)


(defn possible-object-derivation
  "Computes set of attributes possibly common to all objects"
  [ctx objects]
  (let [inz  (incidence ctx)
        atts (attributes ctx)]
    (set-of m [m atts :when (forall [g objects] (contains? #{known-true unknown} (inz [g m])))])))

(defalias odiamond possible-object-derivation)
(defalias possible-intent possible-object-derivation)


(defn certain-attribute-derivation
  "Computes the set of objects that certainly have all attributes"
  [ctx attributes]
  (let [inz  (incidence ctx)
        objs (objects ctx)]
    (set-of g [g objs :when (forall [m attributes] (= (inz [g m]) known-true))])))

(defalias abox certain-attribute-derivation)
(defalias certain-extent certain-attribute-derivation)


(defn possible-attribute-derivation
  "Computes the set of objects that possibly have all attributes"
  [ctx attributes]
  (let [inz  (incidence ctx)
        objs (objects ctx)]
    (set-of g [g objs :when (forall [m attributes] (contains? #{known-true unknown} (inz [g m])))])))

(defalias adiamond possible-attribute-derivation)
(defalias possible-extent possible-attribute-derivation)


(defn possible-intent-of-certain-extent
  ""
  [cxt attributes]
  (possible-intent cxt (certain-extent cxt attributes)))

(defalias abox->odiamond possible-intent-of-certain-extent)
(defalias possibly-implied-attributes possible-intent-of-certain-extent)
(defalias ABoxODiamond possible-intent-of-certain-extent)


(defn possibly-holds?
  "Returns true iff impl is satisfiable in given context ctx."
  [impl ctx]
  (subset? (conclusion impl) (possibly-implied-attributes ctx (premise impl))))

(defalias satisfiable? possibly-holds?)



(defn attributes-not-had
  "given an object and a context returns the attributes that the object does not have"
  [context objs]
  (let [inz (incidence context)
        attrs (attributes context)]
    (set-of m [m attrs :when (forall [g objs] (= (inz [g m]) known-false))])))




(defn is-counterexample?
  "given context K, implication impl and object g
  return true if g is a counterexample for impl in K
  and false if it is not"
  [K impl g]
  (let [prem (set (premise impl))
        concl (set (conclusion impl))]
    (and
     (subset? prem (obox K [g]))
     (not (empty? (intersection concl (difference (set (attributes K))
                                                  (odiamond K [g]))))))))

(defn is-potential-counterexample?
  "given context K, implication impl and object g
  return true if g is a potential counterexample for impl in K
  and false if it is not"
  [K impl g]
  (let [prem (set (premise impl))
        concl (set (conclusion impl))]
    (and
     (subset? prem (odiamond K [g]))
     (not (empty? (intersection concl (difference (set (attributes K))
                                                  (obox K [g]))))))))

(defn counterexamples-subcontext
  "Input: incomplete context K, implication impl
  Output: incomplete subcontext of counterexamples"
  [K impl]
  (let [counterexamples (filter (partial is-counterexample? K impl) (objects K))]
    (incomplete-subcontext K counterexamples (attributes K))))

(defn potential-counterexamples-subcontext
  "Input: incomplete context K, implication impl
  Output: incomplete subcontext of potential counterexamples"
  [K impl]
  (let [counterexamples (filter (partial is-potential-counterexample? K impl) (objects K))]
    (incomplete-subcontext K counterexamples (attributes K))))




;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol LaTeX
     "Implements conversion to latex code."
     (latex [this] [this choice] [this choice args] "Returns a string representation of this."))


(extend-type Object
     LaTeX
     (latex
       ([this]
        (.toString this))
       ([this choice]
        (.toString this))
       ([this choice args]
        (.toString this))))

(defn incidence->fcasty-out
  "converts x to X; o to . and ? to ? for burmeister output"
  [in]
  (if (= in  known-true) "x" (if (= in known-false) "." (if (= in unknown) "?" (throw "input not x,o,?")))))



(extend-type Incomplete-Context
  LaTeX
  (latex
    ([this]
     (latex this :fcasty))
    ([this choice]
     (latex this choice {}))
    ([this choice args]
     (case choice
       :plain (with-out-str
                (println (str "\\begin{array}{l||*{" (count (attributes this)) "}{c|}}"))
                (doseq [m (attributes this)]
                  (print (str "& \\text{" m "}")))
                (println "\\\\\\hline\\hline")
                (doseq [g (objects this)]
                  (print (str g))
                  (doseq [m (attributes this)]
                    (print (str "& " (if ((incidence this) [g m]) "\\times" "\\cdot"))))
                  (println "\\\\\\hline"))
                (println (str "\\end{array}")))
       :fcasty (let [attrs (sort (attributes this))
                     objs (sort (objects this))
                     inz (incidence this)]
                 (with-out-str
                   (println "\\begin{cxt}")
                   (if (:name args)
                     (println "  \\cxtName{" (:name args) "}")
                     (println "  \\cxtName{}"))
                   (println "  \\cxtNichtKreuz{}")
                   (doseq [m attrs]
                     (if (< (count (str m)) 4)
                       (println (str "  \\att{" m "}"))
                       (println (str "  \\atr{" m "}")))
                     )
                   (doseq [g objs]
                     (print "  \\obj{")
                     (doseq [m attrs]
                       (print (incidence->fcasty-out (inz [g m]))))
                     (println (str "}{" g "}"))
                     )
                   (println "\\end{cxt}")
                   ))       
       true   (illegal-argument "Unsupported latex format " choice " for contexts.")))))


(extend-type Implication
  LaTeX
  (latex
    ([this]
     (str (if (empty? (premise this))
            "$\\emptyset$"
            (sort (into [] (premise this))))
          " $\\rightarrow$ " (sort (into [] (conclusion this)))))))

nil

