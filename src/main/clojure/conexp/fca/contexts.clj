;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.contexts
  "Provides the implementation of formal contexts and functions on them."
  (:require [clojure.core.reducers :as r]
            [conexp.base :refer :all]))

;;;

(defprotocol Context
  (objects [ctx]    "Returns the objects of a context.")
  (attributes [ctx] "Returns the attributes of a context.")
  (incidence [ctx] "Returns a function that, given a pair [a b], returns true if and only
  if a and b are incident in the context ctx."))

(deftype Formal-Context [objects attributes incidence]
  Object
  (equals [this other]
    (and (instance? Formal-Context other)
         (= objects (.objects ^Formal-Context other))
         (= attributes (.attributes ^Formal-Context other))
         (let [other-incidence (.incidence ^Formal-Context other)]
           (forall [g objects, m attributes]
             (<=> (incidence [g m])
                  (other-incidence [g m]))))))
  (hashCode [this]
    (hash-combine-hash Formal-Context objects attributes incidence))
  ;;
  Context
  (objects [this] objects)
  (attributes [this] attributes)
  (incidence [this] incidence))

(defn context?
  "Returns true iff thing is a formal context."
  [thing]
  (instance? Formal-Context thing))

(defn ^String context-to-string
  "Prints contexts in a human readable form. Orderings can be given as
  sequences or as functions. If given as sequence, the corresponding
  elements will be ordered that way, with all remaining elements at
  the end. If given as function, sort the corresponding elements with
  that function. If no ordering is given for objects and attributes,
  sort-by-second is used. "
  ([ctx]
     (context-to-string ctx sort-by-second sort-by-second))
  ([ctx order-on-objects order-on-attributes]
     (let [str         #(if (= % nil) "nil" (str %)),

           sort-things (fn [things order-on-things]
                         (cond
                          (sequential? order-on-things)
                          (let [order-on-things (filter (things ctx) order-on-things)]
                            (concat order-on-things
                                    (difference (things ctx)
                                                (set order-on-things)))),
                          (fn? order-on-things)
                          (vec (sort order-on-things (things ctx))),
                          :otherwise
                          (illegal-argument "Ordering given to context-to-string must "
                                            "be either a sequence or a function."))),
           attributes  (sort-things attributes order-on-attributes),
           objects     (sort-things objects order-on-objects),
           incident?   (incidence ctx),

           max-att     (reduce #(max %1 (count (str %2))) 0 attributes)
           max-obj     (reduce #(max %1 (count (str %2))) 0 objects)]
       (with-str-out
         (ensure-length "" max-obj " ") " |" (for [att attributes]
                                               [(print-str att) " "]) "\n"
         (ensure-length "" max-obj "-") "-+" (for [att attributes]
                                               (ensure-length "" (inc (count (print-str att))) "-")) "\n"
         (for [obj objects]
           [(ensure-length (print-str obj) max-obj)
            " |"
            (for [att attributes]
              [(ensure-length (if (incident? [obj att]) "x" ".")
                              (count (print-str att)))
               " "])
            "\n"])))))

(defn print-context
  "Prints the result of applying context-to-string to the given
  arguments."
  [ctx & args]
  (print (apply context-to-string ctx args)))

(defmethod print-method Formal-Context [ctx out]
  (.write ^java.io.Writer out
          ^String (context-to-string ctx)))

;;;

(defn make-context-nc
  "Context constructor similar to make-context, but does not do any
  safety checking and is therefore faster. Use with care.

  This function is useful if you want to construct new contexts from old ones.  In
  particular, if you want to use the incidence relation if a given context as the
  incidence relation for a new one, you can just pass it to this function without any
  modifications."
  [objects attributes incidence]
  (Formal-Context. (to-set objects) (to-set attributes) incidence))

(defmulti make-context
  "Standard constructor for contexts. Takes a sequence of objects,
  a sequence of attributes and either a set of pairs or function of two arguments being
  true iff its arguments are incident. Note that the object and attribute sequences are
  converted to sets and therefore have to not contain any duplicate elements. If the
  incidence relation is given as a sequence, it is automatically restricted to the
  cartesian product of the object an the attribute set."
  {:arglists '([objects attributes incidence])}
  (fn [& args]
    (vec (map clojure-type args))))

(defmethod make-context [Object Object clojure-coll]
  [objects attributes incidence]
  (when (or (map? objects) (map? attributes))
    (illegal-argument "Objects and attributes should not be given as a map."))
  (let [objs (to-set objects)
        atts (to-set attributes)
        inz  (set-of [g m] [[g m] incidence
                            :when (and (contains? objs g)
                                       (contains? atts m))])]
    (Formal-Context. objs atts inz)))

(defmethod make-context [Object Object clojure-fn]
  [objects attributes incidence]
  (when (or (map? objects) (map? attributes))
    (illegal-argument "Objects and attributes should not be given as a map."))
  (Formal-Context. (to-set objects)
                   (to-set attributes)
                   (fn [[g m]]
                     (incidence g m))))

(defmethod make-context :default [obj att inz]
  (illegal-argument "The arguments " obj ", " att " and " inz " are not valid for a formal context."))


;;; Common Operations in Contexts

(defn incident?
  "Returns true if and only if in context ctx, the object g is
  incident with the attribute m."
  [ctx g m]
  ;; note that the incidence of ctx contains only valid crosses
  ((incidence ctx) [g m]))

(defn incidence-relation
  "Returns the incidence-relation of the given context, as a set of pairs of objects and
  attributes."
  [ctx]
  (let [inz (incidence ctx)]
    (if (set? inz)
      inz
      (set-of [g m] [g (objects ctx)
                     m (attributes ctx)
                     :when ((incidence ctx) [g m])]))))

(defn context-size
  "Returns tuple of number of objects, number of attributes, and fill rate."
  [ctx]
  (let [obj-cnt (count (objects ctx)),
        att-cnt (count (attributes ctx)),
        inz-cnt (count (incidence-relation ctx))]
    [obj-cnt, att-cnt, (if (or (zero? obj-cnt)
                               (zero? att-cnt))
                         Double/NaN
                         (double (/ inz-cnt obj-cnt att-cnt)))]))

(defn rename-objects
  "Rename objects in ctx by given function old-to-new."
  [ctx old-to-new]
  (let [objs (map old-to-new (objects ctx)),
        inz  (set-of [(old-to-new g) m] [[g m] (incidence-relation ctx)])]
    (make-context-nc objs (attributes ctx) inz)))

(defn rename-attributes
  "Rename attributes in ctx by given function old-to-new."
  [ctx old-to-new]
  (let [atts (map old-to-new (attributes ctx)),
        inz  (set-of [g (old-to-new m)] [[g m] (incidence-relation ctx)])]
    (make-context-nc (objects ctx) atts inz)))

(defn make-context-from-matrix
  "Given objects G and attribute M and an incidence matrix constructs
  the corresponding context. G and M may also be numbers where they
  represent (range G) and (range M) respectively."
  [G M bits]
  (assert (forall [x bits] (or (= 1 x) (= 0 x)))
          "All entries given must be either 0 or 1.")
  (let [G (ensure-seq G),
        M (ensure-seq M),
        m (count G),
        n (count M)]
    (assert (= (* m n) (count bits))
            "Number of objects and attributes does not match the number of entries.")
    (make-context-nc G M
                     (set-of [a b] [i (range (count G)),
                                    j (range (count M)),
                                    :when (= 1 (nth bits (+ (* n i) j)))
                                    :let [a (nth G i),
                                          b (nth M j)]]))))

(defn subcontext?
  "Tests whether ctx-1 is a subcontext ctx-2 or not."
  [ctx-1 ctx-2]
  (let [objs-1 (objects ctx-1)
        objs-2 (objects ctx-2)
        atts-1 (attributes ctx-1)
        atts-2 (attributes ctx-2)]
    (and (subset? objs-1 objs-2)
         (subset? atts-1 atts-2)
         (forall [g (objects ctx-1)
                  m (attributes ctx-1)]
           (=> ((incidence ctx-1) [g m])
               ((incidence ctx-2) [g m]))))))

(defn restrict-concept
  "Restricts the given concept to the given subcontext."
  [concept subcontext]
  [(intersection (first concept) (objects subcontext)),
   (intersection (second concept) (attributes subcontext))])

(defn object-derivation
  "Computes set of attributes common to all objects in context."
  [ctx objects]
  (let [inz  (incidence ctx),
        atts (attributes ctx)]
    (set-of m [m atts :when (forall [g objects] (inz [g m]))])))

(defalias oprime object-derivation)

(defn attribute-derivation
  "Computes set of objects common to all attributes in context."
  [ctx attributes]
  (let [inz  (incidence ctx),
        objs (objects ctx)]
    (set-of g [g objs :when (forall [m attributes] (inz [g m]))])))

(defalias aprime attribute-derivation)

(defn concept?
  "Tests whether given pair is a concept in context ctx."
  [ctx [set-of-obj set-of-att]]
  (and (set? set-of-obj)
       (set? set-of-att)
       (subset? set-of-obj (objects ctx))
       (subset? set-of-att (attributes ctx))
       (= set-of-obj (attribute-derivation ctx set-of-att))
       (= set-of-att (object-derivation ctx set-of-obj))))

(defn object-concept
  "Returns the object concept of the given object g in context ctx."
  [ctx g]
  (assert (contains? (objects ctx) g)
          "Cannot make object concept of a non-object.")
  (let [atts (oprime ctx #{g})]
    [(aprime ctx atts) atts]))

(defn attribute-concept
  "Returns the attribute concept of the given attribute m in context
  ctx."
  [ctx m]
  (assert (contains? (attributes ctx) m)
          "Cannot make attribute concept of a non-attribute.")
  (let [objs (aprime ctx #{m})]
    [objs (oprime ctx objs)]))

(defn clarify-objects
  "Clarifies objects in context ctx."
  [ctx]
  (let [prime (memoize (partial object-derivation ctx))
        new-objs (set (distinct-by-key (objects ctx) #(prime #{%})))]
    (make-context-nc new-objs (attributes ctx) (incidence ctx))))

(defn clarify-attributes
  "Clarifies attributes in context ctx."
  [ctx]
  (let [prime (memoize (partial attribute-derivation ctx))
        new-atts (set (distinct-by-key (attributes ctx) #(prime #{%})))]
    (make-context-nc (objects ctx) new-atts (incidence ctx))))

(defn clarify-context
  "Clarifies context ctx."
  [ctx]
  (clarify-objects (clarify-attributes ctx)))

(defn object-clarified?
  "Tests whether given context ctx is object clarified."
  [ctx]
  (let [prime (memoize (partial object-derivation ctx))]
    (not (exists [g (objects ctx)
                  h (objects ctx)]
           (and (not= g h) (= (prime #{g}) (prime #{h})))))))

(defn attribute-clarified?
  "Tests whether given context ctx is attribute clarified."
  [ctx]
  (let [prime (memoize (partial attribute-derivation ctx))]
    (not (exists [m (attributes ctx)
                  n (attributes ctx)]
           (and (not= m n) (= (prime #{m}) (prime #{n})))))))

(defn context-clarified?
  "Tests whether given context ctx is clarified."
  [ctx]
  (and (object-clarified? ctx)
       (attribute-clarified? ctx)))

(defn down-arrows
  "Computes the down arrow relation of ctx."
  [ctx]
  (let [obj   (objects ctx),
        att   (attributes ctx),
        prime (map-by-fn #(object-derivation ctx #{%}) obj)]
    (r/fold union
            (fn [set g]
              (into set
                    (map #(vector g %)
                         (reduce (fn [places h]
                                   (if (proper-subset? (prime g) (prime h))
                                     (intersection places (prime h))
                                     places))
                                 (difference (attributes ctx) (prime g))
                                 (objects ctx)))))
            (vec (objects ctx)))))

(defn up-arrows
  "Computes the up arrow relation of ctx."
  [ctx]
  (let [obj   (objects ctx),
        att   (attributes ctx),
        prime (map-by-fn #(attribute-derivation ctx #{%}) att)]
    (r/fold union
            (fn [set m]
              (into set
                    (map #(vector % m)
                         (reduce (fn [places n]
                                   (if (proper-subset? (prime m) (prime n))
                                     (intersection places (prime n))
                                     places))
                                 (difference (objects ctx) (prime m))
                                 (attributes ctx)))))
            (vec (attributes ctx)))))

(defn up-down-arrows
  "Returns up-down-arrow relation of ctx."
  [ctx]
  (apply intersection (pvalues (up-arrows ctx) (down-arrows ctx))))

(defn reduce-objects
  "Object reduction for ctx."
  [ctx]
  (make-context-nc (set-of g [[g _] (down-arrows ctx)])
                   (attributes ctx)
                   (incidence ctx)))

(defn reduce-attributes
  "Attribute reduction for ctx."
  [ctx]
  (make-context-nc (objects ctx)
                   (set-of m [[_ m] (up-arrows ctx)])
                   (incidence ctx)))

(defn reduce-context
  "Reduces context ctx."
  [ctx]
  (let [uda     (up-down-arrows ctx),
        new-obj (set-of g [[g _] uda]),
        new-att (set-of m [[_ m] uda])]
    (make-context-nc new-obj new-att (incidence ctx))))

(defn object-reduced?
  "Tests whether given context ctx is object-reduced or not."
  [ctx]
  (let [da (down-arrows ctx)]
    (forall [g (objects ctx)]
      (exists [[h _] da]
        (= g h)))))

(defn attribute-reduced?
  "Tests whether given context ctx is attribute-reduced or not."
  [ctx]
  (let [ua (up-arrows ctx)]
    (forall [m (attributes ctx)]
      (exists [[_ n] ua]
        (= m n)))))

(defn context-reduced?
  "Tests whether given context ctx is reduced or not."
  [ctx]
  (and (object-reduced? ctx)
       (attribute-reduced? ctx)))

(defn context-object-closure
  "Computes double prime in context ctx for the given set-of-objects."
  [ctx set-of-objects]
  (attribute-derivation ctx (object-derivation ctx set-of-objects)))

(defalias odprime context-object-closure)

(defn extents
  "Computes a sequence of all extents of «ctx», in a lectic order.  Optionally, one can
  specify a predicate function «pred» that acts as a filter on all extents of «ctx».
  «pred» should specify the same conditions as the predicate function to
  «next-closed-set-in-family»."
  ([ctx]
     (all-closed-sets (objects ctx) (partial context-object-closure ctx)))
  ([ctx pred]
     (all-closed-sets-in-family pred (objects ctx) (partial context-object-closure ctx))))

(defn context-attribute-closure
  "Computes double prime in context ctx for the given set-of-attributes."
  [ctx set-of-attributes]
  (object-derivation ctx (attribute-derivation ctx set-of-attributes)))

(defalias adprime context-attribute-closure)

(defn intents
  "Computes a sequence of all intents of «ctx», in a lectic order.  Optionally, one can
  specify a predicate function «pred» that acts as a filter on all intents of «ctx».
  «pred» should specify the same conditions as the predicate function to
  «next-closed-set-in-family»."
  ([ctx]
     (all-closed-sets (attributes ctx) (partial context-attribute-closure ctx)))
  ([ctx pred]
     (all-closed-sets-in-family pred (attributes ctx) (partial context-attribute-closure ctx))))

(defn intent?
  "Test whether `thing' is an intent of the formal context `ctx.'"
  [ctx thing]
  (assert (context? ctx))
  (and (set? thing)
       (subset? thing (attributes ctx))
       (= thing (adprime ctx thing))))

(defn extent?
  "Test whether `thing' is an extent of the formal context `ctx.'"
  [ctx thing]
  (assert (context? ctx))
  (and (set? thing)
       (subset? thing (objects ctx))
       (= thing (odprime ctx thing))))

(defn- cbo-test
  "Simple implementation of the test used by the «Close by One»
  algorithm."
  [attribute j B D]
  (loop [i 0]
    (if (= i j)
      true
      (if (not= (contains? B (attribute i))
                (contains? D (attribute i)))
        false
        (recur (inc i))))))

(defn concepts
  "Returns a sequence of all concepts of ctx."
  [ctx]
  (let [n             (count (attributes ctx)),
        attribute     (vec (attributes ctx)),
        att-extent    (vec (map #(attribute-derivation ctx #{%}) attribute)),
        generate-from (fn generate-from [A B y]
                        (lazy-seq
                         (cons [A B]
                               (when (not= B (attributes ctx))
                                 (mapcat (fn [j]
                                           (when-not (contains? B (attribute j))
                                             (let [C (intersection A (att-extent j)),
                                                   D (object-derivation ctx C)]
                                               (when (cbo-test attribute j B D)
                                                 (generate-from C D (inc j))))))
                                         (range y n)))))),
        empty-obj     (attribute-derivation ctx #{}),
        empty-att     (object-derivation ctx empty-obj)]
    (generate-from empty-obj empty-att 0)))

;;; Common Operations with Contexts

(defn dual-context
  "Dualizes context ctx, that is (G,M,I) gets (M,G,I^{-1})."
  [ctx]
  (make-context-nc (attributes ctx)
                   (objects ctx)
                   (fn [[m g]] ((incidence ctx) [g m]))))

(defn invert-context
  "Inverts context ctx, that is (G,M,I) gets (G,M,(G x M) \\ I)."
  [ctx]
  (make-context-nc (objects ctx)
                   (attributes ctx)
                   (fn [[g m]]
                     (not ((incidence ctx) [g m])))))

(defn context-union
  "Returns the union of ctx-1 and ctx-2. Note that this union is
  inclusive; use context-disjoint-union if this is not what you want."
  [ctx-1 ctx-2]
  (make-context-nc (union (objects ctx-1) (objects ctx-2))
                   (union (attributes ctx-1) (attributes ctx-2))
                   (fn [[g m]]
                     (or (and (contains? (objects ctx-1) g)
                              (contains? (attributes ctx-1) m)
                              ((incidence ctx-1) [g m]))
                         (and (contains? (objects ctx-2) g)
                              (contains? (attributes ctx-2) m)
                              ((incidence ctx-2) [g m]))))))

(defn context-disjoint-union
  "Returns the disjoint union of ctx-1 and ctx-2."
  [ctx-1 ctx-2]
  (let [new-objs (disjoint-union (objects ctx-1)
                                 (objects ctx-2)),
        new-atts (disjoint-union (attributes ctx-1)
                                 (attributes ctx-2))
        new-inz  (fn [[[g idx-g] [m idx-m]]]
                   (or (and (= idx-g idx-m 0) ((incidence ctx-1) [g m]))
                       (and (= idx-g idx-m 1) ((incidence ctx-2) [g m]))))]
    (make-context-nc new-objs new-atts new-inz)))

(defn context-intersection
  "Returns context intersection of ctx1 and ctx2."
  [ctx1 ctx2]
  (make-context-nc (intersection (objects ctx1) (objects ctx2))
                   (intersection (attributes ctx1) (attributes ctx2))
                   (fn [[g m]]
                     (and ((incidence ctx1) [g m])
                          ((incidence ctx2) [g m])))))

(defn context-composition
  "Returns context composition of ctx-1 and ctx-2, that is

    (G_1,M_1,I_1) o (G_2,M_2,I_2) := (G_1,M_2,I_1 o I_2).
  "
  [ctx-1 ctx-2]
  (make-context-nc (objects ctx-1)
                   (attributes ctx-2)
                   (fn [[g m]]
                     (exists [x (intersection (attributes ctx-1) (objects ctx-2))]
                       (and ((incidence ctx-1) [g x])
                            ((incidence ctx-2) [x m]))))))

(defn context-apposition
  "Returns context apposition of ctx-1 and ctx-2, that is

   (G_1,M_1,I_1) | (G_1,M_2,I_2) := (G_1,M_1 dunion M_2,I_1 dunion I_2).
  "
  [ctx-1 ctx-2]
  (if (not= (objects ctx-1) (objects ctx-2))
    (illegal-argument "Cannot do context apposition, since object sets are not equal."))
  (let [new-atts (disjoint-union (attributes ctx-1) (attributes ctx-2)),
        new-inz  (fn [[g [m i]]]
                   (if (= i 0)
                     ((incidence ctx-1) [g m])
                     ((incidence ctx-2) [g m])))]
    (make-context-nc (objects ctx-1) new-atts new-inz)))

(defn context-subposition
  "Returns context subposition of ctx-1 and ctx-2, that is

     (G_1,M_1,I_1)
    --------------- := (G_1 dunion G_2, M_1, I_1 union I_2).
     (G_2,M_1,I_2)
  "
  [ctx-1 ctx-2]
  (if (not= (attributes ctx-1) (attributes ctx-2))
    (illegal-argument "Cannot do context subposition, since attribute sets are not equal."))
  (let [new-objs (disjoint-union (objects ctx-1) (objects ctx-2))
        new-inz  (fn [[[g i] m]]
                   (if (= i 0)
                     ((incidence ctx-1) [g m])
                     ((incidence ctx-2) [g m])))]
    (make-context-nc new-objs (attributes ctx-1) new-inz)))

(defn context-transitive-closure
  "Transitively closes incidence relation of ctx and returns corresponding context."
  [ctx]
  (make-context-nc (objects ctx) (attributes ctx) (transitive-closure (incidence-relation ctx))))

(defn rand-context
  "Randomly fills context on base-set (or on objects and attributes)
  with crosses and propability fill-rate. If given numbers instead of
  collections uses (range base-set) (and likewise for objects and
  attributes) instead."
  ([base-set fill-rate]
   (rand-context base-set base-set fill-rate))
  ([objects attributes fill-rate]
   (when-not (and (number? fill-rate)
                  (<= 0 fill-rate 1))
     (illegal-argument "Fill-rate must be a number between 0 and 1."))
   (let [objs (to-set objects)
         atts (to-set attributes)]
     (make-context-nc objs atts
                      (set-of [g m] | g objs, m atts, :when (> fill-rate (rand)))))))

(defalias random-context rand-context)

(defn random-contexts
  "Returns a sequence of number contexts, with random fill rate and random
  size between 0 and upper-limit."
  [number upper-limit]
  (repeatedly number #(rand-context (set-of-range (dec (rand upper-limit)))
                                    (set-of-range (dec (rand upper-limit)))
                                    (rand 1.0))))

(defn one-context
  "Returns context full of crosses."
  [base-set]
  (make-context-nc base-set base-set (fn [[_ _]] true)))

(defn null-context
  "Returns context with no crosses."
  [base-set]
  (make-context-nc base-set base-set (fn [[_ _]] false)))

(defn diag-context
  "Returns = on base-set as context."
  [base-set]
  (make-context-nc base-set base-set (fn [[a b]] (= a b))))

(defn adiag-context
  "Returns not= on base-set as context."
  [base-set]
  (make-context-nc base-set base-set (fn [[a b]] (not= a b))))

(defn context-sum
  "Computes the context sum of ctx-1 and ctx-2, that is

    (G_1,M_1,I_1) + (G_2,M_2,I_2) :=
      (G_1 dunion G_2, M_1 dunion M_2, I_1 dunion I_2
       dunion (G_1 x M_2) dunion (G_2 x M_1))

  where all set unions are disjoint set unions."
  [ctx-1 ctx-2]
  (let [new-objs (disjoint-union (objects ctx-1) (objects ctx-2)),
        new-atts (disjoint-union (attributes ctx-1) (attributes ctx-2)),
        new-inz  (fn [[[g i] [m j]]]
                   (cond
                     (= i j 0) ((incidence ctx-1) [g m])
                     (= i j 1) ((incidence ctx-2) [g m])
                     :else true))]
    (make-context-nc new-objs new-atts new-inz)))

(defn context-product
  "Computes the context product of ctx-1 and ctx-2, that is

    (G_1,M_1,I_1) x (G_2,M_2,I_2) :=
      (G_1 x G_2, M_1 x M_2, nabla)

  where

    (g_1,g_2) nabla (m_1,m_2) <=> g_1I_1m_1 or g_2I_2m_2.
  "
  [ctx-1 ctx-2]
  (let [new-objs (cross-product (objects ctx-1) (objects ctx-2))
        new-atts (cross-product (attributes ctx-1) (attributes ctx-2))
        inz-1    (incidence ctx-1)
        inz-2    (incidence ctx-2)
        new-inz  (fn [[[g_1, g_2], [m_1, m_2]]]
                   (or (inz-1 [g_1, m_1])
                       (inz-2 [g_2, m_2])))]
    (make-context-nc new-objs new-atts new-inz)))

(defn context-semiproduct
  "Computes the context semiproduct of ctx-1 and ctx-2, where for
  contexts (G_1,M_1,I_1) and (G_2,M_2,I_2) their semidirect product is
  defined as

    (G_1 x G_2, M_1 dunion M_2, nabla)

  where

    (g_1,g_2) nabla (j,m) <=> g_jI_jm for j in {1,2}.
  "
  [ctx-1 ctx-2]
  (let [new-objs (cross-product (objects ctx-1) (objects ctx-2))
        new-atts (disjoint-union (attributes ctx-1) (attributes ctx-2))
        inzs     [(incidence ctx-1) (incidence ctx-2)]
        new-inz  (fn [[g, [m, idx]]]
                   ((inzs idx) [(g idx) m]))]
    (make-context-nc new-objs new-atts new-inz)))

(defn context-xia-product
  "Computes Xia's product of ctx-1 and ctx-2, where for two contexts
  (G_1,M_1,I_1) and (G_2,M_2,I_2) their Xia product is defined as

    (G_1 x G_2, M_1 x M_2, ~)

  where

    (g_1,g_2) ~ (m_1,m_2) <=> (g_1I_1m_1 <=> g_2I_2m_2).
  "
  [ctx-1 ctx-2]
  (let [G_1 (objects ctx-1)
        G_2 (objects ctx-2)
        M_1 (attributes ctx-1)
        M_2 (attributes ctx-2)
        I_1 (incidence ctx-1)
        I_2 (incidence ctx-2)

        new-objs (cross-product G_1 G_2)
        new-atts (cross-product M_1 M_2)
        new-inz  (fn [[[g_1, g_2], [m_1, m_2]]]
                   (<=> (I_1 [g_1,m_1])
                        (I_2 [g_2,m_2])))]
    (make-context-nc new-objs new-atts new-inz)))

;;; Neighbours in the concept lattice with Lindig's Algorithm

(defn direct-upper-concepts
  "Computes the set of direct upper neighbours of the concept [A B] in
  the concept lattice of ctx. Uses Lindig's Algorithm for that."
  [ctx [A B]]
  (assert (concept? ctx [A B])
          "Given pair must a concept in the given context")
  (loop [objs        (difference (objects ctx) A),
         min-objects (difference (objects ctx) A),
         neighbours  (transient #{})]
    (if (empty? objs)
      (persistent! neighbours)
      (let [g   (first objs),
            M_1 (oprime ctx (conj A g)),
            G_1 (aprime ctx M_1)]
        (if (empty? (intersection min-objects
                                  (difference (disj G_1 g)
                                              A)))
          (recur (rest objs)
                 min-objects
                 (conj! neighbours [G_1 M_1]))
          (recur (rest objs)
                 (disj min-objects g)
                 neighbours))))))

(defn direct-lower-concepts
  "Computes the set of direct upper neighbours of the concept [A B] in
  the concept lattice of ctx. Uses Lindig's Algorithm for that."
  [ctx [A B]]
  (assert (concept? ctx [A B])
          "Given pair must a concept in the given context")
    (loop [atts        (difference (attributes ctx) B),
           min-attrs   (difference (attributes ctx) B),
           neighbours  (transient #{})]
      (if (empty? atts)
        (persistent! neighbours)
        (let [m   (first atts),
              G_1 (aprime ctx (conj B m)),
              M_1 (oprime ctx G_1)]
          (if (empty? (intersection min-attrs
                                    (difference (disj M_1 m)
                                                B)))
            (recur (rest atts)
                   min-attrs
                   (conj! neighbours [G_1 M_1]))
            (recur (rest atts)
                   (disj min-attrs m)
                   neighbours))))))

;;;

nil
