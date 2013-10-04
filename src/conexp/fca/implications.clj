;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.implications
  (:use conexp.base
        conexp.fca.contexts)
  (:import [java.util HashMap HashSet]))

(ns-doc "Implications for Formal Concept Analysis.")

;;;

(deftype Implication [premise conclusion]
  Object
  (equals [this other]
    (generic-equals [this other] Implication [premise conclusion]))
  (hashCode [this]
    (hash-combine-hash Implication premise conclusion))
  (toString [this]
    (str "(" premise " ⟶ " conclusion ")")))

(defmulti premise
  "Returns premise of given object."
  {:arglists '([thing])}
  type)

(defmethod premise Implication [^Implication impl]
  (.premise impl))

(defmulti conclusion
  "Returns conclusion of given object."
  {:arglists '([thing])}
  type)

(defmethod conclusion Implication [^Implication impl]
  (.conclusion impl))

(defmethod print-method Implication
  [impl out]
  (.write ^java.io.Writer out
          ^String (str impl)))

(defn implication?
  "Returns true iff thing is an implication."
  [thing]
  (instance? Implication thing))

;;;

(defn make-implication
  "Creates an implication (premise => conclusion \\ premise)."
  [premise conclusion]
  (let [premise (set premise)
        conclusion (set conclusion)]
    (Implication. premise (difference conclusion premise))))

(defmacro impl
  "Convenience interface for creating implications.  Write implications just as

    user=> (impl 1 2 3 ==> 4 5 6)
    (#{1 2 3} ==> #{4 5 6})"
  [& elements]
  (let [[premise conclusion] (split-with (fn [x]
                                           (not= x '==>))
                                         elements)]
    (when (empty? conclusion)
      (warn "«impl» does not contain ==>"))
    `(make-implication (list ~@premise) (list ~@(rest conclusion)))))

;;;

(defn respects?
  "Returns true iff set respects given implication impl."
  [set impl]
  (or (not (subset? (premise impl) set))
      (subset? (conclusion impl) set)))

(defn holds?
  "Returns true iff impl holds in given context ctx."
  [impl ctx]
  (subset? (conclusion impl) (adprime ctx (premise impl))))

(defn- add-immediate-elements
  "Iterating through the sequence of implications, tries to apply as many implications as
  possible.  Uses subset-test to determine whether a given implication can be used to
  extend a given set, i.e. an implication impl can be used to extend a set s if and only
  if

    (subset-test (premise impl) s)

  is true. Note that if (conclusion impl) is already a subset of s, then s is effectively
  not extended."
  [implications initial-set subset-test]
  (loop [conclusions  (transient initial-set),
         impls        implications,
         unused-impls (transient [])]
    (if-let [impl (first impls)]
      (if (subset-test (premise impl) initial-set)
        (recur (reduce conj! conclusions (conclusion impl))
               (rest impls)
               unused-impls)
        (recur conclusions
               (rest impls)
               (conj! unused-impls impl)))
      [(persistent! conclusions)
       (persistent! unused-impls)])))

(defn close-under-implications
  "Computes smallest superset of set being closed under given implications."
  [implications set]
  (assert (set? set))
  (loop [set   set,
         impls implications]
    (let [[new impls] (add-immediate-elements impls set subset?)]
      (if (= new set)
        new
        (recur new impls)))))

(defn clop-by-implications
  "Returns closure operator given by implications."
  [implications]
  (partial close-under-implications implications))

(defn pseudo-close-under-implications
  "Computes smallest superset of set being pseudo-closed under given
  implications."
  [implications set]
  (assert (set? set))
  (loop [set   set,
         impls implications]
    (let [[new impls] (add-immediate-elements impls set proper-subset?)]
      (if (= new set)
        new
        (recur new impls)))))

(defn pseudo-clop-by-implications
  "Returns for a given set of implications the corresponding closure
  operator whose closures are all closed and pseudo-closed sets."
  [implications]
  (partial pseudo-close-under-implications implications))

(defn follows-semantically?
  "Returns true iff implication follows semantically from given
  implications."
  [implication implications]
  (subset? (conclusion implication)
           (close-under-implications implications (premise implication))))

(defalias follows? follows-semantically?)

(defn equivalent-implications?
  "Returns true iff the two seqs of implications are equivalent."
  [impls-1 impls-2]
  (and (forall [impl impls-1] (follows-semantically? impl impls-2))
       (forall [impl impls-2] (follows-semantically? impl impls-1))))

(defn minimal-implication-set?
  "Checks whether given set of implications is minimal, i.e. no
  implication in this set follows from the others."
  [impl-set]
  (let [impl-set (set impl-set)]
    (forall [impl impl-set]
      (not (follows-semantically? impl (disj impl-set impl))))))

(defn sound-implication-set?
  "Checks whether given set of implications is sound, i.e. every
  implication holds in the given context."
  [ctx impl-set]
  (forall [impl impl-set]
    (holds? impl ctx)))

(defn complete-implication-set?
  "Checks wheter given set of implications is complete in context ctx. This is a very costly
  computation."
  [ctx impl-set]
  (and (forall [impl impl-set]
         (and (subset? (premise impl) (attributes ctx))
              (subset? (conclusion impl) (attributes ctx))))
       (forall [A (subsets (attributes ctx))]
         (subset? (adprime ctx A)
                  (close-under-implications impl-set A)))))

(defn irredundant-subset
  "Given a set impls of implications, returns an irredundant subset of impls.  Note that
  this set does not need to be of minimal cardinality."
  [impls]
  (reduce (fn [impls impl]
            (if (follows-semantically? impl impls)
              impls
              (loop [impls     impls,             ; implications to check
                     new-impls (conj impls impl)] ; all implications
                (if-not (seq impls)
                  new-impls
                  (let [next-impl (first impls)]
                    (if (follows-semantically? next-impl (disj new-impls next-impl))
                      (recur (rest impls) (disj new-impls next-impl)) ; first implication entailed by others
                      (recur (rest impls) new-impls)))))))            ; not
          #{}
          impls))

;;; Stem Base

(declare support)

(defn stem-base
  "Returns stem base of given context.  Uses «background-knowledge» as starting set of
  implications, which will also be subtracted from the final result.  If «minimal-support»
  is specified, computes only the part of the canonical base that satisfies the
  corresponding support constraint."
  ([ctx]
     (stem-base ctx #{} 0))
  ([ctx background-knowledge]
     (stem-base ctx background-knowledge 0))
  ([ctx background-knowledge minimal-support]
     (assert (context? ctx)
             "First argument must be a formal context")
     (assert (and (number? minimal-support)
                  (<= 0 minimal-support 1))
             "Minimal support must be a real number between 0 and 1")
     (assert (and (set? background-knowledge)
                  (forall [x background-knowledge]
                    (implication? x)))
             "Background knowledge must be a set of implications")
     (let [attributes   (attributes ctx),
           next-closure (if (zero? minimal-support)
                          (fn [implications last]
                            (next-closed-set attributes
                                             (clop-by-implications implications)
                                             last))
                          (fn [implications last]
                            (next-closed-set-in-family #(<= minimal-support
                                                            (support % ctx))
                                                       attributes
                                                       (clop-by-implications implications)
                                                       last)))]
       (loop [implications background-knowledge,
              last         (close-under-implications background-knowledge #{})]
         (let [conclusion-from-last (context-attribute-closure ctx last),
               implications         (if (not= last conclusion-from-last)
                                      (conj implications
                                            (make-implication last conclusion-from-last))
                                      implications),
               next                 (next-closure implications last)]
           (if next
             (recur implications next)
             (difference implications background-knowledge)))))))

(defalias canonical-base stem-base)

(defn pseudo-intents
  "Returns the pseudo intents of the given context ctx."
  [ctx]
  (map premise (stem-base ctx)))


;;; Proper Premises

(defn proper-conclusion
  "Returns all elements which are implied in context ctx by A but are neither contained in A or
  follow from a strict subsets of A."
  [ctx A]
  (difference (context-attribute-closure ctx A)
              (reduce into
                      A
                      (map #(context-attribute-closure ctx (disj A %))
                           A))))

(defn proper-premise?
  "Returns true iff set A is a subset of the attributes of context ctx
  and is a proper premise in ctx."
  [ctx A]
  (and (subset? A (attributes ctx))
       (not (empty? (proper-conclusion ctx A)))))

(defn- proper-premises-by-hypertrans
  "Returns all proper premises for the attribute «m» in the formal context «ctx».  The set
  «objs» should contain all objects from ctx which are in down-arrow relation to m."
  [ctx m objs]
  (minimal-hypergraph-transversals
   (disj (attributes ctx) m)
   (set-of (difference (attributes ctx) (oprime ctx #{g})) | g objs)))

(defn proper-premises-for-attribute
  "Returns all proper premises for the attribute «m» in the formal context «ctx»."
  [ctx m]
  (proper-premises-by-hypertrans ctx m (set-of g | [g n] (down-arrows ctx) :when (= n m))))

(defn proper-premises
  "Returns the proper premises of the given context ctx as a lazy sequence."
  [ctx]
  (let [down-arrow-map (loop [arrows    (down-arrows ctx),
                              arrow-map (map-by-fn (constantly #{}) (attributes ctx))]
                         (if-let [[g m] (first arrows)]
                           (recur (rest arrows)
                                  (update-in arrow-map [m] conj g))
                           arrow-map))]
    (distinct
     (reduce concat
             (pmap #(apply proper-premises-by-hypertrans ctx %)
                   down-arrow-map)))))

(defn proper-premise-implications
  "Returns all implications based on the proper premises of the
  context ctx."
  [ctx]
  (set-of (make-implication A (context-attribute-closure ctx A))
          [A (proper-premises ctx)]))

;;;

(defn stem-base-from-base
  "For a given set of implications returns its stem-base."
  [implications]
  (let [implications (pmap (fn [impl]
                             (make-implication
                              (premise impl)
                              (close-under-implications implications
                                                        (union (premise impl)
                                                               (conclusion impl)))))
                           implications)]
    (loop [stem-base    #{},
           implications implications,
           all          (set implications)]
      (if (empty? implications)
        stem-base
        (let [A->B         (first implications),
              implications (rest implications),
              all          (disj all A->B)
              A*           (close-under-implications all (premise A->B)),
              A*->B        (make-implication A* (conclusion A->B))]
          (if (not-empty (conclusion A*->B))
            (recur (conj stem-base A*->B)
                   implications
                   (conj all A*->B))
            (recur stem-base
                   implications
                   all)))))))

(defalias canonical-base-from-base stem-base-from-base)


;;; Association Rules

(defn support
  "Computes the support of the set of attributes B in context ctx. If an implications is given,
  returns the support of this implication in the given context."
  [thing ctx]
  (cond
   (set? thing)
   (if (empty? (objects ctx))
     1
     (/ (count (attribute-derivation ctx thing))
        (count (objects ctx)))),
   (implication? thing)
   (recur (premise thing) ctx),
   :else
   (illegal-argument "Cannot determine support of " (print-str thing))))

(defn confidence
  "Computes the confidence of the given implication in the given context."
  [implication context]
  (let [premise-count (count (attribute-derivation context (premise implication)))]
    (if (zero? premise-count)
      1
      (/ (count (attribute-derivation context
                                      (union (premise implication) (conclusion implication))))
         premise-count))))

;;;

(defn- frequent-itemsets
  "Returns all frequent itemsets of context, given minsupp as minimal support."
  ;; UNTESTED!
  [context minsupp]
  (let [mincount (* minsupp (count (objects context)))]
    (all-closed-sets-in-family (fn [intent]
                                 (>= (count (attribute-derivation context intent))
                                     mincount))
                               (attributes context)
                               identity)))

(defn- association-rules
  "Returns all association rules of context with the parameters minsupp as minimal support and
  minconf as minimal confidence. The result returned is a lazy sequence."
  ;; UNTESTED!
  [context minsupp minconf]
  (let [fitemsets (frequent-itemsets context minsupp)]
    (for [A fitemsets,
          B fitemsets,
          :let [impl (make-implication A B)]
          :when (>= (confidence impl context) minconf)]
      impl)))

;;;

(defn frequent-closed-itemsets
  "Computes for context a lazy sequence of all frequent and closed itemsets, given minsupp as
  minimal support."
  [context minsupp]
  (let [mincount (* minsupp (count (objects context)))]
    (all-closed-sets-in-family (fn [intent]
                                 (>= (count (attribute-derivation context intent))
                                     mincount))
                               (attributes context)
                               (partial context-attribute-closure context))))

(defn luxenburger-basis
  "Computes the luxenburger-basis for context with minimal support minsupp and minimal confidence
  minconf. The result returned will be a lazy sequence."
  [context minsupp minconf]
  (for [B_2  (frequent-closed-itemsets context minsupp),
        B_1  (map second (direct-upper-concepts context [(aprime context B_2) B_2])),
        :let [impl (make-implication B_1 B_2)]
        :when (>= (confidence impl context) minconf)]
    impl))

(defalias luxenburger-base luxenburger-basis)

;;;

(defn- cover [base-set candidates A]
  (let [object-covers (minimum-set-covers
                       (difference base-set A)
                       (set-of (difference base-set N) | N candidates))]
    (map (fn [cover]
           (map #(difference base-set %) cover))
         object-covers)))

(defn ryssel-base
  "Returns the implications computed by Ryssels Algorithm, as a lazy sequence."
  [ctx]
  (let [gens        (reduce! (fn [map x]      ;generating elements of attribute extents
                               (let [extent (aprime ctx #{x})]
                                 (assoc! map extent
                                         (conj (get map extent #{}) x))))
                             {}
                             (attributes ctx)),
        all-extents (set (keys gens)),        ;all attribute extents
        irr-extents (set-of (aprime ctx #{m}) ;attribute extents of irreducible attributes
                            | m (attributes (reduce-attributes ctx))),
        empty-prime (adprime ctx #{})]
    (->> (reduce into
                 (for [m (attributes ctx)
                       :when (not= (adprime ctx #{m})
                                   (conj empty-prime m))]
                   #{m})
                 (pmap (fn [A]
                         (let [candidates (set-of U | U (disj irr-extents A),
                                                      :let [U-cap-A (intersection U A)]
                                                      :when (not (exists [V all-extents]
                                                                   (and (proper-subset? V A)
                                                                        (subset? U-cap-A V))))),
                               covers     (cover (objects ctx) candidates A)]
                           (for [X covers]
                             (set-of m | Y X, m (gens Y)))))
                       all-extents))
         distinct
         (map #(make-implication % (adprime ctx %))))))

;;;

nil
