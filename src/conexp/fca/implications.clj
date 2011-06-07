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

(ns-doc "Implications for Formal Concept Analysis")

;;;

(deftype Implication [premise conclusion]
  Object
  (equals [this other]
    (generic-equals [this other] Implication [premise conclusion]))
  (hashCode [this]
    (hash-combine-hash Implication premise conclusion)))

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
          ^String (str "(" (premise impl) "  ==>  " (conclusion impl) ")")))

;;;

(defn make-implication
  "Creates an implication (premise => conclusion \\ premise)."
  [premise conclusion]
  (let [premise (set premise)
        conclusion (set conclusion)]
    (Implication. premise (difference conclusion premise))))

;;;

(defn respects?
  "Returns true iff set respects given implication impl."
  [set impl]
  (or (not (subset? (premise impl) set))
      (subset? (conclusion impl) set)))

(defn holds?
  "Returns true iff impl holds in given context ctx."
  [impl ctx]
  (forall [intent (intents ctx)]
    (respects? intent impl)))

(defn- add-immediate-elements
  "Adds all elements which follow from implications with premises in
  initial-set. Uses subset-test to dertermine whether a given
  implication can be used to extend a given set, i.e. an implication
  impl can be used to extend a set s if and only if

    (and (subset-test (premise impl) s)
         (not (subset? (conclusion impl) s)))

  is true."
  [implications initial-set subset-test]
  (loop [conclusions  [],
         impls        implications,
         unused-impls []]
    (if (empty? impls)
      [(apply union initial-set conclusions) unused-impls]
      (let [impl (first impls)]
        (if (and (subset-test (premise impl) initial-set)
                 (not (subset? (conclusion impl) initial-set)))
          (recur (conj conclusions (conclusion impl))
                 (rest impls)
                 unused-impls)
          (recur conclusions
                 (rest impls)
                 (conj unused-impls impl)))))))

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
  "Checks wheter given set of implications is complete in context
  ctx."
  [ctx impl-set]
  (and (forall [impl impl-set]
         (and (subset? (premise impl) (attributes ctx))
              (subset? (conclusion impl) (attributes ctx))))
       (forall [A (subsets (attributes ctx))]
         (=> (forall [impl impl-set] (respects? A impl))
             (= A (context-attribute-closure ctx A))))))

;; Stem Base

(defn stem-base
  "Returns stem base of given context. Uses background-knowledge as
  starting set of implications, which will also be subtracted from the
  final result."
  ([ctx]
     (stem-base ctx #{}))
  ([ctx background-knowledge]
     (loop [implications background-knowledge,
            last         #{}]
       (let [conclusion-from-last (context-attribute-closure ctx last),
             implications         (if (not= last conclusion-from-last)
                                    (conj implications
                                          (make-implication last conclusion-from-last))
                                    implications),
             next                 (next-closed-set (attributes ctx)
                                                   (clop-by-implications implications)
                                                   last)]
         (if next
           (recur implications next)
           (difference implications background-knowledge))))))

(defn pseudo-intents
  "Returns the pseudo intents of the given context ctx."
  [ctx]
  (map premise (stem-base ctx)))


;;; Proper Premises

(defn- A-dot
  "Returns A-dot as in the definition of proper premises."
  [ctx A]
  (difference (context-attribute-closure ctx A)
              (reduce union
                      A
                      (map #(context-attribute-closure ctx (disj A %))
                           A))))

(defn proper-premise?
  "Returns true iff set A is a subset of the attributes of context ctx
  and is a proper premise in ctx."
  [ctx A]
  (and (subset? A (attributes ctx))
       (not (empty? (A-dot ctx A)))))

(defn- intersection-set?
  "Tests whether set has non-empty intersection with every set in sets."
  [set sets]
  (boolean
   (forall [other-set sets]
     (exists [x set]
       (contains? other-set x)))))

(defn- minimal-intersection-sets
  "Returns for a sequence set-sqn of sets all sets which have
  non-empty intersection with all sets in set-sqn and are minimal with
  this property."
  [base-set set-sqn]
  (let [cards    (map-by-fn (fn [x]
                              (count (set-of X | X set-sqn :when (contains? X x))))
                            base-set),
        elements (sort (fn [x y]
                         (>= (cards x) (cards y)))
                       base-set),
        result   (atom []),
        search   (fn search [rest-sets current rest-elements]
                   (cond
                    (exists [x current]
                      (intersection-set? (disj current x) set-sqn))
                    nil,
                    (intersection-set? current set-sqn)
                    (swap! result conj current),
                    :else
                    (let [next-elements (remove (fn [x]
                                                  (not (exists [set rest-sets]
                                                         (contains? set x))))
                                                rest-elements)]
                      (doseq [x next-elements]
                        (search (remove #(contains? % x) rest-sets)
                                (conj current x)
                                (rest next-elements))))))]
    (search set-sqn #{} elements)
    (vec (distinct @result))))

(defn- proper-premises-for-attribute
  "Returns in context ctx for the attribute m and the objects in objs,
  which must contain all objects g in ctx such that [g m] are in the
  downarrow relation, the proper premises for m."
  [ctx m objs]
  (let [M (attributes ctx)]
    (remove #(contains? % m)
            (minimal-intersection-sets
             (attributes ctx)
             (set-of (difference M (oprime ctx #{g})) | g objs)))))

(defn proper-premises
  "Returns the proper premises of the given context ctx as a lazy
  sequence."
  [ctx]
  (let [down-arrow-map (loop [arrows    (down-arrows ctx),
                              arrow-map {}]
                         (if-let [[g m] (first arrows)]
                           (recur (rest arrows)
                                  (update-in arrow-map [m] conj g))
                           arrow-map))]
    (distinct (mapcat #(proper-premises-for-attribute ctx % (get down-arrow-map %))
                      (attributes ctx)))))

(defn proper-premise-implications
  "Returns all implications based on the proper premises of the
  context ctx."
  [ctx]
  (set-of (make-implication A (A-dot ctx A))
          [A (proper-premises ctx)]))

;;;

(defn stem-base-from-base
  "For a given set of implications returns its stem-base."
  [implications]
  (loop [stem-base    #{},
         implications (map #(make-implication (premise %)
                                              (close-under-implications implications
                                                                        (union (premise %)
                                                                               (conclusion %))))
                           implications)]
    (if (empty? implications)
      stem-base
      (let [A->B         (first implications),
            implications (rest implications),
            A*           (close-under-implications (into stem-base implications)
                                                   (premise A->B)),
            new-impl     (make-implication A* (conclusion A->B))]
        (recur (if (not-empty (conclusion new-impl))
                 (conj stem-base
                       (make-implication A* (conclusion A->B)))
                 stem-base)
               implications)))))

;;;

nil
