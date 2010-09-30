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
  "Creates an implication (premise => conclusion $\\setminus$ premise)."
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
  (forall [intent (context-intents ctx)]
    (respects? intent impl)))

(defn- add-immediate-elements
  "Adds all elements which follow from implications with premises in
  initial-set."
  [implications initial-set]
  (loop [conclusions  [],
         impls        implications,
         unused-impls []]
    (if (empty? impls)
      [(apply union initial-set conclusions) unused-impls]
      (let [impl (first impls)]
        (if (and (subset? (premise impl) initial-set)
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
  (loop [set   set,
         impls implications]
    (let [[new impls] (add-immediate-elements impls set)]
      (if (= new set)
        new
        (recur new impls)))))

(defn clop-by-implications
  "Returns closure operator given by implications."
  [implications]
  (partial close-under-implications implications))

(defn follows-semantically?
  "Returns true iff implication follows semantically from given
  implications."
  [implication implications]
  (subset? (conclusion implication)
           (close-under-implications implications (premise implication))))

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
  (forall [A (subsets (attributes ctx))]
    (=> (forall [impl impl-set] (respects? A impl))
        (= A (context-attribute-closure ctx A)))))

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

(defn- minimal-intersection-sets
  "Returns for a sequence set-sqn of sets all sets which have
  non-empty intersection with all sets in set-sqn and are minimal with
  that."
  [set-sqn]
  (cond
   (empty? set-sqn) (list #{}),
   (empty? (first set-sqn)) (),
   :else (let [next-set (first set-sqn)]
           (apply partial-min subset?
                  (mapcat (fn [set]
                            (if-not (empty? (intersection set next-set))
                              (list set)
                              (map #(conj set %) next-set)))
                          (minimal-intersection-sets (rest set-sqn)))))))

(defn- proper-premises-for-attribute
  "Returns in context ctx for the attribute m and the objects in objs,
  which must contain all objects g in ctx such that [g m] are in the
  downarrow relation, the proper premises for m."
  [ctx m objs]
  (let [M (attributes ctx),
        I (incidence ctx)]
    (remove #(contains? % m)
            (minimal-intersection-sets
             (for [g objs] (set-of n [n M :when (not (contains? I [g n]))]))))))

(defn proper-premises
  "Returns the proper premises of the given context ctx as a lazy
  sequence."
  [ctx]
  (let [down-arrow-map (loop [arrows (down-arrows ctx),
                              arrow-map {}]
                         (if (empty? arrows)
                           arrow-map
                           (let [[g m] (first arrows)]
                             (recur (rest arrows)
                                    (update-in arrow-map [m] conj g)))))]
    (distinct (mapcat #(proper-premises-for-attribute ctx % (get down-arrow-map %))
                      (attributes ctx)))))

(defn proper-premise-implications
  "Returns all implications based on the proper premises of the
  context ctx."
  [ctx]
  (set-of (make-implication A (A-dot ctx A))
          [A (proper-premises ctx)]))

;;;

nil
