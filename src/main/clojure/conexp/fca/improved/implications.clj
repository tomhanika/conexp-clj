(ns conexp.fca.improved.implications
  (:require [conexp.fca.implications :as impl]
            [conexp.fca.contexts :as contexts]
            [conexp.fca.closure-systems :as cs]
            [conexp.base :refer :all])
  (:import [org.fca NextClosure NextClosure$Implication]
           [java.util BitSet ArrayList List]
           [java.util.function Function Predicate]))

(defn- to-bitset [coll mapping]
  (let [bs (BitSet. (count mapping))]
    (doseq [x coll]
      (.set bs (int (get mapping x))))
    bs))

(defn- from-bitset [bs inverse-mapping]
  (let [res (transient #{})]
    (loop [i (.nextSetBit bs 0)]
      (if (>= i 0)
        (do
          (conj! res (get inverse-mapping i))
          (recur (.nextSetBit bs (inc i))))
        (persistent! res)))))

(defn fast-canonical-base
  ([ctx] (fast-canonical-base ctx #{} (constantly true)))
  ([ctx background-knowledge] (fast-canonical-base ctx background-knowledge (constantly true)))
  ([ctx background-knowledge predicate]
   (let [base (contexts/attributes ctx)
         n (count base)
         mapping (zipmap base (range n))
         inv-mapping (vec base)

         ;; Initial Implication List (Java Objects)
         java-impls (ArrayList.)
         _ (doseq [imp background-knowledge]
             (.add java-impls (NextClosure$Implication.
                                (to-bitset (impl/premise imp) mapping)
                                (to-bitset (impl/conclusion imp) mapping))))

         clop #(contexts/context-attribute-closure ctx %)

         start-bs (NextClosure/computeClosure (BitSet. n) java-impls)
         start-set (from-bitset start-bs inv-mapping)

         ;; We pass a NEW copy of the implications list to each recursive step to ensure safety.
         runner (fn runner [current-impls candidate]
                  (when candidate
                    (let [conclusions (clop candidate)]
                      (if (not= candidate conclusions)
                        ;; Pseudo-intent
                        (let [impl (impl/make-implication candidate conclusions)

                              ;; Create NEW list for next step
                              new-impls (ArrayList. current-impls)
                              premise-bs (to-bitset (impl/premise impl) mapping)
                              concl-bs (to-bitset (impl/conclusion impl) mapping)
                              _ (.add new-impls (NextClosure$Implication. premise-bs concl-bs))

                              curr-bs (to-bitset candidate mapping)
                              next-bs (NextClosure/nextClosedSet n new-impls curr-bs)
                              next-candidate (when next-bs (from-bitset next-bs inv-mapping))]

                          (cons impl (lazy-seq (runner new-impls next-candidate))))

                        ;; Concept intent
                        (let [curr-bs (to-bitset candidate mapping)
                              next-bs (NextClosure/nextClosedSet n current-impls curr-bs)
                              next-candidate (when next-bs (from-bitset next-bs inv-mapping))]
                          (recur current-impls next-candidate))))))]

     (lazy-seq (runner java-impls start-set)))))
