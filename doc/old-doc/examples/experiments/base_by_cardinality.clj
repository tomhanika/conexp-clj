(ns conexp.contrib.experimental.base-by-cardinality
  (:use conexp.main)
  (:require [clojure.core.reducers :as r]))

;;;

(defn canonical-base*
  "Returns stem base of given context.  Uses «background-knowledge» as starting set of
  implications, which will also be subtracted from the final result.  If «predicate» is
  given (a function), computes only those implications from the canonical base whose
  premise satisfy this predicate, i.e. «predicate» returns true on these premises.  Note
  that «predicate» has to satisfy the same conditions as the predicate to
  «next-closed-set-in-family»."
  ([ctx]
     (canonical-base* ctx #{} (constantly true)))
  ([ctx background-knowledge]
     (canonical-base* ctx background-knowledge (constantly true)))
  ([ctx background-knowledge predicate]
     (assert (context? ctx)
             "First argument must be a formal context")
     (assert (fn? predicate)
             "Predicate must be a function")
     (assert (and (set? background-knowledge)
                  (forall [x background-knowledge]
                    (implication? x)))
             "Background knowledge must be a set of implications")
     (let [attributes   (attributes ctx),
           next-closure (fn [implications last]
                          (next-closed-set-in-family predicate
                                                     attributes
                                                     (clop-by-implications implications)
                                                     last)),
           runner       (fn runner [implications candidate]
                          (when candidate
                            (let [conclusions (context-attribute-closure ctx candidate)]
                              (if (not= candidate conclusions)
                                (let [impl  (make-implication candidate conclusions),
                                      impls (conj implications impl)]
                                  (cons impl
                                        (lazy-seq (runner impls (next-closure impls candidate)))))
                                (recur implications (next-closure implications candidate))))))]
       (lazy-seq (runner background-knowledge
                         (close-under-implications background-knowledge #{}))))))

(defn luxenburger-base*
  ;; this is inefficient: for testing direct lower neighbourhood, we do not need to
  ;; traverse all of the list, but just the starting segment of the current candidate
  [context predicate minconf]
  (let [fqis (vec (intents context predicate))]
    (r/fold concat
            (fn [impls B_2]
              (let [lowers (filter (fn [B_1]
                                     (and (proper-subset? B_1 B_2)
                                          (not (exists [B_3 fqis]
                                                 (and (proper-subset? B_1 B_3)
                                                      (proper-subset? B_3 B_2))))))
                                   fqis)]
                (concat impls
                        (doall      ; do actual computation here, to allow for parallelism
                         (filter (fn [impl]
                                   (<= minconf (confidence impl context)))
                                 (map (fn [B_1] (make-implication B_1 B_2)) lowers))))))
            fqis)))


;;;

nil
