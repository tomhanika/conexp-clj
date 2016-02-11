;;
;; Experiments on computing the canonical base in a parallel manner
;;

(ns conexp.contrib.experimental.parallel-canonical
  (:use conexp.main)
  (:require [clojure.core.reducers :as r]))

;;

(defn canonical-base-from-clop
  "Given a closure operator «clop» on the set «base-set», computes its canonical base,
   optionally using the set «background-knowledge» of implications on «base-set» as
   background knowledge."
  ([base-set clop]
     (canonical-base-from-clop base-set clop #{}))
  ([base-set clop background-knowledge]
     ;; checks
     (assert (coll? base-set)
             "«base-set» must be a collection")
     (assert (fn? clop)
             "Closure operator «clop» must be a function")
     (assert (and (coll? background-knowledge)
                  (forall [x background-knowledge]
                    (implication? x)))
             "Background knowledge must be a collection of implications")
     ;; algorithm
     (let [base-set             (set base-set),
           background-knowledge (set background-knowledge),
           next-closure         (fn [implications last]
                                  (next-closed-set base-set
                                                   (clop-by-implications implications)
                                                   last))]
       (loop [implications background-knowledge,
              last         (close-under-implications background-knowledge #{})]
         (let [conclusion-from-last (clop last),
               implications         (if (not= last conclusion-from-last)
                                      (conj implications
                                            (make-implication last conclusion-from-last))
                                      implications),
               next                 (next-closure implications last)]
           (if next
             (recur implications next)
             (difference implications background-knowledge)))))))

(defn intersect-implicational-theories
  "Given a set «base-set» and collections «implication-sets» of implications, returns the
  canonical base of the intersection of the corresponding closure theories."
  [base-set & implication-sets]
  (let [implication-clops (vec (map clop-by-implications implication-sets)),
        clop              (fn [A]
                            (r/fold (r/monoid intersection (constantly base-set))
                                    (r/map #(% A) implication-clops)))]
    (canonical-base-from-clop base-set clop)))

(defn parallel-canonical-base
  "Computes the canonical base of the context «ctx» in a parallel way, dividing the object
  set in chunks of size at most «n» each"
  [ctx n]
  (apply intersect-implicational-theories
         (attributes ctx)
         (pmap (fn [objs]
                 (canonical-base (make-context objs (attributes ctx) (incidence ctx))))
               (partition-all n (objects ctx)))))

(defn test-parallel-canonical-base [ctx & nums]
  (apply = (time (canonical-base ctx))
         (map #(parallel-canonical-base ctx %)
              nums)))

;;

nil
