;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms.linclosure
  (:use [conexp.fca.implications :only (premise conclusion)])
  (:import [java.util HashMap HashSet]))

;;;

(defn close-under-implications
  "Computes smallest superset of start being closed under given implications."
  [implications start]
  ;; this is LinClosure
  (let [^HashMap counts (HashMap.),
        ^HashMap list   (HashMap.),
        ^HashSet update (HashSet. ^java.util.Collection start),
        ^HashSet newdep (HashSet. ^java.util.Collection start)]
    (doseq [impl implications,
            :let [impl-count (count (premise impl))]]
      (.put counts impl impl-count)
      (if (zero? impl-count)
        (do (.addAll update (conclusion impl))
            (.addAll newdep (conclusion impl)))
        (doseq [a (premise impl)]
          (.put list a (conj (.get list a) impl)))))
    (while (not (.isEmpty update))
      (let [a (first update)]
        (.remove update a)
        (doseq [impl (.get list a)
                :let [impl-count (.get counts impl)]]
          (.put counts impl (dec impl-count))
          (when (zero? (dec impl-count))
            (doseq [x (conclusion impl)
                    :when (not (.contains newdep x))]
              (.add newdep x)
              (.add update x))))))
    (set newdep)))

(defn close-under-implications*  ;; this is LinClosure
  "Computes smallest superset of start being closed under given implications."
  [implications start]
  (let [[counts list newdep] (loop [implications implications,
                                    counts       {},
                                    list         {},
                                    newdep       (set start)]
                               (if-let [impl (first implications)]
                                 (let [impl-count (count (premise impl))]
                                   (recur (rest implications)
                                          (if (zero? impl-count)
                                            counts
                                            (assoc counts impl impl-count))
                                          (if (zero? impl-count)
                                            list
                                            (reduce (fn [list a]
                                                      (assoc list a (conj (get list a) impl)))
                                                    list
                                                    (premise impl)))
                                          (if (zero? impl-count)
                                            (into newdep (conclusion impl))
                                            newdep)))
                                 [counts list newdep]))]
    (loop [counts counts,
           update (seq newdep),
           newdep newdep]
      (if (empty? update)
        newdep
        (let [a      (first update),
              update (rest update),
              counts (reduce (fn [counts impl]
                               (assoc counts impl (dec (get counts impl))))
                             counts
                             (get list a)),
              newdep (reduce (fn [newdep impl]
                               (if (zero? (counts impl))
                                 (into newdep (conclusion impl))
                                 newdep))
                             newdep
                             (get list a))]
          (recur counts update newdep))))))

;;;

(comment

  (use '[clojure.contrib.def :only (defvar-)])
  (require '[conexp.main :as cm])

  (let [subsets-10 (cm/subsets #{1 2 3 4 5 6 7 8 9 10})]
    (defvar- impls (cm/set-of (cm/make-implication A B)
                              [A subsets-10
                               B subsets-10])))

  (time (close-under-implications impls #{}))
  (time (close-under-implications* impls #{}))
  (time (cm/close-under-implications impls #{}))

  nil)

;;;

nil
