;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms.linclosure
  (:use [conexp.base :only (difference)]
        [conexp.fca.implications :only (premise conclusion make-implication)]))

;;;

(defn close-under-implications  ;; this is LinClosure
  "Computes smallest superset of start being closed under given implications."
  [implications start]
  (let [[counts list newdep] (loop [implications implications,
                                    counts       (transient {}),
                                    list         (transient {}),
                                    newdep       (set start)]
                               (if-let [impl (first implications)]
                                 (let [impl-count (count (premise impl))]
                                   (if (zero? impl-count)
                                     (recur (rest implications)
                                            counts
                                            list
                                            (into newdep (conclusion impl)))
                                     (recur (rest implications)
                                            (assoc! counts impl impl-count)
                                            (reduce (fn [list a]
                                                      (assoc! list a (conj (get list a) impl)))
                                                    list
                                                    (premise impl))
                                            newdep)))
                                 [counts list newdep]))]
    (loop [counts (persistent! counts)
           update newdep,
           newdep newdep]
      (if (empty? update)
        newdep
        (let [a (first update),
              [counts newdep update]
                (reduce (fn [[counts newdep update] impl]
                          (let [cnt (dec (get counts impl))]
                            (if (zero? cnt)
                              [(assoc counts impl cnt)
                               (into newdep (conclusion impl))
                               (into update (difference (conclusion impl) newdep))]
                              [(assoc counts impl cnt)
                               newdep
                               update])))
                        [counts newdep (disj update a)]
                        (get list a))]
          (recur counts update newdep))))))

(defn stem-base-from-base
  "For a given set of implications returns its stem-base."
  [implications]
  (loop [stem-base    #{},
         implications (pmap #(make-implication (premise %)
                                               (close-under-implications implications
                                                                         (into (premise %) (conclusion %))))
                            implications),
         all          (vec implications)]
    (if (empty? implications)
      stem-base
      (let [A->B         (first implications),
            implications (rest implications),
            all          (subvec all 1)
            A*           (close-under-implications all (premise A->B)),
            A*->B        (make-implication A* (conclusion A->B))]
        (if (not-empty (conclusion A*->B))
          (recur (conj stem-base A*->B)
                 implications
                 (conj all A*->B))
          (recur stem-base
                 implications
                 all))))))

;;;

(comment

  (require '[conexp.main :as cm])

  (let [subsets-10 (cm/subsets #{1 2 3 4 5 6 7 8 9 10})]
    (cm/defvar- impls (cm/set-of (cm/make-implication A B)
                                 [A subsets-10
                                  B subsets-10])))

  (time (close-under-implications impls #{}))
  (time (cm/close-under-implications impls #{}))

  nil)

;;;

nil
