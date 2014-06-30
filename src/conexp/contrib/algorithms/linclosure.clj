;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms.downing-gallier
  (:use [conexp.base :only (not-yet-implemented, def-)]
        [conexp.fca.implications :only (premise conclusion)]))

;;;

(defn implication-graph [base-set implications]
  (let [implications     (vec implications),
        where-in-premise (reduce (fn [map [i impl]]
                                   (reduce (fn [map m]
                                             (update-in map [m] conj i))
                                           map
                                           (premise impl)))
                                 {}
                                 (map vector (range) implications))
        numargs          (into {} (map vector
                                       (range)
                                       (map (comp count premise) implications)))]
    [implications where-in-premise numargs]))

(defn close-with-network [base-set implications input-set]
  (let [[implications in-premise numargs] (implication-graph base-set implications),
        numargs                           (reduce (fn [numargs i]
                                                    (update-in numargs [i] dec))
                                                  numargs
                                                  (mapcat in-premise input-set))
        empty-premise-implications        (map first
                                               (filter (fn [[i c]] (zero? c))
                                                       numargs))]
    (loop [queue   (into (clojure.lang.PersistentQueue/EMPTY)
                         empty-premise-implications),
           numargs numargs
           result  input-set]
      (if (empty? queue)
        result
        (let [idx             (first queue),
              _ (println idx)
              new             (difference (conclusion (implications idx)) result)
              [numargs queue] (reduce (fn [[numargs queue] i]
                                        (let [numargs (update-in numargs [i] dec)]
                                          [numargs (if (pos? (get numargs i))
                                                     queue
                                                     (conj queue i))]))
                                      [numargs (pop queue)]
                                      (mapcat in-premise new))
              _ (println numargs)]
          (recur queue numargs (into result new)))))))

;;;

(do

  (def- testing-implications #{(make-implication '#{p_3 p_4} '#{p_5})
                               (make-implication '#{p_1} '#{p_2})
                               (make-implication '#{p_2} '#{p_1})
                               (make-implication '#{p_3} '#{p_4})
                               (make-implication '#{}    '#{p_3})})

  (def- network (implication-graph '[p_1 p_2 p_3 p_4 p_5]
                                   testing-implications))

  nil)

;;;

nil
