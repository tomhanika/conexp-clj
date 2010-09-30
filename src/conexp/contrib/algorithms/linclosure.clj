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

;;;

nil