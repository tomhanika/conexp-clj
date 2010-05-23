;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms.titanic
  (:use conexp.main))

(ns-doc
 "Implements the TITANIC algorithm. Note that this implementation is
 not tune for speed but for flexibility.")

;;; Pragmatics

(defmacro- set-val!
  "Sets (via set!) the value for key in map to val."
  [map key val]
  `(set! ~map (assoc ~map ~key ~val)))


;;; TITANIC Implementation

(defvar- closure nil
  "Holds the closures of sets. To be rebound.")
(defvar- minimum nil
  "Computes the minimum of weights in a given sequence of weights. To
  be rebound.")
(defvar- set-weight nil
  "Holds the weights of sets. To be rebound.")

(defn- subset-weight
  "Returns the minimum of the weights of all proper subsets of X."
  [max-weight X]
  (minimum (conj (map #(set-weight (disj X %)) X)
                 max-weight)))

(defn- titanic-closure
  "Computes the closure of X by the weights of its subsets."
  [X base-set keys candidates]
  (loop [Y (apply union X (map #(closure (disj X %)) X)),
         elements (difference base-set Y)]
    (if-not (empty? elements)
      (let [m   (first elements),
            X+m (conj X m),
            s   (or (set-weight X+m)
                    (let [weight (minimum (doall (for [K keys, ;bottleneck!
                                                       :when (subset? K X+m)]
                                                   (set-weight K))))]
                      (set-val! set-weight X+m weight)
                      weight))]
        (recur (if (= s (set-weight X))
                 (conj Y m)
                 Y)
               (rest elements)))
      Y)))

(defn- titanic-generate
  "Generates the next candidate set from the given key-set and sets
  subset-weight accordingly."
  [key-set]
  (set-of both [A key-set,
                B key-set,
                :let [both (union A B)]
                :when (and (= (inc (count A)) (count both)) ;one more
                           (forall [x both]                 ;both is candidate
                             (contains? key-set (disj both x))))]))

(defn titanic
  "Implements the titanic algorithm. weigh must return for a
  collection of sets a hash-map mapping every set to its weight, with
  max-weight being the maximal weight possbile. weight-minimum is a
  function of one argument being a sequence of weights and returning
  the minimal of those."
  [base-set weigh max-weight weight-minimum]
  (binding [closure       {},
            set-weight    (weigh [#{}]),
            subset-weight (memoize (partial subset-weight max-weight)),
            minimum       weight-minimum]

    (loop [key-set    #{#{}},
           candidates (set-of #{m} [m base-set]),
           keys       [#{}]]
      (set! set-weight (into set-weight (weigh candidates)))
      (doseq [X key-set]
        (set-val! closure X (titanic-closure X base-set keys candidates)))
      (let [next-key-set (set-of X [X candidates,
                                    :when (not= (subset-weight X)
                                                (set-weight X))])]
        (if-not (empty? next-key-set)
          (recur next-key-set
                 (titanic-generate next-key-set)
                 (concat keys next-key-set))
          (distinct (map closure keys)))))))

;;;

(defn- supports
  "Returns a function of one argument being a collection of sets of attributes,
  returning a hash-map from those sets to their supports in the given
  context, if they have at least minsupp elements. Otherwise they are
  associated with -1."
  [context minsupp]
  (let [num-of-objects (count (objects context)),
        minnum (* minsupp num-of-objects)]
    (fn [coll]
      (into {} (for [atts coll]
                 [atts (let [num (count (attribute-derivation context atts))]
                         (if (< num minnum)
                           -1
                           (/ num num-of-objects)))])))))

(defn titanic-context-intents
  "Computes the intents of the given context via TITANIC."
  [context]
  (titanic (attributes context)
           (supports context 0)
           1.0
           #(apply min %)))

(defn titanic-iceberg-intent-set
  "Computes the iceberg intent set for given context and minimal
  support minsupp via TITANIC."
  [context minsupp]
  (let [intents (titanic (attributes context)
                         (supports context minsupp)
                         1.0
                         #(apply min %))]
    (if (<= (* (count (objects context)) minsupp)
            (count (attribute-derivation context (attributes context))))
      intents
      (remove #{(attributes context)} intents))))

;;;

nil
