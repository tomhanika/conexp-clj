;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms.titanic
  (:use conexp.main))

(ns-doc "Implements the TITANIC algorithm and some common weights.")

;;; Pragmatics

(defn- minimum
  "Returns a random minimal element wrt <= in seq-of-elements."
  [<= seq-of-elements]
  (first (apply partial-min <= seq-of-elements)))

(defmacro set-val!
  "Sets (via set!) the value for key in map to val."
  [map key val]
  `(set! ~map (assoc ~map ~key ~val)))


;;; TITANIC Implementation

(defvar- closure nil
  "Holds the closures of sets. To be rebound.")
(defvar- set-weight nil
  "Holds the weights of sets. To be rebound.")
(defvar- subset-weight nil
  "Holds the minimum of the weights of the proper subsets of a given
  set. To be rebound.")
(defvar- max-weight nil
  "The maximal weight possible. To be rebound.")

(defn- titanic-closure
  ""
  [X base-set key-set candidates]
  (let [Y (apply union X (map #(closure (disj X %)) X))]
    (loop [Y Y,
           elements (difference base-set Y)]
      (if-not (empty? elements)
        (let [m   (first elements),
              X+m (conj X m),
              s   (if (contains? candidates X+m)
                    (set-weight X+m)
                    (minimum (for [K key-set,
                                   :when (subset? K X+m)]
                               (set-weight K))))]
          (recur (if (= s (set-weight X))
                   (conj Y m)
                   Y)
                 (rest elements)))
        Y))))

(defn- titanic-generate
  "Generates the next candidate set from the given key-set and sets
  subset-weight accordingly."
  [key-set]
  (let [C (set-of both [A key-set,
                        B key-set,
                        :let [both (union A B)]
                        :when (and (= (inc (count A)) (count both)) ;one more
                                   (forall [x both]                 ;both is candidate
                                     (contains? key-set (disj both x))))])]
    (doseq [X C]
      (set-val! subset-weight X
                (minimum (conj max-weight
                               (map #(set-weight (disj X %)) X)))))
    C))

(defn titanic
  "Implements the titanic algorithm."
  ;; weigh returns a map of sets to weights
  ;; <=-weight must be an infimum semilattice
  [base-set weigh [weights <=-weight]]
  (let [max-weight-values (apply partial-max <=-weight weights)]
    (when (not= 1 (count max-weight-values))
      (illegal-argument "Given partial order for weights does not have a greatest element."))

    (binding [closure       {},
              set-weight    {},
              subset-weight {},
              max-weight    (first max-weight-values),
              minimum       (partial minimum <=-weight)]

      (set! set-weight (weigh [#{}]))
      (let [empty-weight (set-weight #{})]
        (doseq [m base-set]
          (set-val! subset-weight #{m} empty-weight)))
      (loop [key-set    [#{}],
             candidates (set-of #{m} [m base-set]),
             key-sets   [key-set]]
        (set! set-weight (into set-weight (weigh candidates)))
        (doseq [X key-set]
          (set-val! closure X (titanic-closure X base-set key-set candidates)))
        (let [next-key-set (set-of X [X candidates,
                                      :when (not= (subset-weight X)
                                            (set-weight X))])]
          (if-not (empty? next-key-set)
            (recur next-key-set
                   (titanic-generate key-set)
                   (conj key-sets key-set))
            (distinct (mapcat #(map closure %) key-sets))))))))


;;; Common Weights

(defn supports
  "Returns a function of one argument being a collection of sets of attributes,
  returning a hash-map from those sets to their supports in the given
  context."
  [context]
  (fn [coll]
    (into {} (for [atts coll]
               [atts (count (context-attribute-closure context atts))]))))

;; iceberg intent set

;;;

nil
