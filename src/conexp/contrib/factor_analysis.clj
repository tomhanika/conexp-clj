;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.factor-analysis
  (:use conexp.main))

(ns-doc "Implements factorization algorithms for contexts.")

;;;

(defmulti factorize-context
  "Factorize context by given method."
  {:arglists '([method context & args])}
  (fn [method context & _] method))

;;; Boolean

(defn clear-factors
  "From the given factors remove all which are not needed."
  [factors]
  ;; WRITE ME!
  factors)

(defmethod factorize-context :boolean-full
  [_ context]
  (let [F (intersection (set-of [g-prime-prime, g-prime]
                                [g (objects context)
                                 :let [g-prime (object-derivation context #{g}),
                                       g-prime-prime (attribute-derivation context g-prime)]])
                        (set-of [m-prime, m-prime-prime]
                                [m (attributes context)
                                 :let [m-prime (attribute-derivation context #{m}),
                                       m-prime-prime (object-derivation context m-prime)]])),
        S (difference (set (concepts context)) F),
        U (difference (incidence context)
                      (set-of [i j] [[C D] F, i C, j D]))]
    (loop [U U,
           S S,
           F F]
      (if-not (empty? U)
        (let [[C D] (apply max-key
                           (fn [[X Y]]
                             (count (intersection U (cross-product X Y))))
                           S)]
          (recur (difference U (cross-product C D))
                 (disj S [C D])
                 (conj F [C D])))
        (clear-factors F)))))

(defn- oplus-count
  "Implements $|D\\oplus j|$. D must be subset of the attributes of
  context, as must #{j}."
  [context U D j]
  (let [D+j (conj D j),
        D+j-prime (attribute-derivation context D+j),
        D+j-prime-prime (object-derivation context D+j-prime)]
    (count (intersection U (cross-product D+j-prime D+j-prime-prime)))))

(defmethod factorize-context :boolean-adaptive
  [_ context]
  (let [M (attributes context)]
    (loop [U (incidence context),
           F #{}]
      (if-not (empty? U)
        (let [D (loop [D #{},
                       V 0]
                  (let [j-s     (difference M D),
                        max-j   (apply max-key (partial oplus-count context U D) j-s),
                        max-val (oplus-count context U D max-j)]
                    (if (< V max-val)
                      (recur (context-attribute-closure context (conj D max-j))
                             max-val)
                      D))),
              C (attribute-derivation context D)]
          (recur (difference U (cross-product C D))
                 (conj F [C D])))
        (clear-factors F)))))

;;; Many Valued (with t-norms, TODO)

;;;

nil
