;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.factor-analysis
  (:use conexp.main)
  (:import [java.util HashMap]))

(ns-doc "Implements factorization algorithms for contexts.")

;;;

(defmulti factorize-context
  "Factorize context by given method."
  {:arglists '([method context & args])}
  (fn [method context & _] method))

;;; Boolean

(defn- clear-factors
  "From the given factors remove all which are not needed."
  [factors]
  (let [^HashMap marked (HashMap.)]
    (doseq [factor-tail (tails factors),
            :let [[A_i B_i] (first factor-tail)]
            [A_j B_j] (rest factor-tail)]
      (let [S (intersection (cross-product A_i B_i)
                            (cross-product A_j B_j))]
        (when-not (empty? S)
          (.put marked [A_i B_i]
                (into (.get marked [A_i B_i])
                      S))
          (.put marked [A_j B_j]
                (into (.get marked [A_j B_j])
                      S)))))
    (filter (fn [[A B]]
              (not= (cross-product A B)
                    (set (.get marked [A B]))))
            factors)))

(defn- object-concepts
  "Returns the object concepts of context."
  [context]
  (set-of [g-prime-prime, g-prime]
          [g (objects context)
           :let [g-prime (object-derivation context #{g}),
                 g-prime-prime (attribute-derivation context g-prime)]]))

(defn- attribute-concepts
  "Returns the attribute concepts of context."
  [context]
  (set-of [m-prime, m-prime-prime]
          [m (attributes context)
           :let [m-prime (attribute-derivation context #{m}),
                 m-prime-prime (object-derivation context m-prime)]]))

(defmethod factorize-context :boolean-full
  [_ context]
  (let [F (intersection (object-concepts context)
                        (attribute-concepts context)),
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

;;; Many Valued (with t-norms)

(defmulti t-norm
  "Returns the t-norm and it's residuum corresponding to the name given."
  {:arglists '[(t-norm-name)]}
  identity)

(defmethod t-norm :default
  [norm]
  (illegal-argument "Norm " (str norm) " is not known."))

(defmethod t-norm :łukasiewicz
  [_]
  [(fn [x y] (max 0 (+ x y -1))),
   (fn [x y] (min 1 (+ 1 (- x) y)))])

(defmethod t-norm :lukasiewicz
  [_]
  (t-norm :łukasiewicz))

(defmethod t-norm :gödel
  [_]
  [(fn [x y] (min x y)),
   (fn [x y] y)])

(defmethod t-norm :goedel
  [_]
  (t-norm :gödel))

(defmethod t-norm :product
  [_]
  [(fn [x y] (* x y)),
   (fn [x y] (/ y x))])

;;;

(defmacro- define-fuzzy-operator
  "Defines a fuzzy operator, which throws an
  UnsupportedOperationException when called. The operator is meant to be
  rebound."
  [name arity]
  `(defn ~name ~(vec (map (fn [_] (gensym)) (range arity)))
     (unsupported-operation "The operator " (str '~name) " is not rebound.")))

(define-fuzzy-operator f-star 2)
(define-fuzzy-operator f-impl 2)
(define-fuzzy-operator f-and 2)
(define-fuzzy-operator f-or 2)
(define-fuzzy-operator f-neg 1)

(defmacro with-fuzzy-logic
  "For the given t-norm norm and the names of the corresponding operators evaluates body in an
  dynamic environment where the fuzzy logic for norm is in effect."
  [norm & body]
  `(let [[x# y#] (t-norm ~norm)]
     (binding [~'f-star x#,
               ~'f-impl y#,
               ~'f-and (fn [x# y#] (f-star x# (f-impl x# y#))),
               ~'f-or  (fn [x# y#] (f-and (f-impl (f-impl x# y#) y#)
                                          (f-impl (f-impl y# x#) x#))),
               ~'f-neg (fn [x#] (f-impl x# 0))]
       ~@body)))

;;;

(deftype Fuzzy-Set [hashmap]
  Object
  (equals [this other]
    (boolean (or (identical? this other)
                 (when (= (class this) (class other))
                   (let [ohashmap (.hashmap ^Fuzzy-Set other)]
                     (and (forall [k (keys hashmap)]
                            (=> (not (zero? (hashmap k)))
                                (= (hashmap k) (ohashmap k))))
                          (forall [k (keys ohashmap)]
                            (=> (not (zero? (ohashmap k)))
                                (= (hashmap k) (ohashmap k))))))))))
  (hashCode [this]
    (hash-combine-hash Fuzzy-Set hashmap))
  ;;
  clojure.lang.ISeq
  (first [this]
    (first hashmap))
  (next [this]
    (next hashmap))
  (more [this]
    (if-let [n (next hashmap)]
      n
      ()))
  (cons [this o]
    (if (zero? (second o))
      this
      (Fuzzy-Set. (conj hashmap o))))
  (seq [this]
    (seq hashmap))
  (count [this]
    (count hashmap))
  (empty [this]
    (Fuzzy-Set. {}))
  (equiv [this other]
    (.equals ^Fuzzy-Set this other))
  ;;
  clojure.lang.IFn
  (invoke [this thing]
    (let [result (hashmap thing)]
      (or result 0))))

(defn make-fuzzy-set [map]
  (Fuzzy-Set. (select-keys map (map (fn [k] (and (< 0 (map k))
                                                 (<= (map k) 1)))
                                    (keys map)))))

;;;

(defn fuzzy-object-derivation
  "Computes the fuzzy derivation of the fuzzy set C of objects in
  the given context."
  [context C]
  (let [inz (incidence context)]
    (map-by-fn (fn [m]
                 (reduce (fn [a g]
                           (f-and a (f-impl (C g) (inz [g m]))))
                         1
                         (objects context)))
               (attributes context))))

(defn fuzzy-attribute-derivation
  "Computes the fuzzy derivation of the fuzzy set D of attributes in
  the given context."
  [context D]
  (let [inz (incidence context)]
    (map-by-fn (fn [g]
                 (reduce (fn [a m]
                           (f-and a (f-impl (D m) (inz [g m]))))
                         1
                         (attributes context)))
               (objects context))))

(defn- fuzzy-oplus-a
  ""
  [context U D a j]
  (let [D+ (assoc D j a),
        D+down (fuzzy-attribute-derivation context D+),
        D+down-up (fuzzy-object-derivation context D+down)]
    (set-of [k l] [[k l] U,
                   :when (>= (f-star (D+down k) (D+down-up l))
                             ((incidence context) [k l]))])))

(defn fuzzy-find-factors
  "Implements factorization with fuzzy logic. Note that you need to
  rebind all fuzzy operations."
  [context]
  (let [inz          (incidence context),
        find-maximal (fn [U D]
                       (max-key second
                                (map (fn [[g m] v]
                                       [[m v], (count (fuzzy-oplus-a context U D v m))])
                                     inz)))]
    (loop [U (set-of [g m] [g (objects context),
                            m (attributes context),
                            :when (not (zero? (inz [g m])))]),
           F #{}]
      (if (empty? U)
        F
        (let [D (loop [D {},
                       V 0,
                       [[j a] value] (find-maximal U D)]
                  (if (> value V)
                    (recur (assoc D j a)
                           value
                           (find-maximal U D))
                    D)),
              C (fuzzy-attribute-derivation context D),
              F (conj F [C,D]),
              U (set-of [i j] [[i j] U
                               :when (not (<= (inz [i j])
                                              (f-star (C i) (D j))))])]
          (recur U F))))))

;;;

nil
