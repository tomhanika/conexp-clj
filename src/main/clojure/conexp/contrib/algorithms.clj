;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms
  "Provides some optimized versions of the standard algorithms of conexp-clj"
  (:require [clojure.core.async :refer [<!! >!! chan close! thread]]
            [conexp.base :refer [illegal-argument improve-basic-order]]
            [conexp.contrib.algorithms
             [bitwise :refer :all]
             [close-by-one :as cbo]
             [next-closure :as nc]]
            [conexp.fca
             [contexts :refer [attribute-derivation attributes context-attribute-closure context? incidence objects]]
             [implications :refer [make-implication]]])
  (:import conexp.fca.implications.Implication
           [java.util ArrayList BitSet LinkedList List ListIterator]))

;;; Computing Canonical Base efficiently

(defn- add-immediate-elements
  "Destructs implications and input-bitset"
  [^List implications, ^BitSet input-set, subset-test]
  (let [^ListIterator iter (.listIterator implications)]
    (loop [changed? false]
      (if-not (.hasNext iter)
        [input-set, implications, changed?]
        (let [^Implication impl (.next iter)]
          (if (subset-test (.premise impl) input-set)
            (do (dobits [x (.conclusion impl)]
                  (.set input-set x))
                (.remove iter)
                (recur true))
            (recur changed?)))))))

(defn- close-under-implications
  "Yields new bitset that is the closure of the input bitset under the given collection of
  implications of bitsetsn"
  [implications, ^BitSet set]
  (loop [set   (.clone set),
         impls (LinkedList. implications)]
    (let [[new impls changed?] (add-immediate-elements impls
                                                       set
                                                       (fn [^BitSet A, ^BitSet B]
                                                         (forall-in-bitset [x A]
                                                           (.get B x))))]
      (if changed?
        (recur new impls)
        new))))

(defn clop-by-implications
  [implications]
  (partial close-under-implications implications))


(defn canonical-base
  ([ctx]
   (canonical-base ctx #{}))
  ([ctx background-knowledge]
   (with-binary-context ctx
     (let [bg-knowledge (map (fn [^Implication impl]
                               (Implication. (to-bitset attribute-vector (.premise impl))
                                             (to-bitset attribute-vector (.conclusion impl))))
                             background-knowledge),
           next-closure (fn [implications last]
                          (nc/next-closed-set attribute-count
                                              (clop-by-implications implications)
                                              last)),
           runner       (fn runner [implications, ^BitSet candidate]
                          (when candidate
                            (let [conclusions (nc/bitwise-context-attribute-closure
                                               object-count
                                               attribute-count
                                               incidence-matrix
                                               candidate)]
                              (if (not= candidate conclusions)
                                (let [impl  (Implication. candidate
                                                          (filter-bitset #(not (.get candidate %))
                                                                         conclusions)),
                                      impls (conj implications impl)]
                                  (cons impl
                                        (lazy-seq (runner impls (next-closure impls candidate)))))
                                (recur implications (next-closure implications candidate))))))]
       (map (fn [^Implication bit-impl]
              (make-implication (to-hashset attribute-vector (.premise bit-impl))
                                (to-hashset attribute-vector (.conclusion bit-impl))))
            (lazy-seq (runner (vec bg-knowledge)
                              (close-under-implications bg-knowledge
                                                        (to-bitset attribute-vector #{})))))))))

;;; Compute Concepts of Formal Contexts efficiently

(defmulti concepts
  "Computes concepts with various algorithms, given as first
  argument. Default is :next-closure."
  {:arglists '([algorithm context] [context])}
  (fn [& args]
    (when-not (<= 1 (count args) 2)
      (illegal-argument "Wrong number of args passed to c.c.algorithms.concepts."))
    (when (and (= 1 (count args))
               (not (context? (first args))))
      (illegal-argument "Argument of concepts is not a context."))
    (when (and (= 2 (count args))
               (or (not (keyword? (first args)))
                   (not (context? (second args)))))
      (illegal-argument "First argument to concepts must be a keyword and the second argument must be a context."))
    (cond
     (= 1 (count args)) ::default-concepts
     (= 2 (count args)) (first args))))

(defmethod concepts ::default-concepts [context]
  (concepts :next-closure context))


;;; NextClosure (:next-closure)

(defmethod concepts :next-closure
  [_ context]
  (let [object-vector    (vec (objects context)),
        attribute-vector (vec (improve-basic-order (attributes context)
                                                   #(context-attribute-closure context %))),
        object-count     (count object-vector),
        attribute-count  (count attribute-vector),
        incidence-matrix (to-binary-matrix object-vector attribute-vector (incidence context)),

        o-prime (partial bitwise-object-derivation incidence-matrix object-count attribute-count),
        a-prime (partial bitwise-attribute-derivation incidence-matrix object-count attribute-count),
        start   (o-prime (a-prime (BitSet.))),
        intents (take-while identity
                            (iterate #(nc/next-closed-set attribute-count
                                                          (partial nc/bitwise-context-attribute-closure
                                                                   object-count
                                                                   attribute-count
                                                                   incidence-matrix)
                                                          %)
                                     start))]
    (map (fn [bitset]
           [(to-hashset object-vector (a-prime bitset)),
            (to-hashset attribute-vector bitset)])
         intents)))


;;; Vychodil (:vychodil)

(defn- compute-closure
  [object-count, attribute-count, incidence-matrix, rows, ^BitSet A, ^BitSet B, y]
  (let [^BitSet C (BitSet.),
        ^BitSet D (BitSet.),
        ^BitSet E (.clone A),
        y (int y)]
    (.set D 0 (int attribute-count))
    (.and E (aget ^objects rows y))
    (dobits [i E]
      (.set C i)
      (dotimes [j attribute-count]
        (if (== 0 (deep-aget ints incidence-matrix i j))
          (.set D j false))))
    [C, D]))

(defn vychodil-generate-from
  [output, object-count, attribute-count, incidence-matrix, rows, ^BitSet A, ^BitSet B, y]
  (>!! output [A, B])
  (when (and (not (== attribute-count (.cardinality B)))
             (< (int y) (int attribute-count)))
    (doseq [j (range (int y) (int attribute-count))
            :when (not (.get B j))
            :let [[^BitSet C, ^BitSet D] (compute-closure object-count attribute-count,
                                                          incidence-matrix rows,
                                                          A B j)
                  skip (loop [k (int 0)]
                         (cond
                           (== k j) false,
                           (not= (.get D k) (.get B k)) true,
                           :else (recur (inc k))))]
            :when (not skip)]
      (vychodil-generate-from output
                              object-count attribute-count,
                              incidence-matrix rows,
                              C D (inc j)))))

(alter-meta! (var vychodil-generate-from) assoc :private true)

(defmethod concepts :vychodil
  [_ context]
  (with-binary-context context
    (let [rows          (into-array (map (fn [y]
                                           (let [^BitSet bs (BitSet.)]
                                             (.set bs y)
                                             (bitwise-attribute-derivation incidence-matrix
                                                                           object-count
                                                                           attribute-count
                                                                           bs)))
                                         (range attribute-count)))
          empty-down    (bitwise-attribute-derivation incidence-matrix
                                                      object-count
                                                      attribute-count
                                                      (BitSet.))
          empty-down-up (bitwise-object-derivation incidence-matrix
                                                   object-count
                                                   attribute-count
                                                   empty-down)
          output        (chan)]
      (thread (vychodil-generate-from output
                                      object-count
                                      attribute-count
                                      incidence-matrix
                                      rows
                                      empty-down
                                      empty-down-up
                                      0)
              (close! output))
      (map (fn [pair]
             [(to-hashset object-vector (first pair))
              (to-hashset attribute-vector (second pair))])
           (take-while (comp not nil?)
                       (repeatedly #(<!! output)))))))


;;; In-Close (:in-close)

;; TODO: Check this, it's too slow.

(defn- cannonical?
  "Implements the IsCannonical method of In-Close."
  [incidence-matrix, ^BitSet A-new, ^BitSet B, y]
  (loop [j (dec (int y))]
    (cond
     (< j 0) true,
     (.get B j) (recur (dec j)),
     (forall-in-bitset [i A-new]
       (== 1 (deep-aget ints incidence-matrix i j)))
     false,
     :else (recur (dec j)))))

(defn- in-close
  "Implements the InClose method of In-Close."
  [attribute-count, incidence-matrix, ^List As, ^List Bs, last, current, y]
  (swap! last + 1)
  (.add As @last (BitSet.))
  (loop [j (int y)]
    (when (< j attribute-count)
      (.clear ^BitSet (.get As @last))
      (dobits [i (.get As current)]
        (when (== 1 (deep-aget ints incidence-matrix i j))
          (.set ^BitSet (.get As @last) i)))
      (if (== (.cardinality ^BitSet (.get As current))
              (.cardinality ^BitSet (.get As @last)))
        (.set ^BitSet (.get Bs current) j)
        (when (cannonical? incidence-matrix (.get As @last) (.get Bs current) j)
          (.add Bs @last (.clone ^BitSet (.get Bs current)))
          (.set ^BitSet (.get Bs @last) j)
          (in-close attribute-count incidence-matrix As Bs last @last (inc j))))
      (recur (inc j)))))

(defmethod concepts :in-close
  [_ context]
  (with-binary-context context
    (let [^List As (ArrayList.),
          ^List Bs (ArrayList.),
          last (atom 0)]
      (.add As 0 (BitSet.))
      (.set ^BitSet (.get As 0) 0 (int object-count))
      (.add Bs 0 (BitSet.))
      (in-close attribute-count incidence-matrix As Bs last 0 0)
      (map (fn [A B]
             [(to-hashset object-vector A),
              (to-hashset attribute-vector B)])
           As Bs))))

;;; Parallel-Close-by-One (:pcbo)

(defmethod concepts :pcbo
  [_ context]
  (map #(vector (attribute-derivation context %) ;this is slow
                %)
       (cbo/parallel-intents (* 2 (.availableProcessors (Runtime/getRuntime)))
                             3
                             0
                             context)))

;;; Fast Close-by-One (:fcbo)

(defmethod concepts :fcbo
  [_ context]
  (map #(vector (attribute-derivation context %) ;this is slow
                %)
       (cbo/fast-intents 0 context)))

;;;

true
