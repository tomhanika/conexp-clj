;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.algorithms
  "Provides some optimized versions of the standard algorithms of conexp-clj"
  (:require [clojure.core.async :refer [<!! >!! chan close! thread]]
            [conexp.base :refer [illegal-argument improve-basic-order set-of]]
            [conexp.fca.algorithms.bitwise :refer :all]
            [conexp.fca.contexts
             :refer
             [attribute-derivation
              attributes
              context-attribute-closure
              context?
              incidence
              make-context
              objects]]
            [conexp.fca.implications :refer [make-implication]]
            [conexp.io.util :refer :all]
            [conexp.util.exec :refer :all])
  (:import conexp.fca.implications.Implication
           [java.util ArrayList BitSet LinkedList List ListIterator]))

;;; Next Closure

(defn- lectic-<_i
  "Returns true iff A is lectically smaller than B at position i."
  [^long i, ^BitSet A, ^BitSet B]
  (and (.get B i)
       (not (.get A i))
       (loop [j (long (dec i))]
         (cond
          (< j 0)
          true,
          (not= (.get A j) (.get B j))
          false,
          :else
          (recur (dec j))))))

(defn- bitwise-context-attribute-closure
  "Computes the closure of A in the context given by the parameters."
  [^long object-count, ^long attribute-count, incidence-matrix, ^BitSet A]
  (let [^BitSet A (.clone A),
        ^BitSet B (BitSet.)]
    (dotimes [obj object-count]
      (when (forall-in-bitset [att A]
              (== 1 (deep-aget ints incidence-matrix obj att)))
        (.set B obj)))
    (dotimes [att attribute-count]
      (when (and (not (.get A att))
                 (forall-in-bitset [obj B]
                   (== 1 (deep-aget ints incidence-matrix obj att))))
        (.set A att)))
    A))

(defn next-closed-set
  "Computes the next closed set of closure after A. Returns nil if there is none."
  [^long attribute-count, closure, ^BitSet A]
  (let [^BitSet B (.clone A)]
    (loop [i (dec attribute-count)]
      (cond
       (== -1 i)
       nil,
       (.get B i)
       (do (.clear B i)
           (recur (dec i))),
       :else
       (do (.set B i)
           (let [A_i (closure B)]
             (if (lectic-<_i i A A_i)
               A_i
               (do (.clear B i)
                   (recur (dec i))))))))))


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

(defn- clop-by-implications
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
                          (next-closed-set attribute-count
                                           (clop-by-implications implications)
                                           last)),
           runner       (fn runner [implications, ^BitSet candidate]
                          (when candidate
                            (let [conclusions (bitwise-context-attribute-closure
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
                            (iterate #(next-closed-set attribute-count
                                                       (partial bitwise-context-attribute-closure
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

(defn- string-to-ints
  "Given a string of numbers and spaces, and a vector returns the
  elements of the vector whose indices are represented by the string,
  in that order."
  [vec str]
  (loop [str         str,
         current-int -1,
         ints        (transient #{})]
    (if (empty? str)
      (if (neg? current-int)
        (persistent! ints)
        (persistent! (conj! ints (nth vec current-int))))
      (let [next-char (first str)]
        (if (= \space next-char)
          (recur (rest str)
                 -1
                 (if (neg? current-int)
                   ints
                   (conj! ints (nth vec current-int))))
          (recur (rest str)
                 (let [next-int (int (Character/digit ^Character next-char 10))]
                   (long (+ (* 10 (max 0 current-int)) next-int)))
                 ints))))))

(defn- to-fcalgs-context
  "Transforms ctx to a context suitable for the :fcalgs context
  format, returns a vector [context object-vector attribute-vector]."
  [ctx]
  (let [object-vector    (vec (objects ctx)),
        attribute-vector (vec (attributes ctx)),

        new-objects      (range (count object-vector)),
        new-attributes   (range (count attribute-vector)),
        new-incidence    (set-of [g m] [g new-objects,
                                        m new-attributes,
                                        :when (contains? (incidence ctx)
                                                         [(nth object-vector g)
                                                          (nth attribute-vector m)])])]
    [(make-context new-objects new-attributes new-incidence),
     object-vector,
     attribute-vector]))

(define-external-program pcbo-external
  pcbo :threads :depth :min-support :verbosity :input-file :fcalgs :output-file)
(alter-meta! (var pcbo-external) assoc :private true)

(defn- pcbo
  "Runs pcbo and returns the file where the concepts have been written
  to."
  [threads depth min-support verbosity context]
  (let [^java.io.File output-file (tmpfile)]
    (pcbo-external (str "-P" threads)
                   (str "-L" depth)
                   (str "-S" (float (* min-support 100)))
                   (str "-V" verbosity)
                   context (.getAbsolutePath output-file))
    output-file))

(defn parallel-intents
  "Computes the intents of context using parallel close-by-one (PCbO)."
  [threads depth min-support context]
  (let [[ctx _ att-vec] (to-fcalgs-context context),
        output-file     (pcbo threads depth min-support 1 ctx),
        intents         (line-seq (reader output-file)),
        transform-back  #(string-to-ints att-vec %)]
    (if (= "" (first intents))
      (cons #{} (map transform-back (rest intents)))
      (map transform-back intents))))

(defn parallel-count-intents
  "Counts the intents of context using parallel close-by-one (PCbO)."
  [threads depth min-support context]
  (-> (pcbo threads depth min-support 1 (first (to-fcalgs-context context)))
      reader
      line-seq
      count))

(defmethod concepts :pcbo
  [_ context]
  (map #(vector (attribute-derivation context %) ;this is slow
                %)
       (parallel-intents (* 2 (.availableProcessors (Runtime/getRuntime)))
                         3
                         0
                         context)))


;;; Fast Close-by-One (:fcbo)

(define-external-program fcbo-external
  pcbo :min-support :verbosity :input-file :fcalgs :output-file)
(alter-meta! (var fcbo-external) assoc :private true)

(defn- fcbo
  "Runs fcbo and returns the file where the concepts have been written
  to."
  [min-support verbosity context]
  (let [^java.io.File output-file (tmpfile)]
    (fcbo-external (str "-S" (float (* min-support 100)))
                   (str "-V" verbosity)
                   context (.getAbsolutePath output-file))
    output-file))

(defn fast-intents
  "Computes the intents of context using fast close-by-one (FCbO)."
  [min-support context]
  (let [[ctx _ att-vec] (to-fcalgs-context context),
        output-file     (fcbo min-support 1 ctx),
        intents         (line-seq (reader output-file)),
        transform-back  #(string-to-ints att-vec %)]
    (if (= "" (first intents))
      (cons #{} (map transform-back (rest intents)))
      (map transform-back intents))))

(defn fast-count-intents
  "Counts the intents of context using parallel close-by-one (CbO)."
  [min-support context]
  (-> (fcbo min-support 1 (first (to-fcalgs-context context)))
      reader
      line-seq
      count))

(defmethod concepts :fcbo
  [_ context]
  (map #(vector (attribute-derivation context %) ;this is slow
                %)
       (fast-intents 0 context)))

;;;

true
