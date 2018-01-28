;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.fast
  "Provides some optimized versions of the standard algorithms of conexp-clj"
  (:require [clojure.core.async :refer [<!! >!! chan close! thread]]
            [conexp.base :refer [illegal-argument improve-basic-order set-of]]
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
           [java.util ArrayList BitSet LinkedList List ListIterator]
           [clojure.lang IFn PersistentHashSet PersistentVector]))


;;; Helpers to convert to and from BitSets

(defmacro deep-aget
  "Implements fast, type-hinted aget. From Christophe Grand."
  ([hint array idx]
    `(aget ~(vary-meta array assoc :tag hint) (int ~idx)))
  ([hint array idx & idxs]
    `(let [a# (aget ~(vary-meta array assoc :tag 'objects) (int ~idx))]
       (deep-aget ~hint a# ~@idxs))))

(defmacro forall-in-bitset
  "Returns true iff body holds for all var in bitset."
  [[var bitset] & body]
  `(loop [~var (int (.nextSetBit ~bitset 0))]
     (cond
       (== -1 ~var) true,
       (not ~@body) false,
       :else (recur (.nextSetBit ~bitset (inc ~var))))))

(defmacro dobits
  "Executes body for every bit set in bitset. Var gets bound to the
  current location of the bit. Before every iteration, end-test (if
  given) is tested and the loop is stopped if it returns true."
  [[var bitset & end-test] & body]
  `(let [^BitSet bitset# ~bitset]
     (loop [~var (int (.nextSetBit bitset# 0))]
       (if (or (== -1 ~var) ~@end-test)
         nil
         (do
           ~@body
           (recur (int (.nextSetBit bitset# (inc ~var)))))))))

(defn filter-bitset
  "Returns a new bitset of all bits in bitset for which predicate returns true."
  [^IFn predicate, ^BitSet bitset]
  (let [result (BitSet.)]
    (dobits [i bitset]
      (when (predicate i)
        (.set result i)))
    result))

(defn to-bitset
  "Converts hashset to a BitSet. Elements are associated with indices
  as given by element-vector (i.e. the order in this vector is the
  order of the elements in the resulting BitSet."
  ^BitSet [^PersistentVector element-vector ^PersistentHashSet hashset]
  (let [nr-elements (int (count element-vector))
        ^BitSet bs (BitSet. nr-elements)]
    (loop [index nr-elements]
      (if (zero? index)
        bs
        (let [dec-index (dec index)
              elt (.get element-vector dec-index)]
          (when (.contains hashset elt)
            (.set bs index))
          (recur dec-index))))))

(defn to-hashset
  "Converts given bitset to a set, where element-vector gives the
  actual objects to be collected, i.e. element v at position p in
  element-vector is included in the result iff p is set in bitset."
  [^PersistentVector element-vector, ^BitSet bitset]
  (loop [pos    (int (.nextSetBit bitset 0)),
         result (transient #{})]
    (if (== -1 pos)
      (persistent! result)
      (recur (.nextSetBit bitset (inc pos))
             (conj! result (.get element-vector pos))))))

(defn to-binary-matrix
  "Converts the incidence-relation to a binary matrix (in the sense of
  Java) filled with 1 and 0."
  [object-vector attribute-vector incidence]
  (let [incidence-matrix (make-array Integer/TYPE (count object-vector) (count attribute-vector))]
    (dotimes [obj-idx (count object-vector)]
      (let [^ints row (aget ^objects incidence-matrix obj-idx)]
        (dotimes [att-idx (count attribute-vector)]
          (aset row att-idx
                (if (incidence [(nth object-vector obj-idx)
                                (nth attribute-vector att-idx)])
                  (int 1)
                  (int 0))))))
    incidence-matrix))

(defn to-binary-context
  "Returns [object-vector, attribute-vector, object-count,
  attribute-count, incidence-matrix] of context with the obvious
  definitions."
  [context]
  (let [object-vector    (vec (objects context)),
        attribute-vector (vec (attributes context)),
        object-count     (count object-vector),
        attribute-count  (count attribute-vector),
        incidence-matrix (to-binary-matrix object-vector attribute-vector (incidence context))]
    [object-vector attribute-vector object-count attribute-count incidence-matrix]))

(defmacro with-binary-context
  "For a given context defines object-vector, attribute-vector,
  object-count, attribute-count and incidence-matrix in the obvious
  way."
  [context & body]
  `(let [[~'object-vector ~'attribute-vector ~'object-count ~'attribute-count ~'incidence-matrix]
         (to-binary-context ~context)]
     ~@body))

(defn bitwise-object-derivation
  "Implements object derivation for BitSets."
  [incidence-matrix, object-count, attribute-count, ^BitSet bitset]
  (let [^BitSet derived-attributes (BitSet. attribute-count)]
    (dotimes [att (int attribute-count)]
      (when (forall-in-bitset [obj bitset]
              (== 1 (deep-aget ints incidence-matrix obj att)))
        (.set derived-attributes att)))
    derived-attributes))

(defn bitwise-attribute-derivation
  "Implements attribute derivation for BitSets."
  [incidence-matrix, object-count, attribute-count, ^BitSet bitset]
  (let [^BitSet derived-objects (BitSet. object-count)]
    (dotimes [obj (int object-count)]
      (when (forall-in-bitset [att bitset]
              (== 1 (deep-aget ints incidence-matrix obj att)))
        (.set derived-objects obj)))
    derived-objects))


;;; Next Closure

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
  "Computes the next closed set of closure after A.
  Returns nil if there is none."
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
            (let [^BitSet A_i (closure B)]
              (if (loop [j (long (dec i))]
                    (cond
                      (< j 0)
                      true,
                      (not= (.get A j) (.get A_i j))
                      false,
                      :else
                      (recur (dec j))))
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
  "Yields new bitset that is the closure of the input bitset under the given
  collection of implications of bitsets"
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

;; XXX: This does not yet apply to the external programs
(def ^:dynamic *concepts-do-conversion*
  "When set to anything true (the default), concepts will return proper concepts.
  Otherwise, the function returns a list of pairs of BitSets.  This does not yet
  apply to the :fcbo and :pcbo methods."
  true)

(defmulti concepts
  "Computes concepts with various algorithms, given as first
  argument. Default is :next-closure."
  {:arglists '([algorithm context] [context])}
  (fn [& args]
    (when-not (<= 1 (count args) 2)
      (illegal-argument "Wrong number of args."))
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
  (with-binary-context context
    (let [o-prime (partial bitwise-object-derivation incidence-matrix object-count attribute-count),
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
      (if *concepts-do-conversion*
        (map (fn [bitset]
               [(to-hashset object-vector (a-prime bitset)),
                (to-hashset attribute-vector bitset)])
             intents)
        intents))))


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

(defn- vychodil-generate-from
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
      (if *concepts-do-conversion*
        (map (fn [pair]
               [(to-hashset object-vector (first pair))
                (to-hashset attribute-vector (second pair))])
             (take-while (comp not nil?)
                         (repeatedly #(<!! output))))
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
      (if *concepts-do-conversion*
        (map (fn [A B]
               [(to-hashset object-vector A),
                (to-hashset attribute-vector B)])
             As Bs)
        (map vector As Bs)))))


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
