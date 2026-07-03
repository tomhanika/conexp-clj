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
            [conexp.base :refer [illegal-argument set-of]]
            [conexp.fca.closure-systems :refer [improve-basic-order]]
            [conexp.fca.contexts
             :refer
             [attribute-derivation
              attributes
              context-attribute-closure
              context?
              incidence
              make-context
              dual-context
              objects]]
            [conexp.fca.implications :refer [make-implication]]
            [conexp.io.util :refer :all]
            [conexp.util.exec :refer :all]
            [clojure.set :refer [difference]]
            [clojure.core.async :as r])
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
            (.set bs dec-index))
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
  ([context]
   (let [object-vector    (vec (objects context)),
         attribute-vector (vec (attributes context)),
         object-count     (count object-vector),
         attribute-count  (count attribute-vector),
         incidence-matrix (to-binary-matrix object-vector attribute-vector (incidence context))]
     [object-vector attribute-vector object-count attribute-count incidence-matrix]))
  ([object-vector attribute-vector incidence] ;; if you need a specific order on the objects or attributes
   (let [object-count     (count object-vector)
         attribute-count  (count attribute-vector)
         incidence-matrix (to-binary-matrix object-vector attribute-vector incidence)]
     [object-vector attribute-vector object-count attribute-count incidence-matrix])))

(defmacro with-binary-context
  "For a given context defines object-vector, attribute-vector,
  object-count, attribute-count and incidence-matrix in the obvious
  way."
  [context & body]
  `(let [[~'object-vector ~'attribute-vector ~'object-count ~'attribute-count ~'incidence-matrix]
         (if (context? ~context) (to-binary-context ~context) ~context)]
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

(defn bitwise-context-attribute-closure
  "Computes the closure of A in the context given by the parameters."
  [incidence-matrix, ^long object-count, ^long attribute-count, ^BitSet A]
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


;;;;;
;;; Canonical base for contexts with at most 64 attributes.  Here an attribute
;;; set fits in a single `long`, so subset test, union and the whole implicational
;;; closure become single machine-word operations instead of BitSet method calls;
;;; the derivation A'' is a linear scan of the objects' rows.

(defn- to-long
  "Encodes a hashset of attributes as a long via `attribute-vector` (<=64 attrs)."
  ^long [^PersistentVector attribute-vector hashset]
  (loop [i (int (dec (count attribute-vector))), acc 0]
    (if (< i 0)
      acc
      (recur (dec i)
             (if (.contains ^PersistentHashSet hashset (.get attribute-vector i))
               (bit-or acc (bit-shift-left 1 i))
               acc)))))

(defn- long->hashset
  [^PersistentVector attribute-vector ^long x]
  (loop [i 0, s (transient #{})]
    (if (< i (count attribute-vector))
      (recur (inc i) (if (bit-test x i) (conj! s (.get attribute-vector i)) s))
      (persistent! s))))

(defn- long-attribute-rows
  "For each object a long whose bit m is set iff the object has attribute m."
  ^longs [incidence-matrix ^long object-count ^long attribute-count]
  (let [rows (long-array object-count)]
    (dotimes [g object-count]
      (let [^ints row (aget ^objects incidence-matrix g)]
        (aset rows g (long (loop [m 0, acc 0]
                             (if (< m attribute-count)
                               (recur (inc m)
                                      (if (== 1 (aget row m))
                                        (bit-or acc (bit-shift-left 1 m))
                                        acc))
                               acc))))))
    rows))

(defn- long-adprime
  "Closure A'' of the long-encoded attribute set A."
  ^long [^longs rows ^long object-count ^long mask ^long A]
  (loop [g 0, cl mask]
    (if (< g object-count)
      (recur (inc g)
             (let [row (aget rows g)]
               (if (zero? (bit-and A (bit-not row)))    ; A subset of row -> object in extent
                 (bit-and cl row)
                 cl)))
      cl)))

(defn- long-close-under-implications
  "Closure of X under the implications (premise/conclusion long-arrays of length n)."
  ^long [^longs prem ^longs concl ^long n ^long X]
  (loop [result X]
    (let [nr (loop [i 0, r result]
               (if (< i n)
                 (recur (inc i)
                        (if (zero? (bit-and (aget prem i) (bit-not r)))
                          (bit-or r (aget concl i))
                          r))
                 r))]
      (if (== nr result) result (recur nr)))))

(defn- long-next-closed-set
  "Next closed set after A in lectic order; returns a boxed long or nil.
  (Clojure allows at most 4 primitive-hinted args, so the longs are coerced
  inside instead of hinted on the parameter list.)"
  [attribute-count ^longs prem ^longs concl n A]
  (let [n (long n), A (long A)]
    (loop [i (long (dec (long attribute-count))), B A]
      (cond
        (< i 0) nil
        (bit-test B i) (recur (dec i) (bit-clear B i))
        :else (let [Ai  (long-close-under-implications prem concl n (bit-set B i))
                    low (unchecked-dec (bit-shift-left 1 i))]
                (if (== (bit-and A low) (bit-and Ai low))
                  Ai
                  (recur (dec i) B)))))))

(defn canonical-base-long
  "Canonical base for a context with at most 64 attributes, using long-encoded
  attribute sets.  Same result as `canonical-base`."
  ([ctx] (canonical-base-long ctx #{}))
  ([ctx background-knowledge]
   (with-binary-context ctx
     (let [M     (int attribute-count)
           mask  (long (if (== M 64) -1 (unchecked-dec (bit-shift-left 1 M))))
           rows  (long-attribute-rows incidence-matrix object-count M)
           prem* (object-array 1)
           concl* (object-array 1)
           n*    (int-array 1)
           _     (do (aset prem*  0 (long-array (max 16 (count background-knowledge))))
                     (aset concl* 0 (long-array (max 16 (count background-knowledge)))))
           add!  (fn [^long P ^long C]
                   (let [i (aget n* 0), ^longs p (aget prem* 0)]
                     (when (>= i (alength p))
                       (aset prem*  0 (java.util.Arrays/copyOf p (* 2 (alength p))))
                       (aset concl* 0 (java.util.Arrays/copyOf ^longs (aget concl* 0) (* 2 (alength p)))))
                     (aset ^longs (aget prem* 0) i P)
                     (aset ^longs (aget concl* 0) i C)
                     (aset n* 0 (inc i))))]
       (doseq [^Implication impl background-knowledge]
         (add! (to-long attribute-vector (.premise impl))
               (to-long attribute-vector (.conclusion impl))))
       (letfn [(runner [^long candidate]
                 (let [closure (long-adprime rows object-count mask candidate)]
                   (if (== candidate closure)
                     (let [nx (long-next-closed-set M (aget prem* 0) (aget concl* 0) (aget n* 0) candidate)]
                       (when nx (recur (long nx))))
                     (let [C (bit-and closure (bit-not candidate))]
                       (add! candidate C)
                       (cons [candidate C]
                             (lazy-seq
                              (let [nx (long-next-closed-set M (aget prem* 0) (aget concl* 0) (aget n* 0) candidate)]
                                (when nx (runner (long nx))))))))))]
         (map (fn [pair]
                (make-implication (long->hashset attribute-vector (long (nth pair 0)))
                                  (long->hashset attribute-vector (long (nth pair 1)))))
              (lazy-seq
               (let [start (long-close-under-implications (aget prem* 0) (aget concl* 0) (aget n* 0) 0)]
                 (runner start)))))))))

;;; Canonical base for contexts with MORE than 64 attributes, using long[]
;;; (arrays of 64-bit words).  Subset test, union, the whole implicational
;;; closure and the derivation A'' become tight word loops (a couple of
;;; and/or/compare per 64 attributes).

(defn- longs-subset?
  "True iff attribute set A is a subset of B (both long[] of equal length)."
  [^longs A ^longs B]
  (let [w (alength A)]
    (loop [i 0]
      (cond
        (== i w) true
        (not (zero? (bit-and (aget A i) (bit-not (aget B i))))) false
        :else (recur (inc i))))))

(defn- longs-and-into!
  [^longs dst ^longs src]
  (dotimes [i (alength dst)] (aset dst i (bit-and (aget dst i) (aget src i))))
  dst)

(defn- longs-union-changed!
  "dst |= src in place; returns true iff dst gained a bit."
  [^longs dst ^longs src]
  (loop [i 0, changed false]
    (if (< i (alength dst))
      (let [nw (bit-or (aget dst i) (aget src i))]
        (if (== nw (aget dst i))
          (recur (inc i) changed)
          (do (aset dst i nw) (recur (inc i) true))))
      changed)))

(defn- longs-agree-below?
  "True iff A and Ai agree on all bits below i."
  [^longs A ^longs Ai ^long i]
  (let [wi (quot i 64)]
    (and (loop [w 0]
           (cond (== w wi) true
                 (== (aget A w) (aget Ai w)) (recur (inc w))
                 :else false))
         (let [low (unchecked-dec (bit-shift-left 1 (rem i 64)))]
           (== (bit-and (aget A wi) low) (bit-and (aget Ai wi) low))))))

(defn- longs-mask
  ^longs [^long attribute-count]
  (let [mask (long-array (quot (+ attribute-count 63) 64))]
    (dotimes [b attribute-count]
      (aset mask (quot b 64) (bit-or (aget mask (quot b 64)) (bit-shift-left 1 (rem b 64)))))
    mask))

(defn- longs-attribute-rows
  "long[][]: for each object the long[] of its attributes."
  [incidence-matrix ^long object-count ^long attribute-count]
  (let [rows (make-array Long/TYPE object-count (quot (+ attribute-count 63) 64))]
    (dotimes [g object-count]
      (let [^ints row (aget ^objects incidence-matrix g)
            ^longs out (aget ^objects rows g)]
        (dotimes [m attribute-count]
          (when (== 1 (aget row m))
            (aset out (quot m 64) (bit-or (aget out (quot m 64)) (bit-shift-left 1 (rem m 64))))))))
    rows))

(defn- to-longs
  ^longs [^PersistentVector attribute-vector hashset]
  (let [M (int (count attribute-vector))
        out (long-array (quot (+ M 63) 64))]
    (dotimes [i M]
      (when (.contains ^PersistentHashSet hashset (.get attribute-vector i))
        (aset out (quot i 64) (bit-or (aget out (quot i 64)) (bit-shift-left 1 (rem i 64))))))
    out))

(defn- longs->hashset
  [^PersistentVector attribute-vector ^longs x]
  (loop [i 0, s (transient #{})]
    (if (< i (count attribute-vector))
      (recur (inc i) (if (bit-test (aget x (quot i 64)) (rem i 64))
                       (conj! s (.get attribute-vector i)) s))
      (persistent! s))))

(defn- longs-adprime
  ^longs [^objects rows ^long object-count ^longs mask ^longs A]
  (let [result (aclone mask)]
    (dotimes [g object-count]
      (let [^longs row (aget rows g)]
        (when (longs-subset? A row)
          (longs-and-into! result row))))
    result))

(defn- longs-close-under-implications
  ^longs [^java.util.ArrayList prem ^java.util.ArrayList concl ^long n ^longs X]
  (let [result (aclone X)]
    (loop []
      (if (loop [i 0, ch false]
            (if (< i n)
              (recur (inc i)
                     (let [^longs p (.get prem i)]
                       (if (longs-subset? p result)
                         (if (longs-union-changed! result ^longs (.get concl i)) true ch)
                         ch)))
              ch))
        (recur)
        result))))

(defn- longs-next-closed-set
  "Next closed set after A in lectic order; returns a long[] or nil."
  [attribute-count ^java.util.ArrayList prem ^java.util.ArrayList concl n ^longs A]
  (let [M (long attribute-count), n (long n), ^longs B (aclone A)]
    (loop [i (dec M)]
      (cond
        (< i 0) nil
        (bit-test (aget B (quot i 64)) (rem i 64))
        (do (aset B (quot i 64) (bit-clear (aget B (quot i 64)) (rem i 64)))
            (recur (dec i)))
        :else
        (do (aset B (quot i 64) (bit-set (aget B (quot i 64)) (rem i 64)))
            (let [^longs Ai (longs-close-under-implications prem concl n B)]
              (if (longs-agree-below? A Ai i)
                Ai
                (do (aset B (quot i 64) (bit-clear (aget B (quot i 64)) (rem i 64)))
                    (recur (dec i))))))))))

(defn canonical-base-longs
  "Canonical base using long[]-encoded attribute sets, for contexts with more
  than 64 attributes.  Same result as `canonical-base`."
  ([ctx] (canonical-base-longs ctx #{}))
  ([ctx background-knowledge]
   (with-binary-context ctx
     (let [M     (int attribute-count)
           mask  (longs-mask M)
           rows  (longs-attribute-rows incidence-matrix object-count M)
           prem  (java.util.ArrayList.)
           concl (java.util.ArrayList.)
           add!  (fn [^longs P ^longs C] (.add prem P) (.add concl C))]
       (doseq [^Implication impl background-knowledge]
         (add! (to-longs attribute-vector (.premise impl))
               (to-longs attribute-vector (.conclusion impl))))
       (letfn [(runner [^longs candidate]
                 (let [closure (longs-adprime rows object-count mask candidate)]
                   (if (java.util.Arrays/equals candidate closure)
                     (let [nx (longs-next-closed-set M prem concl (.size prem) candidate)]
                       (when nx (recur nx)))
                     (let [C (aclone closure)]
                       (dotimes [w (alength C)]
                         (aset C w (bit-and (aget C w) (bit-not (aget candidate w)))))
                       (add! candidate C)
                       (cons [candidate C]
                             (lazy-seq
                              (let [nx (longs-next-closed-set M prem concl (.size prem) candidate)]
                                (when nx (runner nx)))))))))]
         (map (fn [pair]
                (make-implication (longs->hashset attribute-vector (nth pair 0))
                                  (longs->hashset attribute-vector (nth pair 1))))
              (lazy-seq
               (let [start (longs-close-under-implications prem concl (.size prem)
                                                           (long-array (alength mask)))]
                 (runner start)))))))))

(defn- canonical-base-bitset
  ([ctx]
   (canonical-base-bitset ctx #{}))
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
                                               incidence-matrix
                                               object-count
                                               attribute-count
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

(defn canonical-base
  "Computes the canonical (Duquenne-Guigues) base of `ctx` as a lazy sequence,
  optionally reduced by the set of implications `background-knowledge` (which
  will not appear in the result).

  Uses a long-encoded implementation when the context has at most 64 attributes
  and a BitSet implementation otherwise; both yield the same base."
  ([ctx] (canonical-base ctx #{}))
  ([ctx background-knowledge]
   (if (<= (long (if (context? ctx) (count (attributes ctx)) (nth ctx 3))) 64)
     (canonical-base-long ctx background-knowledge)
     (canonical-base-longs ctx background-knowledge))))

;;; PAC approximation of the canonical base (Angluin's HORN1 with a sampling
;;; equivalence oracle), long-encoded for contexts with at most 64 attributes.
;;; The membership oracle "is S an intent?" is a single long derivation, the
;;; equivalence oracle draws random longs, and every set operation (subset test,
;;; union, intersection) is a single machine word -- so the many oracle calls are
;;; far cheaper than in the hash-set implementation.

(defn approx-canonical-base-long
  "Long-encoded version of conexp.fca.implications/approx-canonical-base for
  contexts with at most 64 attributes."
  [ctx epsilon delta]
  (with-binary-context ctx
    (let [M    (int attribute-count)
          mask (long (if (== M 64) -1 (unchecked-dec (bit-shift-left 1 M))))
          rows (long-attribute-rows incidence-matrix object-count M)
          ^java.util.Random rng (java.util.Random.)
          logd (/ (Math/log (/ (double delta))) (Math/log 2.0))
          inve (/ (double epsilon))
          sub?      (fn [^long a ^long b] (zero? (bit-and a (bit-not b))))     ; a ⊆ b
          intent?   (fn [^long s] (== s (long-adprime rows object-count mask s)))
          resp?     (fn [^long s ^long p ^long c] (or (not (sub? p s)) (sub? c s)))
          resp-all? (fn [^long s hyp]
                      (every? (fn [pc] (resp? s (long (nth pc 0)) (long (nth pc 1)))) hyp))
          mk        (fn [^long p ^long c] [p (bit-and c (bit-not p))])          ; normalized impl
          counter   (atom 0)
          equivalent?
          (fn [hyp]                                     ; -> true, or a counterexample (long)
            (let [nr (long (Math/ceil (* inve (+ (swap! counter inc) logd))))]
              (loop [k 0]
                (if (< k nr)
                  (let [s (bit-and mask (.nextLong rng))]
                    (if (= (intent? s) (resp-all? s hyp))
                      (recur (inc k))
                      s))
                  true))))]
      (loop [hyp []]
        (let [ce (equivalent? hyp)]
          (if (true? ce)
            (map (fn [pc] (make-implication (long->hashset attribute-vector (long (nth pc 0)))
                                            (long->hashset attribute-vector (long (nth pc 1)))))
                 hyp)
            (let [ce (long ce)]
              (if (some (fn [pc] (not (resp? ce (long (nth pc 0)) (long (nth pc 1))))) hyp)
                (recur (mapv (fn [pc]
                               (let [p (long (nth pc 0)), c (long (nth pc 1))]
                                 (if (resp? ce p c) pc (mk p (bit-and c ce)))))
                             hyp))
                (let [idx (loop [i 0]
                            (if (< i (count hyp))
                              (let [p (long (nth (nth hyp i) 0)), rp (bit-and ce p)]
                                (if (and (not (== rp p)) (not (intent? rp))) i (recur (inc i))))
                              nil))]
                  (if idx
                    (recur (let [pc (nth hyp idx), p (long (nth pc 0)), c (long (nth pc 1))
                                 rp (bit-and ce p)]
                             (assoc hyp idx (mk rp (bit-or c (bit-and p (bit-not rp)))))))
                    (recur (conj hyp (mk ce mask)))))))))))))

(defn- longs-and     ^longs [^longs a ^longs b]
  (let [r (aclone a)] (dotimes [i (alength r)] (aset r i (bit-and (aget r i) (aget b i)))) r))
(defn- longs-and-not ^longs [^longs a ^longs b]                    ; a \ b
  (let [r (aclone a)] (dotimes [i (alength r)] (aset r i (bit-and (aget r i) (bit-not (aget b i))))) r))
(defn- longs-or      ^longs [^longs a ^longs b]
  (let [r (aclone a)] (dotimes [i (alength r)] (aset r i (bit-or (aget r i) (aget b i)))) r))

(defn approx-canonical-base-longs
  "long[]-encoded version of approx-canonical-base-long, for contexts with more
  than 64 attributes."
  [ctx epsilon delta]
  (with-binary-context ctx
    (let [M    (int attribute-count)
          W    (int (quot (+ M 63) 64))
          mask (longs-mask M)
          rows (longs-attribute-rows incidence-matrix object-count M)
          ^java.util.Random rng (java.util.Random.)
          logd (/ (Math/log (/ (double delta))) (Math/log 2.0))
          inve (/ (double epsilon))
          intent?   (fn [^longs s] (java.util.Arrays/equals s ^longs (longs-adprime rows object-count mask s)))
          resp?     (fn [^longs s ^longs p ^longs c] (or (not (longs-subset? p s)) (longs-subset? c s)))
          resp-all? (fn [^longs s hyp] (every? (fn [pc] (resp? s (nth pc 0) (nth pc 1))) hyp))
          mk        (fn [^longs p ^longs c] [p (longs-and-not c p)])
          rand-sub  (fn [] (let [r (long-array W)]
                             (dotimes [i W] (aset r i (bit-and (.nextLong rng) (aget mask i))))
                             r))
          counter   (atom 0)
          equivalent?
          (fn [hyp]
            (let [nr (long (Math/ceil (* inve (+ (swap! counter inc) logd))))]
              (loop [k 0]
                (if (< k nr)
                  (let [s (rand-sub)]
                    (if (= (intent? s) (resp-all? s hyp)) (recur (inc k)) s))
                  true))))]
      (loop [hyp []]
        (let [ce (equivalent? hyp)]
          (if (true? ce)
            (map (fn [pc] (make-implication (longs->hashset attribute-vector (nth pc 0))
                                            (longs->hashset attribute-vector (nth pc 1))))
                 hyp)
            (let [^longs ce ce]
              (if (some (fn [pc] (not (resp? ce (nth pc 0) (nth pc 1)))) hyp)
                (recur (mapv (fn [pc]
                               (let [p (nth pc 0), c (nth pc 1)]
                                 (if (resp? ce p c) pc (mk p (longs-and c ce)))))
                             hyp))
                (let [idx (loop [i 0]
                            (if (< i (count hyp))
                              (let [^longs p (nth (nth hyp i) 0), rp (longs-and ce p)]
                                (if (and (not (java.util.Arrays/equals rp p)) (not (intent? rp)))
                                  i (recur (inc i))))
                              nil))]
                  (if idx
                    (recur (let [pc (nth hyp idx), ^longs p (nth pc 0), ^longs c (nth pc 1)
                                 rp (longs-and ce p)]
                             (assoc hyp idx (mk rp (longs-or c (longs-and-not p rp))))))
                    (recur (conj hyp (mk ce mask)))))))))))))


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
                                                                  incidence-matrix
                                                                  object-count
                                                                  attribute-count)
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

;;; Parallel Next Closed sets (Agents)

(defn next-intent-iterator
  "This method is a wrapper for the next-closed-set method. It returns the next closed set given 'start."
  [[object-vector attribute-vector object-count attribute-count incidence-matrix] start]
  (let [o-prime (partial bitwise-object-derivation incidence-matrix object-count attribute-count),
        a-prime (partial bitwise-attribute-derivation incidence-matrix object-count attribute-count),
        next (next-closed-set attribute-count
                         (partial bitwise-context-attribute-closure
                                  object-count
                                  attribute-count
                                  incidence-matrix)
                         start)]
    next))


(defn next-intent-async
"This method computes all closed sets starting with 'start (exclusive false per default)
  using next-closure. All closed sets are computed asynchron
  and are put into the return channel. The used lectic order has
  'start as lowest elements, such that all computed closed sets
  contain at least one element of 'start.
  (Read closed sets with <!! until :fin is returned)"

  ([ctx start & [exlusive]]
   (let [other-attributes (difference 
                           (attributes ctx)
                           start)
         attr-order (into
                     (vec start) 
                     (vec other-attributes))
         obj-vec (vec (objects ctx))
         bin-incidence (to-binary-matrix obj-vec attr-order 
                                         (fn ([a b] ((incidence ctx) [a b]))
                                           ([[a b]] ((incidence ctx) [a b]))))
         bin-ctx [obj-vec attr-order (count obj-vec) (count attr-order)  bin-incidence]
         init (BitSet.)
         setter (if (not (empty? start))
                  (.set init (count start) (count attr-order) true))

         first (if exlusive 
                 (next-intent-iterator bin-ctx init)
                 (bitwise-object-derivation bin-incidence (count obj-vec) (count attr-order)
                                            (bitwise-attribute-derivation bin-incidence (count obj-vec) (count attr-order) init)))
         
         concepts (r/chan)]
     (r/go-loop [bin-next first]
       (if (nil? bin-next) 
         (r/>! concepts :fin)
         (do 
           (r/>! concepts (to-hashset attr-order bin-next))
           (recur (next-intent-iterator bin-ctx bin-next)))))
     concepts))
  ([ctx]
   (next-intent-async ctx #{})))

(defn next-extent-async
"This method computes all closed sets starting with 'start (inclusive
  start) using next-closure. All closed sets are computed asynchron
  and are put into the return channel. The used lectic order has
  'start as lowest elements, such that all computed closed sets
  contain at least one element of 'start.
  (Read closed sets with <!! until :fin is returned)"
  ([ctx start]
   (next-intent-async (dual-context ctx) start))
  ([ctx]
   (next-intent-async (dual-context ctx) #{})))
