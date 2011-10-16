;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(in-ns 'conexp.base)

(use 'clojure.contrib.profile,
     'clojure.test)

(import 'javax.swing.JOptionPane
        'java.util.Calendar
        'java.text.SimpleDateFormat)

;;;

(defn immigrate
  "Create a public var in this namespace for each public var in the
  namespaces named by ns-names. The created vars have the same name, root
  binding, and metadata as the original except that their :ns metadata
  value is this namespace.

  This function is literally copied from the clojure.contrib.ns-utils library."
  [& ns-names]
  (doseq [ns ns-names]
    (require ns)
    (doseq [[sym, ^clojure.lang.Var var] (ns-publics ns)]
      (let [sym (with-meta sym (assoc (meta var) :ns *ns*))]
        (if (.hasRoot var)
          (intern *ns* sym (.getRawRoot var))
          (intern *ns* sym))))))

(immigrate 'clojure.set
           'clojure.core.incubator
           'clojure.math.numeric-tower
           'clojure.contrib.def)

;;; Namespace documentation

(defmacro update-ns-meta!
  "Updates meta hash of given namespace with given description."
  [ns & key-value-description]
  `(alter-meta! (find-ns '~ns)
                (fn [meta-hash#]
                  (merge meta-hash# ~(apply hash-map key-value-description)))))

(defmacro ns-doc
  "Sets the documentation of the current namespace to be doc."
  [doc]
  (let [ns (symbol (str *ns*))]
    `(update-ns-meta! ~ns :doc ~doc)))


;;; Testing

(defmacro tests-to-run
  "Defines tests to run when the namespace in which this macro is
  called is tested by test-ns."
  [& namespaces]
  `(defn ~'test-ns-hook []
     (doseq [ns# '~namespaces]
       (let [result# (do (require ns#) (test-ns ns#))]
         (dosync
          (ref-set *report-counters*
                   (merge-with + @*report-counters* result#)))))))

(defmacro with-testing-data
  "Expects for all bindings the body to be evaluated to true. bindings
  must be those of doseq."
  [bindings & body]
  `(doseq ~bindings
     ~@(map (fn [expr]
              `(let [result# (try
                               (do ~expr)
                               (catch Throwable e#
                                 (println "Caught exception:" e#)
                                 false))]
                 (if-not result#
                   ~(let [vars (vec (remove keyword? (take-nth 2 bindings)))]
                      `(do (println "Test failed for" '~vars "being" ~vars)
                           (is false)))
                   (is true))))
            body)))

;;; Types

(defvar clojure-set clojure.lang.PersistentHashSet)
(defvar clojure-fn  clojure.lang.Fn)
(defvar clojure-seq clojure.lang.Sequential)
(defvar clojure-vec clojure.lang.PersistentVector)
(defvar clojure-map clojure.lang.Associative)
(defvar clojure-coll clojure.lang.IPersistentCollection)

(defn clojure-type
  "Dispatch function for multimethods."
  [thing]
  (class thing))


;;; Technical Helpers

(defn proper-subset?
  [set-1 set-2]
  "Returns true iff set-1 is a proper subset of set-2."
  (and (not= set-1 set-2)
       (subset? set-1 set-2)))

(defn proper-superset?
  [set-1 set-2]
  "Returns true iff set-1 is a proper superset of set-2."
  (proper-subset? set-2 set-1))

(defn singleton?
  "Returns true iff given thing is a singleton sequence or set."
  [x]
  (and (or (set? x) (sequential? x))
       (not (empty? x))
       (not (next x))))

(defn ensure-length
  "Fills given string with padding to have at least the given length."
  ([string length]
     (ensure-length string length " "))
  ([string length padding]
     (apply str string (repeat (- length (count string)) padding))))

(defn with-str-out
  "Returns string of all output being made in (flatten body)."
  [& body]
  (with-out-str
    (doseq [element (flatten body)]
      (print element))))

(defn- compare-order
  "Orders things for proper output of formal contexts."
  [x y]
  (if (and (= (class x) (class y))
           (instance? Comparable x))
    (> 0 (compare x y))
    (> 0 (compare (str (class x)) (str (class y))))))

(defn sort-by-second
  "Ensures that pairs are ordered by second entry first. This gives
  better output for context sums, products, ..."
  [x y]
  (cond
    (and (vector? x)
         (vector? y)
         (= 2 (count x) (count y)))
    (if (= (second x) (second y))
      (compare-order (first x) (first y))
      (compare-order (second x) (second y)))
    :else
    (compare-order x y)))

(defn sort-by-first
  "Ensures that pairs are ordered by first entry first."
  [x y]
  (cond
    (and (vector? x)
         (vector? y)
         (= 2 (count x) (count y)))
    (if (= (first x) (first y))
      (compare-order (second x) (second y))
      (compare-order (first x) (first y)))
    :else
    (compare-order x y)))

(defn zip
  "Returns sequence of pairs [x,y] where x runs through seq-1 and
  y runs through seq-2 simultaneously. This is the same as
  (map #(vector %1 %2) seq-1 seq-2)."
  [seq-1 seq-2]
  (map #(vector %1 %2) seq-1 seq-2))

(defn first-non-nil
  "Returns first non-nil element in seq, or nil if there is none."
  [seq]
  (loop [seq seq]
    (when seq
      (let [elt (first seq)]
        (or elt (recur (next seq)))))))

(defn split-at-first
  "Splits given sequence at first element satisfing predicate.
  The first element satisfing predicate will be in the second sequence."
  [predicate sequence]
  (let [index (or (first-non-nil
                   (map #(if (predicate %1) %2)
                        sequence (iterate inc 0)))
                  (count sequence))]
    (split-at index sequence)))

(defn split-at-last
  "Splits given sequence at last element satisfing predicate.
  The last element satisfing predicate will be in the first sequence."
  [predicate sequence]
  (let [index (or (first-non-nil
                   (map #(if (predicate %1) %2)
                        (reverse sequence) (range (count sequence) 0 -1)))
                  0)]
    (split-at index sequence)))

(defn die-with-error
  "Stops program by raising the given error with strings as message."
  [^Class error, strings]
  (assert (isa? error Throwable))
  (throw (.. error
             (getConstructor (into-array Class [String]))
             (newInstance (into-array Object [(apply str strings)])))))

(defn illegal-argument
  "Throws IllegalArgumentException with given strings as message."
  [& strings]
  (die-with-error IllegalArgumentException strings))

(defn unsupported-operation
  "Throws UnsupportedOperationException with given strings as message."
  [& strings]
  (die-with-error UnsupportedOperationException strings))

(defn illegal-state
  "Throws IllegalStateException with given strings as message."
  [& strings]
  (die-with-error IllegalStateException strings))

(defmacro with-altered-vars
  "Executes the code given in a dynamic environment where the var
  roots of the given names are altered according to the given
  bindings. The bindings have the form [name_1 f_1 name_2 f_2 ...]
  where f_i is applied to the original value of the var associated
  with name_i to give the new value which will be in place during
  execution of body. The old value will be restored after execution
  has been finished."
  [bindings & body]
  (when-not (even? (count bindings))
    (illegal-argument "Bindings must be given pairwise."))
  (let [bindings (partition 2 bindings),
        bind-gen (for [[n _] bindings] [(gensym) n])]
    `(let ~(vec (mapcat (fn [[name thing]] [name `(deref (var ~thing))])
                        bind-gen))
       (try
         ~@(map (fn [[thing f]]
                  `(alter-var-root (var ~thing) ~f))
                bindings)
         ~@body
         (finally
          ~@(map (fn [[name thing]]
                   `(alter-var-root (var ~thing) (constantly ~name)))
                 bind-gen))))))

(defmacro with-var-bindings
  "Executes body with the vars in bindings set to the corresponding
  values."
  [bindings & body]
  (when-not (even? (count bindings))
    (illegal-argument "Bindings must be given pairwise."))
  `(with-altered-vars ~(vec (mapcat (fn [[a b]] `[~a (constantly ~b)])
                                   (partition 2 bindings)))
     ~@body))

(defmacro with-memoized-fns
  "Runs code in body with all given functions memoized."
  [functions & body]
  `(with-altered-vars ~(vec (mapcat (fn [f] [f `memoize]) functions))
     ~@body))

(defmacro memo-fn
  "Defines memoized, anonymous function."
  [name args & body]
  `(let [cache# (atom {})]
     (fn ~name ~args
       (if (contains? @cache# ~args)
         (@cache# ~args)
         (let [rslt# (do ~@body)]
           (swap! cache# assoc ~args rslt#)
           rslt#)))))

(defn inits
  "Returns a lazy sequence of the beginnings of sqn."
  [sqn]
  (let [runner (fn runner [init rest]
                 (if (not rest)
                   [init]
                   (lazy-seq
                     (cons init (runner (conj init (first rest))
                                        (next rest))))))]
    (runner [] (seq sqn))))

(defn tails
  "Returns a lazy sequence of the tails of sqn."
  [sqn]
  (let [runner (fn runner [rest]
                 (if (not rest)
                   [[]]
                   (lazy-seq
                     (cons (vec rest) (runner (next rest))))))]
    (runner (seq sqn))))

(defn now
  "Returns the current time in a human readable format."
  []
  (let [^Calendar cal (Calendar/getInstance),
        ^SimpleDateFormat sdf (SimpleDateFormat. "HH:mm:ss yyyy-MM-dd")]
    (.format sdf (.getTime cal))))

(defn ask
  "Performs simple quering. prompt is printed first and then the user
  is asked for an answer (via read). The other arguments are
  predicates with corresponding error messages. If a given answer does
  not satisfy some predicate pred, it's associated error message is
  printed (if it is a string) or it is assumed to be a function of one
  argument, whereupon it is called with the invalid answer and the
  result is printed. In any case, the user is asked again, until the
  given answer fulfills all given predicates."
  [prompt read & preds-and-fail-messages]
  (when-not (even? (count preds-and-fail-messages))
    (illegal-argument "Every predicate needs to have a corresponding error message."))
  (let [predicates (partition 2 preds-and-fail-messages),
        sentinel   (Object.),
        read-fn    #(try (read) (catch Throwable _ sentinel))]
    (do
      (print prompt)
      (flush)
      (loop [answer (read-fn)]
        (if-let [fail (if (identical? answer sentinel)
                        "An error occured while reading.\n",
                        (first-non-nil (map #(when-not ((first %) answer)
                                               (second %))
                                            predicates)))]
          (do
            (if (string? fail)
              (print fail)
              (print (fail answer)))
            (flush)
            (recur (read-fn)))
          answer)))))

(defn yes-or-no?
  "Asks string, expecting 'yes' or 'no'. Returns true when answered
  'yes' and false otherwise."
  [question]
  (= 'yes (ask (str question)
               #(read-string (str (read-line)))
               #{'yes 'no}
               "Please answer 'yes' or 'no': ")))

;;; deftype utilities

(defmacro generic-equals
  "Implements a generic equals for class on fields."
  [[this other] class fields]
  `(boolean (or (identical? ~this ~other)
                (when (= (class ~this) (class ~other))
                  (and ~@(map (fn [field]
                                `(= ~field (. ~(vary-meta other assoc :tag class) ~field)))
                              fields))))))

(defn hash-combine-hash
  "Combines the hashes of all things given."
  [& args]
  (reduce #(hash-combine %1 (hash %2)) 0 args))


;;; Math

(defmacro =>
  "Implements implication."
  [a b]
  `(if ~a ~b true))

(defn <=>
  "Implements equivalence."
  [a b]
  (or (and a b)
      (and (not a) (not b))))

(defn- expand-bindings
  "Expands bindings used by forall and exists."
  [bindings body]
  (if (empty? bindings)
    body
    (let [[x xs] (first bindings)]
      `(loop [ys# ~xs]
         (if (empty? ys#)
           true
           (let [~x (first ys#)]
             (and ~(expand-bindings (rest bindings) body)
                  (recur (rest ys#)))))))))

(defmacro forall
  "Implements logical forall quantor. Bindings is of the form [var-1
  seq-1 var-2 seq-2 ...]."
  [bindings condition]
  (when-not (let [c (count bindings)]
              (and (<= 0 c)
                   (zero? (mod c 2))))
    (illegal-argument "forall requires even number of bindings."))
  (expand-bindings (map vec (partition 2 bindings)) condition))

(defmacro exists
  "Implements logical exists quantor. Bindings is of the form [var-1
  seq-1 var-2 seq-2 ...]."
  [bindings condition]
  (when-not (let [c (count bindings)]
              (and (<= 0 c)
                   (zero? (mod c 2))))
    (illegal-argument "exists requires even number of bindings."))
  `(not ~(expand-bindings (map vec (partition 2 bindings)) `(not ~condition))))

(defmacro set-of
  "Macro for writing sets as mathematicians do (at least similar to
  it.) The following syntax constructions are supported:

    (set-of x [x [1 2 3]])
      for the set of all x with x in [1 2 3]

    (set-of x | x [1 2 3])
      for the same set

    (set-of [x y] [x [1 2 3], y [4 5 6] :when (= 1 (gcd x y))])
      for the set of all pairs [x y] with x in [1 2 3], y in [4 5 6]
      and x and y are coprime.

  In general, the condition vector (or the sequence after |) must be
  suitable for doseq."
  [thing & condition]
  (let [condition (if (= '| (first condition))
                    (vec (rest condition))
                    (vec (first condition)))]
    `(let [result# (atom (transient #{}))]
       (doseq ~condition
         (swap! result# conj! ~thing))
       (persistent! @result#))))

(defn div
  "Integer division."
  [a b]
  (round (Math/floor (/ a b))))

(defn distinct-by-key
  "Returns a sequence of all elements of the given sequence with distinct key values,
  where key is a function from the elements of the given sequence. If two elements
  correspond to the same key, the one is chosen which appeared earlier in the sequence.

  This function is copied from clojure.core/distinct and adapted for using a key function."
  [sequence key]
  (let [step (fn step [xs seen]
               (lazy-seq
                 ((fn [xs seen]
                    (when-let [s (seq xs)]
                      (let [f     (first xs)
                            key-f (key f)]
                        (if (contains? seen key-f)
                          (recur (rest s) seen)
                          (cons f (step (rest s) (conj seen key-f)))))))
                  xs seen)))]
    (step sequence #{})))

(defn reduce!
  "Does the same as reduce, but calls transient on the initial value
  and persistent! on the result."
  [fn initial-value coll]
  (persistent! (reduce fn (transient initial-value) coll)))

(defn map-by-fn
  "Returns a hash map with the values of keys as keys and their values
  under function as values."
  [function keys]
  (persistent! (reduce (fn [map k]
                         (assoc! map k (function k)))
                       (transient {})
                       keys)))

(defmacro with-printed-result
  "Prints string followed by result, returning it."
  [string & body]
  `(let [result# (do
                   ~@body)]
     (println ~string result#)
     result#))


;;;

nil
