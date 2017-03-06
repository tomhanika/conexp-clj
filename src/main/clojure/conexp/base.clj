;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.base
  "Basic definitions for conexp-clj."
  (:require [clojure.math.combinatorics :as comb]
            [clojure.java.io            :as io]))

;;; def macros, inspired and partially copied from clojure.contrib.def

(defmacro def-
  "Same as def, but yields a private definition"
  [name & decls]
  (list* `def (with-meta name (assoc (meta name) :private true)) decls))

(defmacro defalias
  "Defines an alias for a var: a new var with the same root binding (if
any) and similar metadata. The metadata of the alias is its initial
metadata (as provided by def) merged into the metadata of the original."
  ([name orig]
     `(do
        (alter-meta!
         (if (.hasRoot (var ~orig))
           (def ~name (.getRawRoot (var ~orig)))
           (def ~name))
         ;; When copying metadata, disregard {:macro false}.
         ;; Workaround for http://www.assembla.com/spaces/clojure/tickets/273
         #(conj (dissoc % :macro)
                (apply dissoc (meta (var ~orig)) (remove #{:macro} (keys %)))))
        (var ~name)))
  ([name orig doc]
     (list `defalias (with-meta name (assoc (meta name) :doc doc)) orig)))

;;; Namespace tools

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
           'clojure.math.numeric-tower)

;;; Version

;; https://stackoverflow.com/questions/11235445/embed-version-string-from-leiningen-project-in-application
(def- internal-version-string
  (or (System/getProperty "conexp-clj.version")
      (-> (doto (java.util.Properties.)
            (.load (-> "META-INF/maven/conexp-clj/conexp-clj/pom.properties"
                       (io/resource)
                       (io/reader))))
          (.get "version"))))

(def- conexp-version-map
  (let [[_ major minor patch qualifier] (re-find #"(\d+)\.(\d+)\.(\d+)(?:-(.+))?"
                                                 internal-version-string)]
    {:major ^int (Integer/parseInt major),
     :minor ^int (Integer/parseInt minor),
     :patch ^int (Integer/parseInt patch),
     :qualifier qualifier}))

(defn conexp-version
  "Returns the version of conexp as a string."
  []
  (let [{:keys [major minor patch qualifier]} conexp-version-map]
    (str major "." minor "." patch (when qualifier (str "-" qualifier)))))

(defn has-version?
  "Compares given version of conexp and returns true if and only if
  the current version of conexp is higher or equal than the given one"
  [{^int my-major :major, ^int my-minor :minor, ^int my-patch :patch}]
  (assert (and my-major my-minor my-patch))
  (let [{:keys [^int major, ^int minor, ^int patch]} conexp-version-map]
    (or (and (< my-major major))
        (and (= my-major major)
             (< my-minor minor))
        (and (= my-major major)
             (= my-minor minor)
             (< my-patch patch)))))

;;; Testing

(require 'clojure.test)

(defn test-conexp
  "Runs tests for conexp-clj."
  ([]
   (let [nss '[conexp.tests.base
               conexp.tests.math.optimize
               conexp.tests.math.statistics
               conexp.tests.fca.contexts
               conexp.tests.fca.exploration
               conexp.tests.fca.implications
               conexp.tests.fca.lattices
               conexp.tests.fca.many-valued-contexts
               conexp.tests.fca.misc
               conexp.tests.io.contexts
               conexp.tests.io.lattices
               conexp.tests.io.layouts
               conexp.tests.io.many-valued-contexts
               conexp.tests.io.util
               conexp.tests.layouts.base
               conexp.tests.layouts.common
               conexp.tests.layouts.force
               conexp.tests.layouts.freese
               conexp.tests.layouts.layered
               conexp.tests.layouts.util
               conexp.contrib.tests.algorithms
               conexp.contrib.tests.algorithms.concepts
               conexp.contrib.tests.algorithms.generators
               conexp.contrib.tests.algorithms.titanic
               conexp.contrib.tests.factor-analysis
               conexp.contrib.tests.fuzzy.sets
               conexp.contrib.tests.retracts
               conexp.contrib.tests.util.general-sorted-sets]]
     (apply require nss)
     (apply clojure.test/run-tests nss))))

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
                           (clojure.test/is false)))
                   (clojure.test/is true))))
            body)))

;;; Types

(def clojure-set clojure.lang.PersistentHashSet)
(def clojure-fn  clojure.lang.Fn)
(def clojure-seq clojure.lang.Sequential)
(def clojure-vec clojure.lang.PersistentVector)
(def clojure-map clojure.lang.Associative)
(def clojure-coll clojure.lang.IPersistentCollection)

(defn clojure-type
  "Dispatch function for multimethods."
  [thing]
  (class thing))


;;; Technical Helpers

(defn quit
  "Quits conexp-clj."
  []
  (System/exit 0))

(defmacro defmacro-                     ; from clojure.core.incubator
  "Same as defmacro but yields a private definition"
  [name & decls]
  (list* `defmacro (with-meta name (assoc (meta name) :private true)) decls))

(defn seqable?                          ; from clojure.core.incubator
  "Returns true if (seq x) will succeed, false otherwise."
  [x]
  (or (seq? x)
      (instance? clojure.lang.Seqable x)
      (nil? x)
      (instance? Iterable x)
      (-> ^Object x .getClass .isArray)
      (string? x)
      (instance? java.util.Map x)))

(defn proper-subset?
  "Returns true iff set-1 is a proper subset of set-2."
  [set-1 set-2]
  (and (not= set-1 set-2)
       (subset? set-1 set-2)))

(defn proper-superset?
  "Returns true iff set-1 is a proper superset of set-2."
  [set-1 set-2]
  (proper-subset? set-2 set-1))

(defn singleton?
  "Returns true iff given thing is a singleton sequence or set."
  [x]
  (and (or (set? x) (sequential? x))
       (not (empty? x))
       (not (next x))))

(defn ensure-length
  "Fills given string with padding to have at least the given length."
  ([string, ^long length]
   (ensure-length string length " "))
  ([string, ^long length, padding]
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

(defn order-by
  "Returns a function on two arguments a and b that returns true if and only if
  b occurs not after a in the given sequence (ascending order.)"
  [sequence]
  (let [pos (zipmap sequence (range))]
    (fn [a b]
      (and (contains? pos a)
           (contains? pos b)
           (>= ^long (pos a) ^long (pos b))))))
(defn zip
  "Returns sequence of pairs [x,y] where x runs through seq-1 and
  y runs through seq-2 simultaneously. This is the same as
  (map #(vector %1 %2) seq-1 seq-2)."
  [seq-1 seq-2]
  (map #(vector %1 %2) seq-1 seq-2))

(defn first-position-if
  "Return the index of the first element in sequence for which predicate returns
  true."
  [predicate sequence]
  (loop [index    0
         sequence sequence]
    (cond
      (not (seq sequence))
      nil
      (predicate (first sequence))
      index
      :else
      (recur (inc index) (rest sequence)))))

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

(defn warn
  "Emits a warning message on *out*."
  [message]
  (println "WARNING:" message))

(defmacro die-with-error
  "Stops program by raising the given error with strings as message."
  [^Class error, strings]
  `(throw (new ~error ^String (apply str ~strings))))

(defn illegal-argument
  "Throws IllegalArgumentException with given strings as message."
  [& strings]
  (die-with-error IllegalArgumentException strings))

(defn unsupported-operation
  "Throws UnsupportedOperationException with given strings as message."
  [& strings]
  (die-with-error UnsupportedOperationException strings))

(defn not-yet-implemented
  "Throws UnsupportedOperationException with \"Not yet implemented\"
  message."
  []
  (unsupported-operation "Not yet implemented"))

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

(import 'java.util.Calendar
        'java.text.SimpleDateFormat)

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
    (illegal-argument
     "Every predicate needs to have a corresponding error message."))
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

(defmacro dopar
  "Executes body binding k to each value in v.
  Execution is done in parallel.  Code adapted from
  http://www.acooke.org/cute/Clojuremac0.html"
  [[k v] & body]
  `(apply await
          (for [k# ~v]
            (let [a# (agent k#)]
              (send a# (fn [~k] ~@body))
              a#))))

;;; deftype utilities

(defmacro generic-equals
  "Implements a generic equals for class on fields."
  [[this other] class fields]
  `(boolean
    (or (identical? ~this ~other)
        (when (= (class ~this) (class ~other))
          (and ~@(map (fn [field]
                        `(= ~field
                            (. ~(vary-meta other assoc :tag class) ~field)))
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
  seq-1 var-2 seq-2 ...]. Returns boolean value."
  [bindings condition]
  (when-not (even? (count bindings))
    (illegal-argument "forall requires even number of bindings."))
  `(boolean ~(expand-bindings (map vec (partition 2 bindings)) condition)))

(defmacro exists
  "Implements logical exists quantor. Bindings is of the form [var-1
  seq-1 var-2 seq-2 ...]. Returns boolean value."
  [bindings condition]
  (when-not (even? (count bindings))
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

(defmacro sum
  "Computes the sum of expr for indices from start to end, named as
  index."
  [index start end & expr]
  `(reduce (fn [sum# ~index]
             (+ sum#
                (do ~@expr)))
           0N
           (range ~start (inc ~end))))

(defmacro prod
  "Computes the product of expr for indices from start to end, named
  as index."
  [index start end & expr]
  `(reduce (fn [prod# ~index]
             (* prod#
                (do ~@expr)))
           1N
           (range ~start (inc ~end))))

(defn expt
  "Exponentiation of arguments. Is exact if given arguments are exact
  and returns double otherwise."
  [a b]
  (if (not (and (integer? a) (integer? b)))
    (clojure.math.numeric-tower/expt a b)
    (let [^clojure.lang.BigInt a (bigint a),
          b (long b)]
      (if (< b 0)
        (/ (expt a (- b)))
        (loop [result 1N,
               aktpot a,
               power  b]
          (if (zero? power)
            result
            (if (zero? (Long/numberOfTrailingZeros power))
              ;; power is odd
              (recur (.multiply result aktpot)
                     (.multiply aktpot aktpot)
                     (long (/ (dec power) 2)))
              ;; power is even
              (recur result
                     (.multiply aktpot aktpot)
                     ;; divide power by two
                     (long (/ power 2))))))))))

(defn distinct-by-key
  "Returns a sequence of all elements of the given sequence with distinct key
  values, where key is a function from the elements of the given sequence. If
  two elements correspond to the same key, the one is chosen which appeared
  earlier in the sequence.

  This function is copied from clojure.core/distinct and adapted for using a key
  function."
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
  (reduce! (fn [map k]
             (assoc! map k (function k)))
           {}
           keys))

(defmacro with-printed-result
  "Prints string followed by result, returning it."
  [string & body]
  `(let [result# (do
                   ~@body)]
     (println ~string result#)
     result#))

(defn ensure-seq
  "Given anything that can be made a sequence from, returns that
  sequence. If given a number x, returns (range x)."
  [x]
  (cond
   (number? x) (range x),
   (or (nil? x)
       (instance? java.util.Collection x)
       (.isArray ^Class (class x)))
   (or (seq x) ())
   :otherwise
   (illegal-argument "Cannot generate meaningful sequence from " x ".")))

(defn topological-sort
  "Returns a linear extension of the given collection coll and the
  supplied comparator comp."
  [comp coll]
  (let [dependencies (map-by-fn (fn [x]
                                  (set (filter #(and (comp % x)
                                                     (not= % x))
                                               coll)))
                                coll)]
    (loop [dependencies dependencies,
           sorted       (transient [])]
      (if (empty? dependencies)
        (persistent! sorted)
        (let [nexts (set (filter #(empty? (dependencies %))
                                 (keys dependencies)))]
          (if (empty? nexts)
            (illegal-argument "Cyclic comparator given.")
            (recur (reduce (fn [map x]
                             (if (contains? nexts x)
                               (dissoc map x)
                               (update-in map [x] difference nexts)))
                           dependencies
                           (keys dependencies))
                   (reduce conj! sorted nexts))))))))


;;; Searching for minimum covers

(defn- covers?
  "Technical Helper. Tests wheterh all elements in base-set are contained at
  least one set in sets."
  [sets base-set]
  (if (empty? base-set)
    true
    (loop [rest (transient base-set),
           sets sets]
      (if-not sets
        false
        (let [new-rest (reduce disj! rest (first sets))]
          (if (zero? (count new-rest))
            true
            (recur new-rest (next sets))))))))

(defn- redundant?
  "Technical Helper. For a given set base-set, a collection cover of sets and a
  map mapping elements from base-set to the number of times they occur in sets
  in cover, tests whether the cover is redundant or not, i.e. if a proper
  subcollection of cover is already a cover or not."
  [base-set cover count]
  (exists [set cover]
    (forall [x set]
      (=> (contains? base-set x)
          (<= 2 ^long (get count x))))))

(defn minimum-set-covers
  "For a given set base-set and a collection of sets returns all subcollections
  of sets such that the union of the contained sets cover base-set and that are
  minimal with that property."
  [base-set sets]
  (let [result  (atom []),
        search  (fn search [rest-base-set current-cover cover-count sets]
                  (cond
                   (redundant? base-set current-cover cover-count)
                   nil,

                   (empty? rest-base-set)
                   (swap! result conj current-cover),

                   (empty? sets)
                   nil,

                   :else
                   (when (covers? sets rest-base-set)
                     (let [counts (map-by-fn
                                   #(count (intersection rest-base-set %))
                                   sets),
                           ;; sort sets in order of decreasing cardinality
                           sets   (sort-by #(- ^long (counts %)) sets)],
                       (search (difference rest-base-set (first sets))
                               (conj current-cover (first sets))
                               (reduce! (fn [map x]
                                          (if (contains? base-set x)
                                            (assoc! map x (inc (long (get map x))))
                                            map))
                                        cover-count
                                        (first sets))
                               (rest sets))
                       (search rest-base-set
                               current-cover
                               cover-count
                               (rest sets))))))]
    (search base-set
            #{}
            (map-by-fn (constantly 0) base-set)
            sets)
    @result))


;;; Very Basic Set Theory

(defn cross-product
  "Returns cross product of set-1 and set-2."
  [& sets]
  (if (empty? sets)
    #{[]}
    (set-of (conj t x) [t (apply cross-product (butlast sets))
                        x (last sets)])))

(defn disjoint-union
  "Computes the disjoint union of sets by joining the cross-products of the
  sets with natural numbers."
  [& sets]
  (apply union (map #(cross-product %1 #{%2}) sets (iterate inc 0))))

(defn set-of-range
  "Returns a set of all numbers from start upto (but not including) end,
  by step if provided."
  ([end]
     (set-of-range 0 end 1))
  ([start end]
     (set-of-range start end 1))
  ([start end step]
     (set (range start end step))))

(defn to-set
  "Converts given argument «thing» to a set. If it is a number,
  returns the set {0, ..., thing-1}. If it is a collection,
  returns (set thing). Otherwise raises an error."
  [thing]
  (cond
   (integer? thing) (set-of-range thing),
   (seqable? thing) (set thing),
   :else            (illegal-argument "Cannot create set from " thing)))

(defn subsets
  "Returns all subsets of the given base-set."
  [base-set]
  (map set (comb/subsets (seq base-set))))


;;; Next Closure

(defn lectic-<_i
  "Implements lectic < at position i. The basic order is given by the ordering
  of base which is interpreted as increasing order.

  A and B have to be sets."
  [base i A B]
  (and (contains? B i) (not (contains? A i)) ; A and B will always be sets
       (loop [elements base]
         (let [j (first elements)]
           (if (= j i)
             true
             (if (identical? (contains? B j) (contains? A j))
               (recur (rest elements))
               false))))))

(defn lectic-<
  "Implements lectic ordering. The basic order is given by the ordering of base
  which is interpreted as increasing order."
  [base A B]
  (exists [i base] (lectic-<_i base i A B)))

(defn next-closed-set-in-family
  "Computes next closed set as with next-closed-set, which is in the
  family F of all closed sets satisfing predicate. predicate has to
  satisfy the condition

    A in F and i in base ==> clop(A union {1, ..., i-1}) in F.
  "
  [predicate base clop A]
  (loop [i-s (reverse base),
         A   (set A)]
    (if (empty? i-s)
      nil
      (let [i (first i-s)]
        (if (contains? A i)
          (recur (rest i-s) (disj A i))
          (let [clop-A (clop (conj A i))]
            (if (and (lectic-<_i base i A clop-A)
                     (predicate clop-A))
              clop-A
              (recur (rest i-s) A))))))))

(defn improve-basic-order
  "Improves basic order on the sequence base, where the closure operator
  clop operates on."
  [base clop]
  (let [base (seq base),
        clop (memoize clop)]
    (sort (fn [x y]
            (if (= (clop #{y}) (clop #{x}))
              0
              (if (lectic-< base (clop #{y}) (clop #{x}))
                -1
                1)))
          base)))

(defn all-closed-sets-in-family
  "Computes all closed sets of a given closure operator on a given set
  base contained in the family described by predicate. See
  documentation of next-closed-set-in-family for more details. Uses
  initial as first closed set if supplied."
  ([predicate base clop]
     (all-closed-sets-in-family predicate base clop #{}))
  ([predicate base clop initial]
     (lazy-seq
      (let [base    (if (set? base) (improve-basic-order base clop) base),
            initial (clop initial),
            start   (if (predicate initial)
                      initial
                      (next-closed-set-in-family predicate base clop initial)),
            runner  (fn runner [X]
                      (lazy-seq
                       (if (nil? X)
                         nil
                         (cons X (runner (next-closed-set-in-family predicate base clop X))))))]
        (runner start)))))

(defn next-closed-set
  "Computes next closed set of the closure operator clop after A with
  the Next Closure algorithm. The order of elements in base,
  interpreted as increasing, is taken to be the basic order of the
  elements."
  [base clop A]
  (next-closed-set-in-family (constantly true) base clop A))

(defn all-closed-sets
  "Computes all closed sets of a given closure operator on a given
  set. Uses initial as first closed set if supplied."
  ([base clop]
     (all-closed-sets base clop #{}))
  ([base clop initial]
     (all-closed-sets-in-family (constantly true) base clop initial)))

(defn parallel-closures
  "Returns the set of all closures of the closure operator on the given base set.
  Computes the closures in parallel, to the extent possible."
  [base clop]
  (loop [n        0
         closures #{(clop #{})}
         current  #{(clop #{})}]
    (if (< (count base) n)
      closures
      (let [next-current (atom current)]
        (dopar [C current]
          (when (not= (count C) n)
            (swap! next-current #(disj % C))
            (doseq [x base :when (not (contains? C x))]
              (swap! next-current #(conj % (clop (conj C x)))))))
        (recur (inc n) (into closures @next-current) @next-current)))))


;;; Common Math Algorithms

(defn transitive-closure
  "Computes transitive closure of a given set of pairs."
  ;; Inspired by the corresponding code from the graph library by
  ;; Jeffrey Straszheim
  [pairs]
  (let [pairs-as-map (loop [pairs pairs
                            map   {}]
                       (if (not (seq pairs))
                         map
                         (let [[x y] (first pairs)]
                           (recur (rest pairs)
                                  (update map x conj y)))))
        runner (fn runner [to-be-visited already-visited]
                 (lazy-seq
                  (let [not-yet-visited (seq (drop-while #(contains? already-visited %)
                                                         to-be-visited))
                        unseen-node     (first not-yet-visited)]
                    (when (seq not-yet-visited)
                      (cons unseen-node
                            (runner (concat (get pairs-as-map unseen-node)
                                            (rest not-yet-visited))
                                    (conj already-visited
                                          unseen-node)))))))]
    (set-of [x y] [x (keys pairs-as-map)
                   y (runner (get pairs-as-map x) #{})])))

(defn reflexive-transitive-closure
  "Computes the reflexive, transitive closure of a given set of pairs
  on base-set."
  [base-set pairs]
  (transitive-closure (union (set pairs)
                             (set-of [x x] [x base-set]))))

(defn transitive-reduction
  "Returns for a set of pairs its transitive reduction. Alternatively,
  the relation can be given as a base set and a predicate p which
  returns true in (p x y) iff [x y] is in the relation in question.

  Note that if the relation given is not acyclic, the transitive
  closure of the reduction may not yield the transitive closure of the
  original relation anymore, since the reduction itself can be empty."
  ([pairs]
     (let [result (atom (transient #{}))]
       (doseq [[x y] pairs]
         (when (not (exists [[a b] pairs,
                             [c d] pairs]
                      (and (= a x)
                           (= b c)
                           (= d y))))
           (swap! result conj! [x y])))
       (persistent! @result)))
  ([base pred]
     (let [result (atom (transient #{}))]
       (doseq [x base, y base]
         (when (and (pred x y)
                    (not (exists [z base]
                           (and (not= x z)
                                (not= z y)
                                (pred x z)
                                (pred z y)))))
           (swap! result conj! [x y])))
       (persistent! @result))))

(defn graph-of-function?
  "Returns true iff relation is the graph of a function from source to target."
  [relation source target]
  (and (= (set-of x [[x y] relation]) source)
       (subset? (set-of y [[x y] relation]) target)
       (= (count source) (count relation))))

(defn minimal-generating-subsets
  "Given a set A and a closure operator clop returns all subsets B of
  A such that (= (clop A) (clop B))."
  [clop A]
  (let [clop-A (clop A)]
    (loop [left     [A],                ;yet to consider
           minimals []]                 ;minimal elements already found
      (if (empty? left)
        (distinct minimals)
        (let [next (first left)]
          (if (empty? next)
            (recur (rest left) (conj minimals next))
            (let [generating-subsets (set-of X [x next,
                                                :let [X (disj next x)]
                                                :when (= clop-A (clop X))])]
              (if (empty? generating-subsets)
                (recur (rest left) (conj minimals next))
                (recur (into (rest left) generating-subsets) minimals)))))))))

(defn partial-min
  "For a given partial order <= and given elements returns the minimal
  among them."
  [<= xs]
  (let [runner (fn runner [left minimals]
                 (if (empty? left)
                   minimals
                   (let [next (first left),
                         new-minimals (remove #(<= next %) minimals)]
                     (if (not= (count minimals) (count new-minimals))
                       (recur (rest left) (conj new-minimals next))
                       (if (some #(<= % next) minimals)
                         (recur (rest left) minimals)
                         (recur (rest left) (conj minimals next)))))))]
    (runner xs ())))

(defn partial-max
  "For a given partial order <= and given elements returns the maximal
  among them."
  [<= xs]
  (partial-min #(<= %2 %1) xs))

;;; Hypergraph Transversals

(defn- intersection-set?
  "Tests whether set has non-empty intersection with every set in sets."
  [set sets]
  (forall [other-set sets]
    (exists [x set]
      (contains? other-set x))))

(defn minimal-hypergraph-transversals
  "Returns all minimal hypergraph transversals of the hypergraph defined by
  «edges» on the vertex sets «vertices»."
  [vertices edges]
  (let [cards    (map-by-fn (fn [x]
                              (count (set-of X | X edges :when (contains? X x))))
                            vertices),
        elements (sort #(compare (cards %2) (cards %1))
                       vertices)]
    ;; What follows is rather complicated, but corresponds roughtly to the lazy
    ;; version of the following code:
    ;;
    ;;  (let [result   (atom []),
    ;;        search   (fn search [rest-sets current rest-elements]
    ;;                   (cond
    ;;                    (exists [x current]
    ;;                      (intersection-set? (disj current x) edges))
    ;;                    nil,
    ;;                    (intersection-set? current edges)
    ;;                    (swap! result conj current),
    ;;                    :else
    ;;                    (when-let [x (first rest-elements)]
    ;;                      (when (exists [set rest-sets]
    ;;                              (contains? set x))
    ;;                        (search (remove #(contains? % x) rest-sets)
    ;;                                (conj current x)
    ;;                                (rest rest-elements)))
    ;;                      (search rest-sets
    ;;                              current
    ;;                              (rest rest-elements)))))]
    ;;    (search edges #{} elements)
    ;;    @result)
    (let [search (fn search [stack]
                   (when-let [[rest-sets current-hitting-set rest-elements instruction] (first stack)]
                     (lazy-seq
                      (cond
                        ;; start at the beginning
                        (= instruction 'beginning)
                        (cond
                          ;; discard if current hitting-set candidate is not minimal
                          (exists [x current-hitting-set]
                            (intersection-set? (disj current-hitting-set x) edges))
                          (search (rest stack))

                          ;; if we have a (minimal) hitting-set, then we just return it
                          (intersection-set? current-hitting-set edges)
                          (cons current-hitting-set
                                (search (rest stack)))

                          ;; otherwise we try to extend our current candidate
                          :else
                          (if-let [x (first rest-elements)]
                            (if (exists [set rest-sets]
                                  (contains? set x))
                              (search (conj (rest stack)
                                            [rest-sets
                                             current-hitting-set
                                             rest-elements
                                             'first-case]
                                            [(remove #(contains? % x) rest-sets)
                                             (conj current-hitting-set x)
                                             (rest rest-elements)
                                             'beginning]))
                              (search (conj (rest stack)
                                            [rest-sets
                                             current-hitting-set
                                             rest-elements
                                             'second-case]
                                            [rest-sets
                                             current-hitting-set
                                             (rest rest-elements)
                                             'beginning])))
                            (search (rest stack))))

                        ;; entered `search' by returning from the first recursive call
                        (= instruction 'first-case)
                        (search (conj (rest stack)
                                      [rest-sets
                                       current-hitting-set
                                       rest-elements
                                       'second-case]
                                      [rest-sets
                                       current-hitting-set
                                       (rest rest-elements)
                                       'beginning]))

                        ;; entered `search' by returning from the second recursive call
                        (= instruction 'second-case)
                        (search (rest stack))))))]
      (search (list [edges #{} elements 'beginning])))))

;;;

nil
