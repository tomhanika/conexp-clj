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
    `(update-ns-meta! ~ns
       :doc ~doc)))

;;; Testing

(defmacro tests-to-run
  "Defines tests to run when the namespace in which this macro is
  called is tested by test-ns."
  [& namespaces]
  `(defn ~'test-ns-hook []
     (dosync
      (ref-set *report-counters*
	       (merge-with + ~@(map (fn [ns]
				      `(do
					 (require '~ns)
					 (test-ns '~ns)))
				    namespaces))))))

(defmacro with-testing-data
  "Expects for all bindings the body to be evaluated to true. bindings
  must be suitable for forall."
  [bindings & body]
  `(forall ~bindings
     ~@(map (fn [expr] `(is ~expr))
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

(defn zip
  "Returns sequence of pairs [x,y] where x runs through seq-1 and
  y runs through seq-2 simultaneously. This is the same as
  (map #(vector %1 %2) seq-1 seq-2)."
  [seq-1 seq-2]
  (map #(vector %1 %2) seq-1 seq-2))

(defn first-non-nil
  "Returns first non-nil element in seq."
  [seq]
  (first (drop-while #(= nil %) seq)))

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

(defn- die-with-error
  "Stops program by raising the given error with strings as message."
  [^Throwable error, strings]
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

(defmacro with-memoized-fns
  "Runs code in body with all functions in functions memoized."
  [functions & body]
  `(binding ~(vec (interleave functions
			      (map (fn [f] `(memoize ~f)) functions)))
     ~@body))

(defmacro memo-fn
  "Defines memoized, anonymous function."
  [name args & body]
  `(let [cache# (ref {})]
     (fn ~name ~args
       (if (contains? @cache# ~args)
	 (@cache# ~args)
	 (let [rslt# (do ~@body)]
	   (dosync
	    (alter cache# assoc ~args rslt#))
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
    (runner [] sqn)))

(defn tails
  "Returns a lazy sequence of the tails of sqn."
  [sqn]
  (let [runner (fn runner [rest]
		 (if (not rest)
		   [[]]
		   (lazy-seq
		     (cons (vec rest) (runner (next rest))))))]
    (runner sqn)))

(defn now
  "Returns the current time in a human readable format."
  []
  (let [#^Calendar cal (Calendar/getInstance),
        #^SimpleDateFormat sdf (SimpleDateFormat. "HH:mm:ss yyyy-MM-dd")]
    (.format sdf (.getTime cal))))

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
  "Macro for writing sets as mathematicians do (at least similar to it.)"
  [thing condition]
  `(with-local-vars [set# (transient #{})]
     (doseq ~condition
       (var-set set# (conj! (var-get set#) ~thing)))
     (persistent! (var-get set#))))

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


;;; Swings

(defn get-root-cause
  "Returns original message of first exception causing the given one."
  [#^Throwable exception]
  (if-let [cause (.getCause exception)]
    (get-root-cause cause)
    (.getMessage exception)))

(defmacro with-swing-error-msg
  "Runs given code and catches any thrown exception, which is then
  displayed in a message dialog."
  [frame title & body]
  `(try
    ~@body
    (catch Exception e#
      (javax.swing.JOptionPane/showMessageDialog ~frame
						 (apply str (get-root-cause e#) "\n"
                                                        (interpose "\n" (.getStackTrace e#)))
						 ~title
						 javax.swing.JOptionPane/ERROR_MESSAGE))))

;;;

nil
