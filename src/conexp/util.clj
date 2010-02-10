(ns conexp.util
  (:use clojure.contrib.profile
	[clojure.contrib.math :only (round)]))

;;; Compilation

(defn compile-conexp
  "Compiles Java classes needed for conexp-clj."
  []
  (compile 'conexp.fca.contexts)
  (compile 'conexp.fca.implications)
  (compile 'conexp.fca.association-rules)
  (compile 'conexp.fca.lattices)
  (compile 'conexp.fca.many-valued-contexts)
  (compile 'conexp.gui.repl)
  nil)

;;; Types

; work over this and make it more flexible
; we may need our own hierachy?

(defn math-type
  "Dispatch function for multimethods. Identifies sets and sequences
  as :conexp.util/set and functions as :conexp.util/fn, all other as
  :conexp.util/other."
  [thing]
  (cond
    (or (map? thing)
        (fn? thing))         ::fn
    (or (set? thing)
	(sequential? thing)) ::set
    :else                    ::other))

;;; Technical Helpers

(defn ensure-length
  "Fills given string with padding to have at least the given length."
  ([string length]
     (ensure-length string length " "))

  ([string length padding]
     (apply str string (repeat (- length (count string)) padding))))

(defn flatten
  "Flattens in to be a sequence of depth 1 at most."
  [in]
  (cond
    (sequential? in) (apply concat (map flatten in))
    :else (seq [in])))

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

(defn illegal-argument
  "Throws IllegalArgumentException with given strings as message"
  [& strings]
  (throw (IllegalArgumentException. #^String (apply str strings))))

(defmacro with-profiled-fns [fns & body]
  `(binding ~(vec (apply concat
			 (for [fn (distinct fns)]
			   `[~fn (let [orig-fn# ~fn]
				   (fn [& args#]
				     (prof ~(keyword fn)
				       (apply orig-fn# args#))))])))
     (let [data# (with-profile-data ~@body)]
       (if (not (empty? data#))
	 (print-summary (summarize data#))))))

(defmacro memo-fn [name args & body]
  `(let [cache# (ref {})]
     (fn ~name ~args
       (if (contains? @cache# ~args)
	 (@cache# ~args)
	 (let [rslt# (do ~@body)]
	   (dosync
	    (alter cache# assoc ~args rslt#))
	   rslt#)))))

(defmacro recur-sequence [& things]
  (let [initials (vec (butlast things))
	recur-fn (last things)]
    `(let [initials# (lazy-seq ~initials)]
       (concat initials#
	       ((fn step# [n# old-vals#]
		  (lazy-seq
		    (let [new-val# (apply ~recur-fn n# old-vals#)]
		      (cons new-val# (step# (inc n#) (concat (rest old-vals#) (list new-val#)))))))
		~(count initials) initials#)))))

(defmacro with-recur-seqs [seq-definitions & body]
  (let [seq-names (take-nth 2 seq-definitions)
	seq-defs  (take-nth 2 (rest seq-definitions))]
    `(let [~@(reduce concat (map (fn [seq-name] `(~seq-name (ref []))) seq-names))]
       (dosync
	~@(map (fn [name def] `(ref-set ~name (recur-sequence ~@def))) seq-names seq-defs))
       ~@body)))

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

(defmacro forall
  "Implements logical forall quantor."
  [bindings condition]
  `(every? identity
	   (for ~bindings ~condition)))

(defmacro exists
  "Implements logical exists quantor."
  [bindings condition]
  `(or (some identity
	     (for ~bindings ~condition))
       false))

(defmacro set-of
  "Macro for writing sets as mathematicians do (at least similar to it.)"
  [thing condition]
  `(set (for ~condition ~thing)))

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

(defn hashmap-by-function
  "Returns a hash map with the values of keys as keys and their values
  under function as values."
  [function keys]
  (reduce (fn [map k]
	    (assoc map k (function k)))
	  {}
	  keys))

(defmacro update-ns-meta!
  "Updates meta hash of given namespace with given description."
  [ns & key-value-description]
  `(alter-meta! (find-ns '~ns)
		(fn [meta-hash#]
		  (merge meta-hash# ~(apply hash-map key-value-description)))))

(defn hash-from-attributes
  "Computes a hash value from the values returned from funs applied to this."
  [this funs]
  (reduce bit-xor 0 (map #(%1 this) funs)))

;;;

nil
