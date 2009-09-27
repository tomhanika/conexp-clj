(ns conexp.util)

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
  [sequence predicate]
  (let [index (or (first-non-nil
		   (map #(if (predicate %1) %2)
			sequence (iterate inc 0)))
		  (count sequence))]
    (split-at index sequence)))

(defn split-at-last
  "Splits given sequence at last element satisfing predicate.
  The last element satisfing predicate will be in the first sequence."
  [sequence predicate]
  (let [index (or (first-non-nil
		   (map #(if (predicate %1) %2)
			(reverse sequence) (range (count sequence) 0 -1)))
		  0)]
    (split-at index sequence)))

(defn illegal-argument
  "Throws IllegalArgumentException with given strings as message"
  [& strings]
  (throw (IllegalArgumentException. (apply str strings))))


;;; Math

(defn subset?
  "Returns true iff set-1 \\subseteq set-2."
  [set-1 set-2]
  (every? #(set-2 %) set-1))

(defn proper-subset?
  "Returns true iff (not= set-1 set-2) and set-1 \\subseteq set-2."
  [set-1 set-2]
  (and (not= set-1 set-2)
       (subset? set-1 set-2)))

(defmacro =>
  "Implements implication."
  [a b]
  `(if ~a ~b true))

(defmacro <=>
  "Implements equivalence."
  [a b]
  `(or (and ~a ~b)
       (and (not ~a) (not ~b))))

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

(defn subelts 
  "Returns a subsequence of G until i is reached."
  ;; better implementation with take-while?
  [G i]
  (if (or (empty? G) (= (first G) i))
    (empty G)
    (conj (subelts (rest G) i) 
	  (first G))))
