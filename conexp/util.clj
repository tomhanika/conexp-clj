(ns conexp.util)

(defn ensure-length
  ([string length]
     (ensure-length string length " "))

  ([string length padding]
     (apply str string (repeat (- length (count string)) padding))))

(defn flatten [in]
  (cond
    (sequential? in) (apply concat (map flatten in))
    :else (seq [in])))

(defn with-str-out [& body]
  (with-out-str 
    (doseq [element (flatten body)]
      (print element))))

(defn subset? [set-1 set-2]
  (every? #(set-2 %) set-1))

(defn proper-subset? [set-1 set-2]
  (and (not= set-1 set-2)
       (subset? set-1 set-2)))

(defmacro => [a b]
  `(if ~a ~b true))

(defmacro <=> [a b]
  `(or (and ~a ~b)
       (and (not ~a) (not ~b))))

(defmacro forall [bindings condition]
  `(every? identity
	   (for ~bindings ~condition)))

(defmacro exists [bindings condition]
  `(or (some identity
	     (for ~bindings ~condition))
       false))

(defmacro set-of [thing condition]
  `(set (for ~condition ~thing)))

(defn zip [seq-1 seq-2]
  (if (or (empty? seq-1)
	  (empty? seq-2))
    (empty seq-1)
    (lazy-seq
      (cons [(first seq-1) (first seq-2)]
	    (zip (rest seq-1) (rest seq-2))))))

(defn subelts 
  "Returns a subsequence of G until i is reached."
  [G i]
  (if (or (empty? G) (= (first G) i))
    (empty G)
    (conj (subelts (rest G) i) 
	  (first G))))

(defn illegal-argument [& strings]
  (throw (IllegalArgumentException. (apply str strings))))

(defn first-non-nil [seq]
  (first (drop-while #(= nil %) seq)))