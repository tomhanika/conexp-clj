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

(defmacro forall [bindings condition]
  `(every? identity
	   (for ~bindings ~condition)))

(defmacro set-of [thing condition]
  `(set (for ~condition ~thing)))