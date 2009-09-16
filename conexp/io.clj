(ns conexp.io
  (:use conexp.fca.contexts
	conexp.util
	[clojure.contrib.str-utils :only (str-join)]
	[clojure.contrib.duck-streams :only (reader)]
	[clojure.set :only (union)]))

(defn illegal-argument [& strings]
  (throw (IllegalArgumentException. (apply str strings))))

(defn get-line [input-reader]
  (.readLine input-reader))

(defn get-lines [input-reader n]
  (doall (take n (repeatedly #(get-line input-reader)))))

(defmulti write-context (fn [method ctx] method))

(let [known-context-input-formats (ref {})]
  (defn add-context-input-format [name predicate]
    (dosync (alter known-context-input-formats assoc name predicate)))
  
  (defn find-context-input-format [file]
    (with-open [input-reader (reader file)]
      (let [input-lines (take-while identity (repeatedly #(.readLine input-reader)))]
	(first
	 (for [[name predicate] @known-context-input-formats
	       :when (predicate input-lines)]
	   name))))))

(defmulti read-context find-context-input-format)

(defmethod write-context :default [method _]
  (illegal-argument "Method " method " for context output is not known."))

(defmethod read-context :default [file]
  (illegal-argument "Cannot determine format of context in " file))

;;;
;;; Formats
;;;

;; Burmeister Format

(defmethod write-context :burmeister [_ ctx]
  (with-str-out
    "B\n"
    "\n"
    (count (attributes ctx)) "\n"
    (count (objects ctx))    "\n"
    "\n"
    (map #(str % "\n") (vec (attributes ctx)))
    (map #(str % "\n") (vec (objects ctx)))
    (let [inz (incidence ctx)]
      (str-join "\n"
		(map (fn [m] 
		       (str-join ""
				 (map (fn [g] 
					(if (inz [g m]) "X" "."))
				      (attributes ctx))))
		     (objects ctx))))))

(add-context-input-format :burmeister
			  (fn [input-lines]
			    (= \B (first (first input-lines)))))

(defmethod read-context :burmeister [file]
  (with-open [input (reader file)]
    (let [_                    (get-lines input 2) ; "B\n\n"

	  number-of-objects    (Integer/parseInt (get-line input))
	  number-of-attributes (Integer/parseInt (get-line input))

	  _                    (get-line input)	; "\n"

	  seq-of-objects       (get-lines input number-of-objects)
	  seq-of-attributes    (get-lines input number-of-attributes)]
      (loop [objs seq-of-objects
	     incidence #{}]
	(if (empty? objs)
	  (make-context (set seq-of-objects)
			(set seq-of-attributes)
			incidence)
	  (let [line (get-line input)]
	    (recur (rest objs)
		   (union incidence
			  (set-of [(first objs) (nth seq-of-attributes idx-m)]
				  [idx-m (range number-of-attributes)
				   :when (= \X (nth line idx-m))])))))))))
