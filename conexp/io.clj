(ns conexp.io
  (:use conexp.fca.contexts
	conexp.util
	[clojure.contrib.str-utils :only (str-join)]
	[clojure.contrib.duck-streams :only (reader)]))

(defn illegal-argument [& strings]
  (throw (IllegalArgumentException. (apply str strings))))

(defmulti write-context (fn [method ctx] method))

(let [known-context-input-formats (ref {})]
  (defn add-context-input-format [name predicate]
    (dosync (alter known-context-input-formats assoc name predicate)))
  
  (defn find-context-input-format [file]
    (first (for [[name predicate] @known-context-input-formats :when (predicate file)]
	     name))))

(defmulti read-context find-context-input-format)

(defmethod write-context :default [method _]
  (illegal-argument "Method " method " for context output is not known."))

(defmethod read-context :default [file]
  (illegal-argument "Cannot determind format of context in " file))


;;; Formats


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
			  (fn [file]
			    (let [first-character (first (.readLine (reader file)))]
			      (= \B first-character))))

(defmethod read-context :burmeister [file] ; this implementation is bad, reads in the whole file
  (let [input (reader file)
	input-stream (take-while identity (repeatedly #(.readLine input)))]
    (do
      (let [number-of-objects    (Integer/parseInt (nth input-stream 2))
	    number-of-attributes (Integer/parseInt (nth input-stream 3))

	    seq-of-objects       (map #(nth input-stream %) (range 5 (+ 5 number-of-objects)))
	    seq-of-attributes    (map #(nth input-stream %) (range (+ 5 number-of-objects)
								   (+ 5 number-of-attributes number-of-objects)))
	    incidence-matrix     (map #(nth input-stream %) (range (+ 5 number-of-attributes number-of-objects)
								   (+ 5 number-of-objects number-of-attributes
								      number-of-objects)))

	    incidence (set-of [(nth seq-of-objects idx-g) (nth seq-of-attributes idx-m)]
			      [idx-g (range number-of-objects)
			       idx-m (range number-of-attributes)
			       :when (= \X (nth (nth incidence-matrix idx-g) idx-m))])]
	  (make-context (set seq-of-objects)
			(set seq-of-attributes)
			incidence)))))
			    
	