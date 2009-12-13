(ns conexp.contrib.generators
  (:use [clojure.contrib.macro-utils :only (macrolet)])
  (:use [clojure.contrib.def])
  (:import [java.util.concurrent SynchronousQueue]
	   [java.util NoSuchElementException]))

;; Aim:
;;
;;  user=> (defg generator [x y]
;;           (dotimes [i x]
;;             (dotimes [j y]
;;               (yield (+ i j)))))
;;  #'user/generator
;;  user=> (def gen (generator 3 4))
;;  #'user/gen
;;  user=> (gen)
;;  0
;;  user=> (gen)
;;  1
;;  user=> (generate (generator 2 2))
;;  (0 1 1 2)
;;  user=>
;;

;; be aware of lazy sequences!
;; This does not work:
;;   (generate-by (fn runner [x]
;;                  (for [i (range x)]
;;                    (yield i)))
;;                100)
;;   -> () ; or even nothing (?)
;; but this does
;;   (generate-by (fn runner [x]
;;                  (doseq [i (range x)]
;;                    (yield i)))
;;                100)
;;   -> (0 1 2 3 ...)

(defmacro make-generator
  ""
  [function]
  `(let [#^SynchronousQueue queue# (SynchronousQueue.),
	 end# (atom false)]
     (letfn [(~(symbol "yield") [x#] (.put queue# x#))]
       (fn [& args#]
	 (.start (Thread. (fn []
			    (apply ~function args#)
			    (swap! end# (constantly true)))))
	 (fn []
	   (if @end#
	     (throw
	      (java.util.NoSuchElementException. "No more elements to generate."))
	     (.take queue#)))))))

(defmacro defg
  ""
  [name args & body] ; enable docstrings?
  `(defn ~name [& args#]
     (let [gen# (make-generator (fn ~name ~args ~@body))]
       (apply gen# args#))))

(defn generate
  ""
  [generator]
  (let [eos (gensym),
	runner (fn runner []
		 (lazy-seq
		  (let [next-elt (try
				  (generator)
				  (catch NoSuchElementException _
				    eos))]
		    (if (= next-elt eos)
		      nil
		      (cons next-elt (runner))))))]
    (runner)))

(defmacro generate-by
  ""
  [function & args]
  `(generate (apply (make-generator ~function) '~args)))

nil
