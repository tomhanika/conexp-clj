;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.profiler
  (:use [conexp.base :only (ns-doc)])
  (:use clojure.contrib.profile
        [clojure.contrib.def :only (defnk, defvar-)]
        [clojure.contrib.pprint :only (pprint, cl-format)]
        [clojure.contrib.except :only (throw-if, throw-if-not)]))

(ns-doc
 "Provides simple function for statistical and instrumental
 profiling.")

;;; Low Level

(defn current-thread
  "Returns the current thread."
  []
  (Thread/currentThread))

(defn- get-root-thread-group
  "Returns the root ThreadGroup."
  []
  (loop [^ThreadGroup tg (.getThreadGroup ^Thread (Thread/currentThread))]
    (if-let [parent (.getParent tg)]
      (recur parent)
      tg)))

(defn get-thread-list
  "Returns the list of all threads currently running."
  []
  (let [^ThreadGroup rg (get-root-thread-group),
        thread-count (.activeCount rg),
        ^"[Ljava.lang.Thread;" array (make-array Thread (* 2 thread-count)),
        returned-count (.enumerate rg array true)]
    (if (< returned-count (* 2 thread-count))
      (take-while (comp not nil?) array)
      (get-thread-list))))

(defn- stackdump
  "Returns a sequence of names representing the stack of the given
  thread."
  [^Thread thread]
  (map (fn [^StackTraceElement stack-element]
         (str (.getClassName stack-element)
              "/"
              (.getMethodName stack-element)
              " ("
              (.getFileName stack-element)
              ":"
              (.getLineNumber stack-element)
              ")"))
       (.getStackTrace thread)))

(defonce #^{:private true,
            :doc "Contains for every thread a hash-map mapping method
  names to the number of occurences found while profiling."}
  profiled-data (ref {}))

(defn show-profiled-threads
  "Returns a sequence of all profiled threads."
  []
  (keys @profiled-data))

(defonce #^{:private true,
            :doc "Contains for every thread the profiling thread."}
  profilers (ref {}))

(defn- get-profiled-data
  "Returns the profiled data for thread or nil if thread is not
  profiled."
  [thread]
  (get @profiled-data thread nil))

(declare tick-thread)

(defn- add-profiled-thread
  "Adds thread to the currently profiled threads and starts
  profiling it."
  [^Thread thread, period]
  (dosync
   (alter profiled-data assoc thread {:overall 0,
                                      :period period})
   (alter profilers assoc thread (Thread. #(while (and (contains? @profiled-data thread)
                                                       (not= Thread$State/TERMINATED
                                                             (.getState thread)))
                                             (tick-thread thread)
                                             (Thread/sleep period)))))
  (.start ^Thread (get @profilers thread)))

(defn- delete-profiled-thread
  "Removes thread from the currently profiled threads. Returns the
  collected data."
  [thread]
  (dosync
   (let [data (get @profiled-data thread)]
     (alter profiled-data dissoc thread)
     (alter profilers dissoc thread)
     data)))

(defn- tick-thread
  "Adds current stack trace to profiled data of thread."
  [thread]
  (dosync
   (ref-set profiled-data
            (assoc @profiled-data thread
                   (reduce #(assoc %1 %2 (inc (get %1 %2 0)))
                           (update-in (get @profiled-data thread)
                                      [:overall] inc)
                           (stackdump thread))))))

;;; Formatting

(defn- pprint-profiling
  "Pretty prints the given profiling data, given as a sequence of
  [name hitcount] pairs."
  [thread data overall-count period threshold]
  (let [min-count (* threshold overall-count),
        data      (filter (fn [[_ count]] (< min-count count))
                          data)]
    (if (empty? data)
      (cl-format true "~&Nothing~%")
      (let [max-hit-length (+ 1 (count (str (reduce (fn [previous [_ new]]
                                                      (max previous new))
                                                    0
                                                    data))))]
        (cl-format true "~&Profiling data~@
                         ~&  with period ~ams~@
                         ~&  with a total of ~a ticks~@
                         ~%"
                   period overall-count)
        (cl-format true "~&~v@a ~8@a  ~a~%" max-hit-length "Hits" "Amount" "Function")
        (doseq [[name hit-count] data]
          (cl-format true
                     "~&~v@a ~7,2,2f%  ~a~%"
                     max-hit-length hit-count
                     (float (/ hit-count overall-count))
                     name))))))

;;; High Level

(defnk start-profiling
  "Starts profiling the given thread. Options include

     :thread for the thread to be profiled (defaults
             to (current-thread)),
     :period for the period between two ticks, given in ms, defaults
             to 100"
  [:thread (current-thread)
   :period 100]
  (assert (instance? Thread thread))
  (throw-if (get-profiled-data thread)
            IllegalArgumentException "Thread already profiled.")
  (add-profiled-thread thread period))

(defnk stop-profiling
  "Stops profiling the given thread. The :thread keyword argument may
  be given to indicate which profiling to stop, defaults
  to (current-thread)."
  [:thread (current-thread)]
  (assert (instance? Thread thread))
  (throw-if-not (get-profiled-data thread)
                IllegalArgumentException "Thread is not profiled.")
  (delete-profiled-thread thread)
  nil)

(defnk show-profiling
  "Show the statistics collected so far while profiling
  thread. Options include

    :thread    for the thread to be profiled (default
                 to (current-thread)),
    :pattern   for filtering the output via re-find
    :threshold for the minimum precentage a function has
                 to ticked to be shown."
  [:thread (current-thread),
   :pattern #".*",
   :threshold -1.0]
  (assert (instance? Thread thread))
  (let [data (get-profiled-data thread)]
    (throw-if-not data IllegalArgumentException "Thread is not profiled.")
    (pprint-profiling thread
                      (sort (fn [[x a] [y b]]
                              (compare [b y] [a x]))
                            (remove (fn [[name hits]]
                                      (or (keyword? name)
                                          (not (re-find pattern name))))
                                    data))
                      (:overall data)
                      (:period data)
                      threshold)))

;;;

(defn run-and-profile
  "Runs the given function f, which must be a function of no
  arguments, in a separate thread and profiles it. Returns the new
  thread."
  [f]
  (let [thread (Thread. f)]
    (start-profiling :thread thread)
    (.start thread)
    thread))

(defmacro with-profiler
  "Runs given body under the supervision of the profiler and prints
  the result. starting-options will be given to start-profiling,
  output-options will be given to show-profiling."
  [starting-options output-options & body]
  `(let [^Thread thread# (Thread. #(do ~@body))]
     (start-profiling :thread thread# ~@starting-options)
     (try (time (do
                  (.start thread#)
                  (.join thread#)))
          (catch Throwable ex#
            (println "Exception thrown:" ex#)
            (.stop thread#))
          (finally
           (show-profiling :thread thread# ~@output-options)
           (stop-profiling :thread thread#)))))

;;;

(defmacro with-profiled-fns
  "Runs code in body with all given functions being profiled. Each
  function will be instrumentalized to measure its invocations."
  [fns & body]
  `(binding ~(vec (apply concat
			 (for [fn (distinct fns)]
			   `[~fn (let [orig-fn# ~fn]
				   (fn [& args#]
				     (prof ~(keyword fn)
				       (apply orig-fn# args#))))])))
     (let [data# (with-profile-data ~@body)]
       (if (not (empty? data#))
	 (print-summary (summarize data#))))))

;;;

nil
