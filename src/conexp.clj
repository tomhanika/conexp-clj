(ns conexp
  (:use [clojure.contrib.ns-utils :only (immigrate)]))

; Use immigrate, so that all imports behave as if they were defined in here.
; Thus (use 'conexp) makes sense.

(def *conexp-namespaces* '[conexp.base
			   conexp.fca
			   conexp.io
			   conexp.layout
			   conexp.graphics
			   conexp.gui])

(dorun
 (map immigrate *conexp-namespaces*))

(def *conexp-version* {:major 0,
		       :minor 1,
		       :timestamp 1264974270492,
		       :qualifier "pre-alpha"})

(defn conexp-version
  "Returns the version of conexp as a string."
  []
  (let [{:keys [major minor timestamp qualifier]} *conexp-version*]
    (str major "." minor "." timestamp "_" qualifier)))

(defn has-version?
  "Compares given version of conexp and returns true if and only if
  the current version of conexp is higher or equal than the given one"
  [{my-major :major, my-minor :minor, my-timestamp :timestamp}]
  (let [{:keys [major, minor, timestamp]} *conexp-version*]
    (or (and (< my-major major))
	(and (= my-major major)
	     (< my-minor minor))
	(and (= my-major major)
	     (= my-minor minor)
	     (<= my-timestamp timestamp)))))

nil
