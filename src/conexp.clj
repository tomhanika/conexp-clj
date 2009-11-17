(ns conexp
  (:use [clojure.contrib.ns-utils :only (immigrate)]))

; Use immigrate, so that all imports behave as if they were defined in here.
; Thus (use 'conexp) makes sense.

(def *conexp-namespaces* '[conexp.base
			   conexp.fca
			   conexp.io
			   conexp.gui])

(dorun (map immigrate *conexp-namespaces*))

(defn conexp-api
  "Prints conexp-clj api to file."
  [file]
  (apply public-api-to-file file *conexp-namespaces*))

(def *conexp-version* 1258483489601)

nil
