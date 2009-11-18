(ns conexp
  (:use [clojure.contrib.ns-utils :only (immigrate)]))

; Use immigrate, so that all imports behave as if they were defined in here.
; Thus (use 'conexp) makes sense.

(def *conexp-namespaces* '[conexp.base
			   conexp.fca
			   conexp.io
			   conexp.gui
			   conexp.layout])

(dorun (map immigrate *conexp-namespaces*))

(def *conexp-version* 1258574048577)

nil
