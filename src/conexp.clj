(ns conexp
  (:use [clojure.contrib.ns-utils :only (immigrate)]))

; Use immigrate, so that all imports behave as if they were defined in here.
; Thus (use 'conexp) makes sense.

(immigrate 'conexp.base
	   'conexp.fca
	   'conexp.io
	   'conexp.gui)

(def *conexp-version* 1258063004251)

nil
