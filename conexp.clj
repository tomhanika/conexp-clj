(ns conexp
  (:use [clojure.contrib.ns-utils :only (immigrate)]))

(defn compile-conexp []
  (compile 'conexp.fca.contexts)
  (compile 'conexp.fca.implications)
  (compile 'conexp.fca.lattices)
  (compile 'conexp.gui.repl))

(compile-conexp)

; Use immigrate, so that all imports behave as if they were defined in here.
; Thus (use 'conexp) makes sense.

(immigrate 'conexp.base
	   'conexp.fca
	   'conexp.gui)
