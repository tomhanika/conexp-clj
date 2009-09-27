(ns conexp
  (:use [clojure.contrib.ns-utils :only (immigrate)]))

; Use immigrate, so that all imports behave as if they were defined in here.
; Thus (use 'conexp) makes sense.

(immigrate 'conexp.base)

(compile 'conexp.fca.contexts)
(compile 'conexp.fca.implications)

(immigrate 'conexp.fca)

(compile 'conexp.gui.repl)

(immigrate 'conexp.gui)