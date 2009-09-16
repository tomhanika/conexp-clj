(ns conexp.fca
  (:use clojure.set
	conexp.base
	conexp.util
	conexp.fca.contexts
	conexp.fca.implications
	conexp.fca.exploration))

(compile 'conexp.fca.contexts)
(compile 'conexp.fca.implications)

