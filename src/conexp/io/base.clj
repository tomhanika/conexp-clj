(ns conexp.io.base
  (:use [clojure.contrib.ns-utils :only (immigrate)]))

(immigrate 'conexp.base
	   'conexp.fca.contexts
	   'conexp.io.util
	   'clojure.contrib.str-utils
	   'clojure.contrib.duck-streams
	   'clojure.contrib.lazy-xml
	   'clojure.contrib.prxml)
