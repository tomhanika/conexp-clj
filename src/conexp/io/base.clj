(ns conexp.io.base
  (:use [clojure.contrib.ns-utils :only (immigrate)]))

;;;

(immigrate 'conexp.base
	   'conexp.fca.contexts
	   'clojure.contrib.str-utils
	   'clojure.contrib.io
	   'clojure.contrib.lazy-xml
	   'clojure.contrib.prxml
	   'conexp.io.util) ; last to override with-in-reader from c.c.io

;;;

nil
