(ns conexp.fca
  (:use [clojure.contrib.ns-utils :only (immigrate)]))

(immigrate 'conexp.fca.contexts
	   'conexp.fca.implications
	   'conexp.fca.exploration)