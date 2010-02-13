(ns conexp.layout
  (:use [clojure.contrib.ns-utils :only (immigrate)]))

(immigrate 'conexp.layout.layered
	   'conexp.layout.force)

;;;

(def *standard-layout-function* simple-layered-layout)

;;;

nil
