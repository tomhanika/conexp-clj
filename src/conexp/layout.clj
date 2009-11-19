(ns conexp.layout
  (:use [clojure.contrib.ns-utils :only (immigrate)]))

(immigrate 'conexp.layout.basic-layouts)

(def *standard-layout* simple-layered-layout)

nil
