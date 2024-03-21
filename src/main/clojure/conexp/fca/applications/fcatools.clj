(ns conexp.fca.applications.fcatools
 (:require [conexp.io.contexts :refer :all]))

(defn context-from-fcatools [ctx-name]
  (read-context (str "https://raw.githubusercontent.com/fcatools/contexts/main/contexts/" ctx-name))
)

