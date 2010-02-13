(ns conexp.current
  (:require conexp
	    clojure.contrib.pprint
	    clojure.contrib.repl-utils))

;;;

(defn- make-test-lattice
  ""
  [n]
  (conexp/concept-lattice (conexp/rand-context (conexp/set-of-range n) 0.4)))

;;;

nil
