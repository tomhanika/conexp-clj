(ns conexp.current
  (:require conexp)
  (:use clojure.contrib.pprint
	clojure.contrib.repl-utils))
;;;

(defmulti go
  "Main testing function."
  (fn [& args] (first args)))

(defmethod go :default [& args]
  (conexp/illegal-argument "No tester for " args))
;;;

(defn- make-test-lattice
  ""
  [n]
  (conexp/concept-lattice (conexp/rand-context (conexp/set-of-range n) 0.4)))

(defmethod go :draw [_ n]
  (conexp/draw-lattice (make-test-lattice n)))

;;;

nil
