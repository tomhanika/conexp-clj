;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.current
  (:require conexp
	    [conexp.contrib.algorithms.concepts :as concepts])
  (:use clojure.contrib.pprint
	clojure.contrib.repl-utils))

(conexp/update-ns-meta! conexp.current
  :doc "Contains code snippets (i.e. repl helpers) for testing and developing conexp-clj.")

;;;

(defmulti go
  "Main testing function."
  {:arglists '([dispatch & args])}
  (fn [dispatch & args] dispatch))

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


;;;

nil
