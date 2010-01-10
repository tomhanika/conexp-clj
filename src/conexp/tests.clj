(ns conexp.tests
  (:use [clojure.test :only (*stack-trace-depth* run-tests)]))


;;;

(def *testing-namespaces* '[conexp.tests.util
			    conexp.tests.base
			    conexp.tests.fca.contexts
			    conexp.tests.fca.implications])

(apply require *testing-namespaces*)

(binding [*stack-trace-depth* 13]
  (defn test-conexp []
    (apply run-tests *testing-namespaces*)
    nil))

;;;

nil
