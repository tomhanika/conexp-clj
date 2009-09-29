(ns conexp.tests
  (:use [clojure.test :only (*stack-trace-depth* run-tests)]))

(def *testing-namespaces* '[conexp.tests.util
			    conexp.tests.base])

(defn test-conexp []
  (binding [*stack-trace-depth* 13]
    (apply run-tests *testing-namespaces*)))
