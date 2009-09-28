(ns conexp.tests
  (:use [clojure.test :only (run-tests)]))

(def *testing-namespaces* '[conexp.tests.util])

(defn test-conexp []
  (apply run-tests *testing-namespaces*))