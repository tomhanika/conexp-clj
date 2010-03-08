;;;

(defmacro tests-to-run
  "Defines tests to run when the namespace in which this macro is
  called is tested by test-ns."
  [& namespaces]
  `(defn test-ns-hook []
     (dosync
      (ref-set ~'*report-counters*
	       (merge-with + ~@(map (fn [ns] `(test-ns '~ns)) namespaces))))))

;;;

nil
