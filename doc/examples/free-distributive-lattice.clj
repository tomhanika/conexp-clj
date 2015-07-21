(require 'conexp.main)
(in-ns 'conexp.main)

(defn number-of-elements-in-FCD
  "For a given number `n', returns the number of elements in the free distributive lattice
  of n generators."
  [n]
  ;; we *could* do the indictive construction, but for the values where `intents' works,
  ;; the direct contextual construction works as well
  (count (intents (reduce-context (make-context (subsets (set-of-range n))
                                                (subsets (set-of-range n))
                                                (comp not superset?))))))
