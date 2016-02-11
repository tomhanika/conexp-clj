(ns conexp.contrib.experimental.linear-extensions
  (:use conexp.main))

(defn mu-function-from-context
  "Given a formal context ctx, returns (a form of) the Möbius function of the
  corresponding concept lattice."
  [ctx]
  (memo-fn mu [[A B]]
    (if (= A (objects ctx))
      1
      (reduce + (map mu (direct-upper-concepts ctx [A B]))))))

(defn number-of-linear-extensions
  "Returns the number of linear extensions of the concept lattice of the formal context
  ctx."
  [ctx]
  (let [mu (mu-function-from-context ctx)]
    (mu [#{} (oprime ctx #{})])))

(defn task
  "Computes the number of linear extensions of the free distributive lattice on n
  generators."
  [n]
  (number-of-linear-extensions
   (reduce context-product
           (take n
                 (repeat (make-context [0 1] [0 1] [[0 1]]))))))
