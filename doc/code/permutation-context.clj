;; Daniel Borchmann, 2010
;; This file is in the public domain.

;; Permutations can be ordered such that they form a complete
;; lattice. This program computes a formal context for a lattice
;; isomorphic to the lattice of all permutations on a set {1,...,n}.
;; For this see Ganter-Wille, 1.4, Example 12.

(require 'conexp.main)
(in-ns 'conexp.main)

;;; Context computation

(defn context-crossover
  "For four given context ctx-{1,2,3,4} computes

     ctx-1 | ctx-2
    -------+-------
     ctx-3 | ctx-4
  "
  [ctx-1 ctx-2 ctx-3 ctx-4]
  (context-subposition (context-apposition ctx-1 ctx-2)
                       (context-apposition ctx-3 ctx-4)))

(defn L
  "Computes the context IL as in example 12."
  [n]
  (if (<= n 0)
    (one-context #{1})
    (let [L_n-1 (L (- n 1))]
      (context-crossover (make-context (objects L_n-1) (attributes L_n-1) #{})
                         L_n-1 L_n-1 L_n-1))))

(defn K
  "Computes the context IK as in examples 12. Its concept lattice is
  isomorphic to the lattice of all permutation on the set #{1,...,n}."
  [n]
  (if (<= n 0)
    (one-context #{1})
    (let [K_n-1 (K (- n 1))]
      (context-crossover K_n-1 K_n-1 K_n-1 (L (- n 1))))))

;; Try it out!

(defn run-examples []
  (println (count (concepts (K 1)))) ;-> 1
  (println (count (concepts (reduce-context (K 5)))))) ;-> 120

;;;

nil
