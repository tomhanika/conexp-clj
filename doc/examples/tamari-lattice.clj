;; Copyright: Daniel Borchmann, 2010
;; This file is in the public domain.

;; Computes the Tamari lattice T_n of all bracketings of a set of n+1
;; symbols. See GW97, 5.4, example 11.

(in-ns 'user)
(use 'conexp.main)

;;;

(defn two-elemental-subsets
  "Returns the set of all two elemental subsets of set."
  [set]
  (set-of #{a b} [a set, b (disj set a)]))

(defn T
  "Returns a context for the Tamari lattice of all bracketings of n+1
  symbols."
  [n]
  (let [base-set (two-elemental-subsets (set-of-range n))]
    (make-context base-set
                  base-set
                  (set-of [#{i j} #{p q}]
                          [pair-1 base-set,
                           pair-2 base-set,
                           :let [i (apply min pair-1),
                                 j (apply max pair-1),
                                 p (apply min pair-2),
                                 q (apply max pair-2)]
                           :when (not (and (<= p i q j)
                                           (not= i q)))]))))

(defn tamari-lattice
  "Returns a lattice isomorphic to the one of bracketings of n+1
  symbols."
  [n]
  (concept-lattice (T n)))

;;; Now we draw

(use 'conexp.contrib.draw)

(defn draw-tamari-lattice
  "Draws the Tamari lattice for parameter n."
  [n]
  (draw-lattice (tamari-lattice n)))  

;;;

nil
