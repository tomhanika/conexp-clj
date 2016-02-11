;; Daniel Borchmann, 2010
;; This file is in the public domain.

;; Computes the Tamari lattice T_n of all bracketings of a set of n+1
;; symbols. See GW97, 5.4, example 11.

(require 'conexp.main)
(in-ns 'conexp.main)

;;;

(defn T
  "Returns a context for the Tamari lattice of all bracketings of n+1
  symbols."
  [n]
  (let [elements (set-of-range n),
        base-set (set-of [a b] [a elements, b elements, :when (< a b)])]
    (make-context base-set
                  base-set
                  (set-of [[i j] [p q]]
                          [[i j] base-set,
                           [p q] base-set,
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
