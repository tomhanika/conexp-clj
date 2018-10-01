;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.lattices
  "Basis datastructure and definitions for abstract lattices."
  (:use conexp.base
        conexp.fca.contexts))

;;; Datastructure

(declare order)

(deftype Lattice [base-set order-function inf sup]
  Object
  (equals [this other]
    (and (= (class this) (class other))
         (= (.base-set this) (.base-set ^Lattice other))
         (let [order-this (order this),
               order-other (order other)]
           (or (= order-this order-other)
               (forall [x (.base-set this)
                        y (.base-set this)]
                 (<=> (order-this x y)
                      (order-other x y)))))))
  (hashCode [this]
    (hash-combine-hash Lattice base-set)))

(defn base-set
  "Returns the base set of lattice."
  [^Lattice lattice]
  (.base-set lattice))

(defn order
  "Returns a function of one or two arguments representing the order
  relation. If called with one argument it is assumed that this
  argument is a pair of elements."
  [^Lattice lattice]
  (let [order-function (.order-function lattice)]
    (fn order-fn
      ([pair] (order-function (first pair) (second pair)))
      ([x y] (order-function x y)))))

(defn inf
  "Returns a function computing the infimum in lattice."
  [^Lattice lattice]
  (.inf lattice))

(defn sup
  "Returns a function computing the supremum in lattice."
  [^Lattice lattice]
  (.sup lattice))

(defmethod print-method Lattice [^Lattice lattice, ^java.io.Writer out]
  (.write out
          ^String (str "Lattice on " (count (base-set lattice)) " elements.")))


;;; Constructors

(defmulti make-lattice-nc
  "Creates a new lattice from the given arguments, without any
  checks. Use with care."
  {:arglists '([base-set order-function]
               [base-set inf sup]
               [base-set order-function inf sup])}
  (fn [& args] (vec (map clojure-type args))))

(defmethod make-lattice-nc [clojure-coll clojure-coll] [base-set order]
  (let [order (set order)]
    (make-lattice-nc base-set (fn [x y] (contains? order [x y])))))

(defmethod make-lattice-nc [clojure-coll clojure-fn] [base-set order]
  (let [inf (memoize (fn inf [x y]
                       (loop [elements base-set]
                         (let [z (first elements)]
                           (if (and (order z x)
                                    (order z y)
                                    (forall [a base-set]
                                      (=> (and (order a x) (order a y))
                                          (order a z))))
                             z
                             (recur (rest elements))))))),
        sup (memoize (fn sup [x y]
                       (loop [elements base-set]
                         (let [z (first elements)]
                           (if (and (order x z)
                                    (order y z)
                                    (forall [a base-set]
                                      (=> (and (order x a) (order y a))
                                          (order z a))))
                             z
                             (recur (rest elements)))))))]
    (make-lattice-nc base-set order inf sup)))

(defmethod make-lattice-nc [clojure-coll clojure-fn clojure-fn] [base-set inf sup]
  (make-lattice-nc base-set
                   (fn [x y] (= y (sup x y)))
                   inf
                   sup))

(defmethod make-lattice-nc [clojure-coll clojure-fn clojure-fn clojure-fn]
  [base-set order inf sup]
  (Lattice. (set base-set) order inf sup))

(defmethod make-lattice-nc :default [& args]
  (illegal-argument "The arguments " args " are not valid for a Lattice."))

(defn has-lattice-order?
  "Given a lattice checks if its order is indeed a lattice order."
  [lat]
  (let [<= (order lat)]
    (and (forall [x (base-set lat)]
           (<= x x))
         (forall [x (base-set lat),
                  y (base-set lat)]
           (=> (and (<= x y)
                    (<= y x))
               (= x y)))
         (forall [x (base-set lat),
                  y (base-set lat),
                  z (base-set lat)]
           (=> (and (<= x y)
                    (<= y z))
               (<= x z)))
         (forall [x (base-set lat),
                  y (base-set lat)]
           (and (singleton? (for [z (base-set lat)
                                  :when (and (<= x z)
                                             (<= y z)
                                             (forall [w (base-set lat)]
                                               (=> (and (<= x w)
                                                        (<= y w))
                                                   (<= w z))))]
                              z))
                (singleton? (for [z (base-set lat)
                                  :when (and (<= z x)
                                             (<= z y)
                                             (forall [w (base-set lat)]
                                               (=> (and (<= w x)
                                                        (<= w y))
                                                   (<= w z))))]
                              z)))))))

(defn make-lattice
  "Standard constructor for makeing lattice. Call with two arguments
  [base-set order] to construct the lattice by its order
  relation (given as a set of pairs or as a function of two
  arguments). Call with three arguments [base-set inf sup] to
  construct the lattice by its algebraic operations.

  Note: This function will test the resulting lattice for being one,
  which may take some time. If you don't want this, use
  make-lattice-nc."
  [& args]
  (let [lattice (apply make-lattice-nc args)]
    (when-not (has-lattice-order? lattice)
      (illegal-argument "Given arguments do not describe a lattice."))
    lattice))

;;; Standard Lattice Theory

(defn dual-lattice
  "Dualizes given lattice lat."
  [lat]
  (let [order (order lat)]
    (make-lattice-nc (base-set lat) (fn [x y] (order y x)))))

(defn distributive?
  "Checks (primitively) whether given lattice lat is distributive or not."
  [lat]
  (let [inf  (inf lat),
        sup  (sup lat),
        base (base-set lat)]
    (forall [x base,
             y base,
             z base]
      (= (sup x (inf y z))
         (inf (sup x y) (sup x z))))))

(defn modular?
  "Checks (primitively) whether given lattice lat is modular or not."
  [lat]
  (let [inf  (inf lat),
        sup  (sup lat),
        base (base-set lat),
        ordr (order lat)]
    (forall [x base,
             y base,
             z base]
      (=> (ordr x z)
          (= (sup x (inf y z))
             (inf (sup x y) z))))))

(defn lattice-one
  "Returns the one element of lattice lat."
  [lat]
  (when (empty? (base-set lat))
    (illegal-argument "The lattice is empty and therefore cannot have a maximal element."))
  (let [order (order lat)]
    (reduce (fn [x a]
              (if (order x a) a x))
            (base-set lat))))

(defn lattice-zero
  "Returns the zero element of lattice lat."
  [lat]
  (when (empty? (base-set lat))
    (illegal-argument "The lattice is empty and therefore cannot have a minimal element."))
  (let [order (order lat)]
    (reduce (fn [x a]
              (if (order a x) a x))
            (base-set lat))))

(defn directly-neighboured?
  "Checks whether x is direct lower neighbour of y in lattice lat."
  [lat x y]
  (let [order (order lat)]
    (and (not= x y)
         (order x y)
         (let [base  (disj (base-set lat) x y)]
           (forall [z base]
             (not (and (order x z) (order z y))))))))

(defn lattice-upper-neighbours
  "Returns all direct upper neighbours of x in lattice lat."
  [lat x]
  (set-of y [y (base-set lat)
             :when (directly-neighboured? lat x y)]))

(defn lattice-lower-neighbours
  "Returns all direct lower neighbours of y in lattice lat."
  [lat y]
  (set-of x [x (base-set lat)
             :when (directly-neighboured? lat x y)]))

(defn lattice-atoms
  "Returns the lattice atoms of lat."
  [lat]
  (lattice-upper-neighbours lat (lattice-zero lat)))

(defn lattice-coatoms
  "Returns the lattice coatoms of lat."
  [lat]
  (lattice-lower-neighbours lat (lattice-one lat)))

(defn lattice-sup-irreducibles
  "Returns the sup-irreducible elements of lattice lat."
  [lat]
  (set-of y [y (base-set lat)
             :when (= 1 (count (lattice-lower-neighbours lat y)))]))

(defn lattice-inf-irreducibles
  "Returns the inf-irreducible elements of lattice lat."
  [lat]
  (set-of x [x (base-set lat)
             :when (= 1 (count (lattice-upper-neighbours lat x)))]))

(defn lattice-doubly-irreducibles
  "Returns all (i.e. sup and inf) irreducible elements of lattice lat."
  [lat]
  (intersection (lattice-sup-irreducibles lat)
                (lattice-inf-irreducibles lat)))


;;; FCA

(defn concept-lattice
  "Returns for a given context ctx its concept lattice."
  [ctx]
  (make-lattice-nc (set (concepts ctx))
                   (fn <= [[A _] [C _]]
                     (subset? A C))
                   (fn inf [[A _] [C _]]
                     (let [A+C (intersection A C)]
                       [A+C (object-derivation ctx A+C)]))
                   (fn sup [[_ B] [_ D]]
                     (let [B+D (intersection B D)]
                       [(attribute-derivation ctx B+D) B+D]))))

(defn standard-context
  "Returns the standard context of lattice lat."
  [lat]
  (make-context (lattice-sup-irreducibles lat)
                (lattice-inf-irreducibles lat)
                (fn [x y]
                  ((order lat) [x y]))))

;;;

nil
