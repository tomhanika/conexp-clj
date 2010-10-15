;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.lattices
  (:use conexp.base
	conexp.fca.contexts))

(ns-doc "Basis datastructure and definitions for abstract lattices.")

;;; Datastructure

(declare order)

(deftype Lattice [base-set order-relation inf sup]
  Object
  (equals [this other]
    (and (= (class this) (class other))
         (= (.base-set this) (.base-set ^Lattice other))
	 (let [order-this (order this),
	       order-other (order other)]
	   (forall [x (.base-set this)
		    y (.base-set this)]
	     (<=> (order-this [x y])
		  (order-other [x y]))))))
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
  (if-let [order-relation (.order-relation lattice)]
    (fn order-fn
      ([pair] (order-relation pair))
      ([x y] (order-relation [x y])))
    (let [sup (.sup lattice)]
      (fn order-fn
        ([[x y]] (= y (sup x y)))
        ([x y] (= y (sup x y)))))))

(defn inf
  "Returns a function computing the infimum in lattice."
  [^Lattice lattice]
  (or (.inf lattice)
      (let [order (order lattice)
	    base  (base-set lattice)]
	(memo-fn _ [x y]
	  (first (for [z base
		       :when (and (order [z x])
				  (order [z y])
				  (forall [a base]
					  (=> (and (order [a x]) (order [a y]))
					      (order [a z]))))]
		   z))))))

(defn sup
  "Returns a function computing the supremum in lattice."
  [^Lattice lattice]
  (or (.sup lattice)
      (let [order (order lattice)
	    base  (base-set lattice)]
	(memo-fn _ [x y]
	  (first (for [z base
		       :when (and (order [x z])
				  (order [y z])
				  (forall [a base]
					  (=> (and (order [x a]) (order [y a]))
					      (order [z a]))))]
		   z))))))

(defmethod print-method Lattice [^Lattice lattice out]
  (.write ^java.io.Writer out
	  ^String (str "Lattice on " (count (base-set lattice)) " elements.")))


;;; Constructors

(defmulti make-lattice
  "Standard constructor for makeing lattice. Call with two arguments
  [base-set order] to construct the lattice by its order
  relation (given as a set of pairs or as a function of two
  arguments). Call with three arguments [base-set inf sup] to
  construct the lattice by its algebraic operations."
  {:arglists '([base-set order-relation] [base-set inf sup])}
  (fn [& args] (vec (map clojure-type args))))

(defmethod make-lattice [clojure-coll clojure-coll] [base-set order]
  (Lattice. (set base-set) (set order) nil nil))

(defmethod make-lattice [clojure-coll clojure-fn] [base-set order]
  (Lattice. (set base-set) (fn [[x y]] (order x y)) nil nil))

(defmethod make-lattice [clojure-coll clojure-fn clojure-fn] [base-set inf sup]
  (Lattice. (set base-set) nil inf sup))

(defmethod make-lattice :default [& args]
  (illegal-argument "The arguments " args " are not valid for a Lattice."))


;;; Standard Lattice Theory

(defn has-lattice-order?
  "Given a lattice checks if its order is indeed a lattice order."
  [lat]
  (and (forall [x (base-set lat)]
         ((order lat) x x))
       (forall [x (base-set lat),
                y (base-set lat)]
         (=> (and ((order lat) x y)
                  ((order lat) y x))
             (= x y)))
       (forall [x (base-set lat),
                y (base-set lat),
                z (base-set lat)]
         (=> (and ((order lat) x y)
                  ((order lat) y z))
             ((order lat) x z)))
       (forall [x (base-set lat),
                y (base-set lat)]
         (and (= 1 (count (set-of z [z (base-set lat)
                                     :when (and ((order lat) x z)
                                                ((order lat) y z)
                                                (forall [w (base-set lat)]
                                                  (=> (and ((order lat) x w)
                                                           ((order lat) y w))
                                                      ((order lat) w z))))])))
              (= 1 (count (set-of z [z (base-set lat)
                                     :when (and ((order lat) z x)
                                                ((order lat) z y)
                                                (forall [w (base-set lat)]
                                                  (=> (and ((order lat) w x)
                                                           ((order lat) w y))
                                                      ((order lat) w z))))])))))))

(defn dual-lattice
  "Dualizes given lattice lat."
  [lat]
  (let [order (order lat)]
    (make-lattice (base-set lat) (fn [x y] (order [y x])))))

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
  (let [inf  (inf lat)
	sup  (sup lat)
	base (base-set lat)
	ordr (order lat)]
    (forall [x base
	     y base
	     z base]
      (=> (ordr [x z])
	  (= (sup x (inf y z))
	     (inf (sup x y) z))))))

(defn lattice-one
  "Returns the one element of lattice lat."
  [lat]
  (let [order (order lat)
	base  (base-set lat)]
    (first (set-of x [x base :when (forall [y base] (order [y x]))]))))

(defn lattice-zero
  "Returns the zero element of lattice lat."
  [lat]
  (let [order (order lat)
	base  (base-set lat)]
    (first (set-of x [x base :when (forall [y base] (order [x y]))]))))

(defn directly-neighboured?
  "Checks whether x is direct lower neighbour of y in lattice lat."
  [lat x y]
  (let [order (order lat)
	base  (base-set lat)]
    (and (not= x y)
	 (order [x y])
	 (forall [z base]
	   (=> (and (not= z x) (not= z y))
	       (not (and (order [x z])
			 (order [z y]))))))))

(defn lattice-upper-neighbours
  "Returns all direct upper neighbours of x in lattice lat."
  [lat x]
  (set-of y [y (base-set lat) :when (directly-neighboured? lat x y)]))

(defn lattice-lower-neighbours
  "Returns all direct lower neighbours of y in lattice lat."
  [lat y]
  (set-of x [x (base-set lat) :when (directly-neighboured? lat x y)]))

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
  (set-of y [y (base-set lat) :when (= 1 (count (lattice-lower-neighbours lat y)))]))

(defn lattice-inf-irreducibles
  "Returns the inf-irreducible elements of lattice lat."
  [lat]
  (set-of x [x (base-set lat) :when (= 1 (count (lattice-upper-neighbours lat x)))]))

(defn lattice-irreducibles
  "Returns all (i.e. sup or inf) irreducible elements of lattice lat."
  [lat]
  (intersection (lattice-sup-irreducibles lat)
		(lattice-inf-irreducibles lat)))


;;; FCA

(defn concept-lattice
  "Returns for a given context ctx its concept lattice."
  [ctx]
  (make-lattice (set (concepts ctx))
		(fn [A B]
		  (subset? (first A) (first B)))))

(defn standard-context
  "Returns the standard context of lattice lat."
  [lat]
  (make-context (lattice-sup-irreducibles lat)
		(lattice-inf-irreducibles lat)
		(fn [x y]
		  ((order lat) [x y]))))

;;;

nil
