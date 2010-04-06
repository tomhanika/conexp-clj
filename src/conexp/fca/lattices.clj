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

;;; Datastructure

(declare order)

(deftype Lattice [base-set order-relation inf sup]
  :as this
  Object
  (equals [other]
    (and (= ::Lattice (type other))
     	 (= (:base-set this) (:base-set other))
	 (let [order-this (order this),
	       order-other (order other)]
	   (forall [x (:base-set this)
		    y (:base-set this)]
	     (<=> (order-this [x y])
		  (order-other [x y]))))))
  (hashCode []
    ;; can't think of a better way ...
    (hash base-set)))

(defn base-set
  "Returns the base set of lattice."
  [lattice]
  (:base-set lattice))

(defn order
  "Returns a set of pairs representing the order relation on lattice."
  [lattice]
  (or (:order-relation lattice)
      (let [sup (:sup lattice)]
	(fn [[x y]] (= y (sup x y))))))

(defn inf
  "Returns a function computing the infimum in lattice."
  [lattice]
  (or (:inf lattice)
      (let [order (order lattice)
	    base  (base-set lattice)]
	(fn [x y]
	  (first (for [z base
		       :when (and (order [z x])
				  (order [z y])
				  (forall [a base]
					  (=> (and (order [a x]) (order [a y]))
					      (order [a z]))))]
		   z))))))

(defn sup
  "Returns a function computing the supremum in lattice."
  [lattice]
  (or (:sup lattice)
      (let [order (order lattice)
	    base  (base-set lattice)]
	(fn [x y]
	  (first (for [z base
		       :when (and (order [x z])
				  (order [y z])
				  (forall [a base]
					  (=> (and (order [x a]) (order [y a]))
					      (order [z a]))))]
		   z))))))

(defmethod print-method ::Lattice [lattice out]
  (.write out
	  (str "Lattice on " (count (base-set lattice)) " elements.")))


;;; Constructors

(defn- type-of
  "Type dispatch for make-lattice.

  TODO: Change this to use type dispatch from conexp.util."
  [thing]
  (cond
    (set? thing) ::set
    (fn? thing)  ::fn
    (seq? thing) ::seq
    :else        ::invalid))

(defmulti make-lattice
  "Standard constructor for makeing lattice. Call with two arguments
  [base-set order] to construct the lattice by its order
  relation (given as a set or as a function). Call with three
  arguments [base-set inf sup] to construct the lattice by its
  algebraic operations."
  {:arglists '([base-set order-relation] [base-set inf sup])}
  (fn [& args] (vec (map type-of args))))

(defmethod make-lattice [::set ::set] [base-set order]
  (Lattice base-set order nil nil))

(defmethod make-lattice [::set ::fn] [base-set order]
  (Lattice base-set order nil nil))

(defmethod make-lattice [::set ::fn ::fn] [base-set inf sup]
  (Lattice base-set nil inf sup))

(defmethod make-lattice :default [& args]
  (illegal-argument "The arguments " args " are not valid for a Lattice."))


;;; Standard Lattice Theory

(defn dual-lattice
  "Dualizes given lattice lat."
  [lat]
  (let [order (order lat)]
    (make-lattice (base-set lat) (fn [[x y]] (order [y x])))))

(defn distributive?
  "Checks (primitively) whether given lattice lat is distributive or not."
  [lat]
  (let [inf  (inf lat)
	sup  (sup lat)
	base (base-set lat)]
    (forall [x base
	     y base
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
		(fn [[A B]]
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
