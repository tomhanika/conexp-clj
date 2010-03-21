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

(deftype Lattice [base-set order inf sup]
  :as this
  Object
  (equals [other]
    (and (= ::Lattice (type other))
	 (= (base-set this) (base-set other))
	 (let [order-this (order this)
	       order-other (order other)]
	   (or (= order-this order-other)
	       (forall [pair (cross-product (base-set this) (base-set this))]
		       (<=> (order-this pair) (order-other pair)))))))
  (hashCode []
    ;; this is not correct
    (let [base-set (base-set this)
	  order    (order this)
	  inf      (inf this)
	  sup      (sup this)]
      (bit-xor (hash base-set)
	       (if order
		 (hash order)
		 (bit-xor (hash inf)
			  (hash sup)))))))

(defn base-set [lattice]
  (:base-set lattice))

(defn order [lattice]
  (or (:order lattice)
      (let [sup (:sup lattice)]
	(fn [[x y]] (= y (sup x y))))))

(defn inf [lattice]
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

(defn sup [lattice]
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

(defn type-of [thing]
  (cond
    (set? thing) ::set
    (fn? thing)  ::fn
    (seq? thing) ::seq
    :else        ::invalid))

(defmulti make-lattice (fn [& args] (vec (map type-of args))))

(defmethod make-lattice [::set ::set] [base-set order]
  (Lattice base-set order nil nil))

(defmethod make-lattice [::set ::fn] [base-set order]
  (Lattice base-set order nil nil))

(defmethod make-lattice [::set ::fn ::fn] [base-set inf sup]
  (Lattice base-set nil inf sup))

(defmethod make-lattice :default [& args]
  (illegal-argument "The arguments " args " are not valid for a Lattice."))


;;; Standard Lattice Theory

(defn dual-lattice [lat]
  (let [order (order lat)]
    (make-lattice (base-set lat) (fn [[x y]] (order [y x])))))

(defn distributive? [lat]
  (let [inf  (inf lat)
	sup  (sup lat)
	base (base-set lat)]
    (forall [x base
	     y base
	     z base]
      (= (sup x (inf y z))
	 (inf (sup x y) (sup x z))))))

(defn modular? [lat]
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

(defn lattice-zero [lat]
  (let [order (order lat)
	base  (base-set lat)]
    (first (set-of x [x base :when (forall [y base] (order [x y]))]))))

(defn directly-neighboured? [lat x y]
  (let [order (order lat)
	base  (base-set lat)]
    (and (not= x y)
	 (order [x y])
	 (forall [z base]
	   (=> (and (not= z x) (not= z y))
	       (not (and (order [x z])
			 (order [z y]))))))))

(defn lattice-upper-neighbours [lat x]
  (set-of y [y (base-set lat) :when (directly-neighboured? lat x y)]))

(defn lattice-lower-neighbours [lat y]
  (set-of x [x (base-set lat) :when (directly-neighboured? lat x y)]))

(defn lattice-atoms [lat]
  (lattice-upper-neighbours lat (lattice-zero lat)))

(defn lattice-coatoms [lat]
  (lattice-lower-neighbours lat (lattice-one lat)))

(defn lattice-sup-irreducibles [lat]
  (set-of y [y (base-set lat) :when (= 1 (count (lattice-lower-neighbours lat y)))]))

(defn lattice-inf-irreducibles [lat]
  (set-of x [x (base-set lat) :when (= 1 (count (lattice-upper-neighbours lat x)))]))

(defn lattice-irreducibles [lat]
  (intersection (lattice-sup-irreducibles lat)
		(lattice-inf-irreducibles lat)))


;;; FCA

(defn concept-lattice [ctx]
  (make-lattice (set (concepts ctx))
		(fn [[A B]]
		  (subset? (first A) (first B)))))

(defn standard-context [lat]
  (make-context (lattice-sup-irreducibles lat)
		(lattice-inf-irreducibles lat)
		(fn [x y] 
		  ((order lat) [x y]))))

;;;

nil
