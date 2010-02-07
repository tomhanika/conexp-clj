(ns conexp.fca.lattices
  (:use conexp.base
	conexp.fca.contexts)
  (:gen-class
   :name "conexp.fca.Lattice"
   :prefix "Lattice-"
   :init init
   :constructors { [ Object Object ] []            ; relational lattices (<=)
		   [ Object Object Object ] [] }   ; functional lattices (inf, sup)
   :state state))

;;; Datastructure

(defn Lattice-init
  ([base-set order]
     [ [] {:base-set base-set, :order order, :inf nil, :sup nil} ])
  ([base-set inf sup]
     [ [] {:base-set base-set, :order nil,   :inf inf, :sup sup} ]))

(defn base-set [#^conexp.fca.Lattice lattice]
  ((.state lattice) :base-set))

(defn order [#^conexp.fca.Lattice lattice]
  (or ((.state lattice) :order)
      (let [sup ((.state lattice) :sup)]
	(fn [[x y]] (= y (sup x y))))))

(defn inf [#^conexp.fca.Lattice lattice]
  (or ((.state lattice) :inf)
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

(defn sup [#^conexp.fca.Lattice lattice]
  (or ((.state lattice) :sup)
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

(defn Lattice-toString [this]
  (str "Lattice on " (count (base-set this)) " elements."))

(defn Lattice-equals [this other]
  (and (instance? conexp.fca.Lattice other)
       (= (base-set this) (base-set other))
       (let [order-this (order this)
	     order-other (order other)]
	 (or (= order-this order-other)
	     (forall [pair (cross-product (base-set this) (base-set this))]
	       (<=> (order-this pair) (order-other pair)))))))

(defn Lattice-hashCode
  "Implements hashCode for lattices."
  [#^conexp.fca.Lattice this]
  (let [base-set (base-set this)
	order    ((.state this) :order)
	inf      ((.state this) :inf)
	sup      ((.state this) :sup)]
    (bit-xor (hash base-set)
	     (if order
	       (hash order)
	       (bit-xor (hash inf)
			(hash sup))))))

(defn type-of [thing]
  (cond
    (set? thing) ::set
    (fn? thing)  ::fn
    (seq? thing) ::seq
    :else        ::invalid))

(defmulti make-lattice (fn [& args] (vec (map type-of args))))

(defmethod make-lattice [::set ::set] [base-set order]
  (conexp.fca.Lattice. base-set order))

(defmethod make-lattice [::set ::fn] [base-set order]
  (conexp.fca.Lattice. base-set order))

(defmethod make-lattice [::set ::fn ::fn] [base-set inf sup]
  (conexp.fca.Lattice. base-set inf sup))

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

nil
