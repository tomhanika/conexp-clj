(ns conexp.fca.lattices
  (:use conexp.base)
  (:gen-class
   :name "conexp.fca.Lattice"
   :prefix "Lattice-"
   :init init
   :constructors { [ clojure.lang.PersistentHashSet clojure.lang.PersistentHashSet ] [] }
   :state state))

(defn Lattice-init [base-set order]
  [ [] {:base-set base-set :order order} ])

(defn base-set [lattice]
  ((.state lattice) :base-set))

(defn order [lattice]
  ((.state lattice) :order))

(defn Lattice-toString [this]
  (str "Lattice on " (count (base-set this)) " elements."))

(defn Lattice-equals [this other]
  (and (instance? conexp.fca.Lattice other)
       (= (base-set this) (base-set other))
       (= (order this) (order other))))

(defn type-of [thing]
  (cond
    (set? thing) ::set
    (fn? thing)  ::fn
    (seq? thing) ::seq
    :else        ::invalid))

(defmulti make-lattice (fn [base-set order] [(type-of base-set) (type-of order)]))

(defmethod make-lattice [::set ::set] [base-set order]
  (conexp.fca.Lattice. base-set (union (set-of [x x] [x base-set])
				       (transitive-closure order))))

(defmethod make-lattice [::set ::fn] [base-set order]
  (make-lattice base-set (set-of [x y] [x base-set y base-set :when (order x y)])))

(defmethod make-lattice :default [base-set order]
  (illegal-argument "The arguments " base-set " and " order " are not valid for a Lattice."))

(defn inf [lat x y]
  (let [order (order lat)
	base  (base-set lat)]
    (first (set-of z [z base
		      :when (and (order [z x])
				 (order [z y])
				 (forall [a base]
				   (=> (and (order [a x]) (order [a y]))
				       (order [a z]))))]))))

(defn sup [lat x y]
  (let [order (order lat)
	base  (base-set lat)]
    (first (set-of z [z base
		      :when (and (order [x z])
				 (order [y z])
				 (forall [a base]
				   (=> (and (order [x a]) (order [y a]))
				       (order [z a]))))]))))

(defn dual-lattice [lat]
  (make-lattice (base-set lat) (set-of [y x] [[x y] (order lat)])))

(defn distributive? [lat]
  (let [inf  (partial inf lat)
	sup  (partial sup lat)
	base (base-set lat)]
    (forall [x base
	     y base
	     z base]
      (= (sup x (inf y z))
	 (inf (sup x y) (sup x z))))))

(defn modular? [lat]
  (let [inf  (partial inf lat)
	sup  (partial sup lat)
	base (base-set lat)
	ordr (order lat)]
    (forall [x base
	     y base
	     z base]
      (=> (ordr [x z])
	  (= (sup x (inf y z))
	     (inf (sup x y) z))))))