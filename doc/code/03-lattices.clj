;; Sebastian Böhm, Daniel Borchmann, 2010
;; This file is in the public domain

(require 'conexp.main)
(in-ns 'conexp.main)

;; Working with lattices

;; Now we want to work a little bit with concept-lattices, all actions
;; work also with other lattices.  First we define a lattice from a
;; given formal context ctx-1 (the chevron from the basics)
(def ctx-1 (make-context [1 2 3] [1 2 3] <=))
(def lat-1 (concept-lattice ctx-1))

;; If you want to draw the lattice (same procedure as in the basics)
;; then enter first
(use 'conexp.contrib.draw)
;; and then 
(draw-lattice lat-1)

;; To check whether the lattice is distributive use
(distributive? lat-1)

;; To get the lattice atoms use
(lattice-atoms lat-1)
;; Here you get a set of sets back. So the elements of the lattice are
;; sets of set.  In this case you get: #{ [ #{d} #{f d e}] [ #{a} #{a
;; c b f e} ] } as atoms.

;; The coatoms are
(lattice-coatoms lat-1)

;; To get the lattice one and zero use
(lattice-one lat-1)
(lattice-zero lat-1)

;; The see the infimum irreducible elemets enter
(lattice-inf-irreducibles lat-1)

;; The supremum irreducible elements are similar
(lattice-sup-irreducibles lat-1)

;; To check both (infimum and supremum irreducible elements) just use
(lattice-doubly-irreducibles lat-1)

;; Now we will take an element of our lattice and work with it.  Let's
;; choose [#{a b} #{b e}]. You get the lower- and upper-neighbours
;; with
(lattice-lower-neighbours lat-1 [#{'a 'b} #{'b 'e}])
(lattice-upper-neighbours lat-1 [#{'a 'b} #{'b 'e}])

;; If you got two elements of a lattice and want to know whether they
;; are directly neighboured use
(directly-neighboured? lat-1  [#{'a 'b} #{'b 'e}] [#{'a} #{'a 'c 'b 'f 'e}] )

;; To get the dual lattice of a given lattice lat-1 and save it as
;; lat-2 enter
(def lat-2 (dual-lattice lat-1))

;; If you want to get the standard context out of a given lattice use
(standard-context lat-1)

;;

nil
