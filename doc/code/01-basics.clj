;; Sebastian Böhm, Daniel Borchmann, 2010
;; This file is in the public domain

(require 'conexp.main)
(in-ns 'conexp.main)

;; How to create a context?

;; A context (G,M,I) consists of two sets G and M and an incidence
;; realtion I between G and M.  G is a set of objects, M is a set of
;; attributes and gIm can be read as g has the attribute m.  To create
;; a context you only have to define these sets. There a (at least)
;; three options:

;; 1. By using a defined relation:
(def ctx-1 (make-context [1 2 3] [1 2 3] <=))

;; 2. By defining all sets explicitly:

;; This is the ordinale scale for the Chevron
;;
;;       5     6
;;       | \ / |
;;       |  4  |
;;       2     3
;;         \ /
;;          1
;;
(def ctx-2 (make-context #{1 2 3 4 5 6}
                         #{1 2 3 4 5 6}
                         #{[1 1] [1 2] [1 3] [1 5]
                           [1 6] [2 2] [2 5] [3 3]
                           [3 6] [4 4] [4 5] [4 6]
                           [5 5] [6 6]}))

(def ctx-3 (make-context-from-matrix 3 3
                                     [0 0 1
                                      1 0 1
                                      1 1 0]))

;; 3. By using a custom function
(def ctx-4 (make-context [1 2 3 4 5 6]
                         [1 2 3 4 5 6]
                         (fn [x y]
                           (= 1 (gcd x y)))))

;; If you want to use a random context you can use rand-context
(rand-context #{1 2 3} 0.5)

;; The first parameter is the set of objects and attributes and the
;; second parameter is the probality for the incidence.


;; How to compute all formal concepts?

;; First we define a context ctx-1:
(def ctx-1 (make-context-from-matrix ['a 'b 'c 'd 'e 'f]
                                     ['a 'b 'c 'd 'e 'f]
                                     [1 1 1 0 1 1
                                      0 1 0 0 1 0
                                      0 0 1 0 0 1
                                      0 0 0 1 1 1
                                      0 0 0 0 1 0
                                      0 0 0 0 0 1]))

;; This ist the Chevron again, but this time with a instead of 1, b
;; instead of 2 etc. With ' (quote) you can use symbols in conexp.

;; Now I want to get all objects, which attribute a and b have in
;; common
(attribute-derivation ctx-1 #{'a 'b})

;; Note that the argument is a set. This also works for the objects:
(object-derivation ctx-1 #{'c 'd 'f})

;; If you want to compute the closure of a given set of objects or
;; attributes in our context use
(context-attribute-closure ctx-1 #{'a 'b})
(context-object-closure ctx-1 #{'a 'b})

;; Now we can compute the arrow-relations (e.g. to see which objects
;; or attributes are irreducible.)
(up-arrows ctx-1)
(down-arrows ctx-1)

;; To get all formal concepts use
(concepts ctx-1)

;; To see how many formal concepts you get, you can write:
(count (concepts ctx-1))

;; You can compute the concept lattice via
(concept-lattice ctx-1) 

;; To see the diagram of the concept-lattice enter first
(use 'conexp.contrib.draw)

;; and then you can draw the lattice with
(draw-lattice (concept-lattice ctx-1))

;; You get the stem base with
(stem-base ctx-1)

;;

nil
