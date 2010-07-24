;; Copyright: Daniel Borchmann, 2010
;; This file is in the public domain.

;; This file shows some examples how to use conexp-clj for (very
;; limited) Fuzzy-FCA.

(in-ns 'user)

(use 'conexp.main
     'conexp.contrib.fuzzy)

;;; First play around with some fuzzy sets

;; This is the empty set
(defvar- fs-1 (make-fuzzy-set {}))
;; This set contains 1 with degree 0.2, 3 with degree 0.4 and so on
(defvar- fs-2 (make-fuzzy-set {1 0.2, 3 0.4, 4 0, 5 1}))
;; You may also specify ratios to make all computation exact (and you
;; can also use bigfloats for that)
(defvar- fs-3 (make-fuzzy-set {1 1/5, 2 4/7}))

;; Now ask to containment; note that fuzzy sets can be called like
;; ordinary functions
(println (fs-2 1)) ; -> 0.2
(println (fs-2 6)) ; -> 0
(println (fs-3 1)) ; -> 1/5

;; More to come (set operations and the like)

;;; Fuzzy logics

;;; TODO!

;;; Fuzzy FCA

;; Let's create a fuzzy context
(defvar- fuzzy-context
  (make-fuzzy-context [1 2 3 4] ; vector of objects
                      [1 2 3 4] ; vector of attributes
                      [0.1 0.2 0.3 0.4, ; vector of entries
                       0.5 0.6 0.7 0.8, ; , is a whitespace
                       0.9 1.0 0.9 0.8,
                       0.7 0.6 0.5 0.4]))

;;;

nil
