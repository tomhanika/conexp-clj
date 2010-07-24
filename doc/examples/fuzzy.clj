;; Copyright: Daniel Borchmann, 2010
;; This file is in the public domain.

;; This file shows some examples how to use conexp-clj for (very
;; limited) Fuzzy-FCA.

(in-ns 'user)

(use 'conexp.main
     'conexp.contrib.fuzzy.sets
     'conexp.contrib.fuzzy.logics
     'conexp.contrib.fuzzy.fca)

;;; First play around with some fuzzy sets

;; This is the empty set

(defvar- fs-1 (make-fuzzy-set {}))

;; This set contains 1 with degree 0.2, 3 with degree 0.4 and so on

(defvar- fs-2 (make-fuzzy-set {1 0.2, 3 0.4, 4 0, 5 1}))

;; You may also specify ratios to make all computation exact (and you
;; can also use bigfloats for that)

(defvar- fs-3 (make-fuzzy-set {1 1/5, 2 4/7}))

;; Now you can ask for containment; note that fuzzy sets can be called
;; like ordinary functions

(println (fs-2 1)) ; -> 0.2
(println (fs-2 6)) ; -> 0
(println (fs-3 1)) ; -> 1/5

;; More to come (set operations and the like)

;;; Fuzzy logics

;; Fuzzy logics come from t-norm and conexp-clj defines 3 norms:
;; Lukasiewicz, Goedel and Product t-norm. They can be used to rebind
;; the fuzzy operators f-star, f-impl, f-and, f-or and f-neg to their
;; corresponding functions. This is done using the with-fuzzy-logic
;; macro

(with-fuzzy-logic :product
  (println (f-impl (f-and 1.0 0.4) (f-or 0.4 0.234))))

;; That's it actually. Whenever an operation uses fuzzy operators just
;; make sure that a call to this operation is wrapped in a
;; with-fuzzy-logic call.

;;; Fuzzy FCA

;; Let's create a fuzzy context

(defvar- fuzzy-context
  (make-fuzzy-context [1 2 3 4] ; vector of objects
                      [1 2 3 4] ; vector of attributes
                      [0.1 0.2 0.3 0.4, ; vector of entries
                       0.5 0.6 0.7 0.8, ; , is a whitespace
                       0.9 1.0 0.9 0.8,
                       0.7 0.6 0.5 0.4]))

;; The only thing you can do by now is to compute fuzzy derivation as
;; used by Belohlavek and Vychodil

(with-fuzzy-logic :lukasiewicz
  (println
   (fuzzy-attribute-derivation fuzzy-context
                               (make-fuzzy-set {2 0.3, 4 0.8}))))

;; Note that you can also directly use hashmaps here, since the
;; derivation operations automatically convert them to fuzzy sets if
;; possible

(with-fuzzy-logic :goedel
  (println
   (fuzzy-attribute-derivation fuzzy-context {2 0.3, 4 0.8})))

;; If you don't like rounding errors and you are willing to wait a bit
;; longer you can also use rationals (i.e. 1/2 instead of 0.5) or
;; BigFloats (i.e. 0.5M instead of 0.5) when specifying the fuzzy
;; context.

;;;

nil
