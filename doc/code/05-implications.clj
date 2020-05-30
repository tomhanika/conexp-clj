;; Sebastian Böhm, Daniel Borchmann, 2010
;; This file is in the public domain

(require 'conexp.main)
(in-ns 'conexp.main)

;; Consider a given context ctx-1. In this example we will we use the
;; Chevron again.
(def ctx-1 (make-context #{1 2 3 4 5 6}
                         #{1 2 3 4 5 6}
                         #{[1 1] [1 2] [1 3] [1 5]
                           [1 6] [2 2] [2 5] [3 3]
                           [3 6] [4 4] [4 5] [4 6]
                           [5 5] [6 6]}))

;; Compute and save the stem-base as sb-1 via
(def sb-1 (stem-base ctx-1))

;; Now we define the implication {d} ==> {f,e}.
(def impl-1 (make-implication #{'d} #{'f 'e}))

;; To check whether this implication holds in our context use
(holds? impl-1 ctx-1)

;; To get to know if this implication follows semantically from a set
;; of implications (here the stem-base) use
(follows-semantically? impl-1 sb-1)

;; You can start the exploration of attributes for our context with
(comment
  (explore-attributes ctx-1))

;; (Note that this function call is commented out, because it has to
;; be called interactively. Non-interactive evaluation may lead to
;; infinite reading loops.)

;;

nil
