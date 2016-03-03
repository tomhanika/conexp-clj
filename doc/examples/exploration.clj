;; Daniel Borchmann, 2010
;; This file is in the public domain.

;; This example file shows how to use conexp-clj to explore formal
;; contexts

(require 'conexp.main)
(in-ns 'conexp.main)

;;;

;;; Let us start with some simple (and vapid) example
;;;
;;; To explore a context without any other features, just call
;;; «explore-attributes» with the corresponding context as its only
;;; argument

(def- ctx (make-context-from-matrix '[a b c]
                                    [1 2 3]
                                    [0 1 0
                                     1 1 0
                                     1 0 1]))
(explore-attributes ctx)

;;; Exploration now proceeds as follows:
;;;
;;; user=> (explore-attributes ctx)
;;; Does the implication (#{3}  ==>  #{1}) hold? no
;;; counterexample> object
;;; Please enter new object: d
;;; counterexample> attributes
;;; Please enter the attributes the new object should have: 3
;;; counterexample> q
;;; Do you want to give another counterexample? no
;;; Does the implication (#{2 3}  ==>  #{1}) hold? yes
;;; {:implications #{(#{2 3}  ==>  #{1})}, :context   |1 2 3
;;;  --+------
;;;  a |. x .
;;;  b |x x .
;;;  c |x . x
;;;  d |. . x
;;;  }
;;; user=>
;;;
;;; The result returned by explore-attributes is a hash-map with the
;;; implications found, together with the resulting context.

;;; It is also possible to add background knowledge to the
;;; exploration. Just give, as second argument, a set of valid
;;; implications.

(explore-attributes ctx :background-knowledge #{(make-implication #{3} #{1})})

;;; This "shortens" the exploration a bit:
;;;
;;; user=> (explore-attributes ctx :background-knowledge #{(make-implication #{3} #{1})})
;;; {:implications #{}, :context   |1 2 3
;;;  --+------
;;;  a |. x .
;;;  b |x x .
;;;  c |x . x
;;;  }
;;; user=>
;;;
;;; Note that the given implications are not part of the returned
;;; implications.

;;; Finally, you can control the way the exploration handles
;;; interaction with a custom handler function. This functions is
;;; called whenever an expert has to be asked, getting as its
;;; arguments the current context, all heretofore known implications
;;; and the current implication. Standardly, the function
;;; «default-handler» is used, which implements low level
;;; communication via the command line.

;;; Handlers have to return specific values to indicate approval or
;;; rejection of a given implication. The format of the return value
;;; is as follows:
;;;
;;;   On success: nil
;;;   On failure: sequence of [«a new object» «a sequence of its attributes»]
;;;
;;; With that, you could immitate «stem-base» with explore-attributes
;;; via

(= (:implications (explore-attributes ctx :handler (constantly nil)))
   (stem-base ctx))

;;; and indeed this is one of the test cases for explore-attributes.

;;;

nil
