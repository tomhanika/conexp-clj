;; Sebastian Böhm, Daniel Borchmann, 2010
;; This file is in the public domain

(require 'conexp.main)
(in-ns 'conexp.main)

;; Working with Formal Contexts

;; Now we want to work with a given context. In the following we want
;; to use this context
(def ctx-2 (make-context-from-matrix [1 2 3]
                                     [1 2 3 4]
                                     [1 1 0 0
                                      1 1 1 0
                                      0 0 1 1]))

;; First we want to check wether object- or attribute-sets are
;; clarified. That means that there are no identical colums or rows in
;; our context.
(object-clarified? ctx-2) 
(attribute-clarified? ctx-2)

;; To chech both in one step use
(context-clarified? ctx-2)

;; You can clarify the attribute set or the object set or the complete
;; context with the following functions
(clarify-attributes ctx-2)
(clarify-objects ctx-2)
(clarify-context ctx-2)

;;Because we want to save the resulting context we take
(def ctx-3 (clarify-context ctx-2))

;; The next step ist to reduce our clarified context. First we check
(context-reduced? ctx-2)

;; then we reduce, that means all reducible objects and attributes
;; will be deleted.  Note that objects with same object intent are
;; kept, likewise for attributes.
(reduce-context ctx-2)

;; Now let's take the Chevron from the basics part again as ctx-1.
(def ctx-1 (make-context [1 2 3] [1 2 3] <=))

;; We can compute all intents and extents via
(extents ctx-1)
(intents ctx-1)

;; There a several operations you can do with contexts, e.g. the
;; apposition. We define two contexts:
(def ctx-1 (make-context #{1 2 3} #{1 2 3} < ))
(def ctx-2 (make-context-from-matrix [1 2 3] ['a 'b 'c 'd] [1 1 0 1
                                                            1 0 1 0
                                                            0 0 1 1]))

;; The apposition of these two contexts is
(context-apposition ctx-1 ctx-2)

;; To compute the dual context (and save it as ctx-3) we use
(def ctx-3 (dual-context ctx-2))

;; Now we can build the subposition of context 1 and context 3:
(context-subposition ctx-1 ctx-3)

;; If you want to invert a given context use
(invert-context ctx-1)

;; You can create a composition of two contexts with
(context-composition ctx-1 ctx-2)

;; The union of two contexts is created by
(context-union ctx-1 ctx-2)

;; Sum to contexts with
(context-sum ctx-1 ctx-2)

;; To compute the intersection of two contexts use
(context-intersection ctx-1 ctx-2)

;; The context product goes like this
(context-product ctx-1 ctx-2)

;; If you want to do a context semiproduct 
(context-semiproduct ctx-1 ctx-2)

;; Compute Xia's product 
(context-xia-product ctx-1 ctx-2)

;;
nil
