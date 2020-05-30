;; Daniel Borchmann, 2010
;; This file is in the public domain.

;; This file shows how to use conexp-clj to factorize context (formal
;; and fuzzy ones)

(require 'conexp.main)
(in-ns 'conexp.main)

(use 'conexp.contrib.factor-analysis
     'conexp.contrib.fuzzy.logics
     'conexp.contrib.fuzzy.fca)

;;; Factorizing Formal Contexts

;; The multifunction factorize-context does the factorization of a
;; context as described by Belohlavek et. al.

(def- ctx (rand-context #{1 2 3 4 5} 0.5))

;; The first parameter gives the method to use for factorization. Here
;; :boolean-full is Algorithm 1 (i.e. computing all concepts and then
;; choosing the "good" ones) and :boolean-adaptive is Algorithm 2
;; (i.e. computing "good" concepts when needed). Note that both
;; algorithms try to remove redundant concepts afterwards as described
;; by Cynthia.

(factorize-context :boolean-full ctx)
(factorize-context :boolean-adaptive ctx)

;;; Fuzzy Factorization

;; This is actually very simple to do. First define a fuzzy context

(def- fuzzy-ctx (make-fuzzy-context [1 2 3 4]
                                    [1 2 3 4]
                                    [1/10 2/10 3/10 4/10,
                                     5/10 6/10 7/10 8/10,
                                     9/10 1    9/10 8/10,
                                     7/10 6/10 5/10 4/10]))

;; and then call factorize-context with the parameter :fuzzy. Note
;; that you need to wrap this call in with-fuzzy-logic to determine
;; the logic to use. Also see the example for fuzzy FCA for "more" details.

(with-fuzzy-logic :goedel
  (factorize-context :fuzzy
                     fuzzy-ctx
                     #{0 1/10 2/10 3/10 4/10 5/10 6/10 7/10 8/10 9/10 1}))

;;;

nil
