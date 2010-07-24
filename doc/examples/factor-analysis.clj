;; Copyright: Daniel Borchmann, 2010
;; This file is in the public domain.

;; This file shows how to use conexp-clj to factorize context (formal
;; and fuzzy ones)

(in-ns 'user)

(use 'conexp.main
     'conexp.contrib.factor-analysis
     'conexp.contrib.fuzzy.logics
     'conexp.contrib.fuzzy.fca)

;;; Factorizing Formal Contexts

;; The multifunction factorize-context does the factorization of a
;; context as described by Belohlavek et. al.

(defvar- ctx (rand-context #{1 2 3 4 5} 0.5))

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

(defvar- fuzzy-ctx (make-fuzzy-context [1 2 3 4]
                                       [1 2 3 4]
                                       [0.1 0.2 0.3 0.4,
                                        0.5 0.6 0.7 0.8,
                                        0.9 1.0 0.9 0.8,
                                        0.7 0.6 0.5 0.4]))

;; and then call factorize-context with the parameter :fuzzy. Note
;; that you need to wrap this call in with-fuzzy-logic to determine
;; the logic to use. Also see the example for fuzzy FCA for "more" details.

(with-fuzzy-logic :goedel
  (factorize-context :fuzzy fuzzy-ctx))

;;;

nil
