;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.fuzzy.fca-test
  (:use [conexp.base]
        conexp.fca.contexts
        conexp.fca.many-valued-contexts
        conexp.fca.fuzzy.sets
        conexp.fca.fuzzy.fca)
  (:use clojure.test))

;;;
(def- mvctx (make-mv-context [1 2 3 4]
                             [1 2 3 4 5 6]
                             #{[1 1 1.0]
                               [1 2 1.0]
                               [1 3 0.0]
                               [1 4 1.0]
                               [1 5 1.0]
                               [1 6 0.2]
                               [2 1 1.0]
                               [2 2 0.4]
                               [2 3 0.3]
                               [2 4 0.8]
                               [2 5 0.5]
                               [2 6 1.0]
                               [3 1 0.2]
                               [3 2 0.9]
                               [3 3 0.7]
                               [3 4 0.5]
                               [3 5 1.0]
                               [3 6 0.6]
                               [4 1 1.0]
                               [4 2 1.0]
                               [4 3 0.8]
                               [4 4 1.0]
                               [4 5 1.0]
                               [4 6 0.5]}))

(def- fctx (make-fuzzy-context [1 2 3 4]
                               [1 2 3 4 5 6]
                               [1.0 1.0 0.0 1.0 1.0 0.2,
                                1.0 0.4 0.3 0.8 0.5 1.0,
                                0.2 0.9 0.7 0.5 1.0 0.6,
                                1.0 1.0 0.8 1.0 1.0 0.5]))

(println fctx)

(deftest test-mv-to-fuzzy

  (is (= (mv->fuzzy-context-nc mvctx) fctx))
)

(deftest test-fuzzy-derivation
  (let [fset1 (make-fuzzy-set #{1 2})
        fset2 (make-fuzzy-set {3 0.6 4 0.4})
        fset3 (make-fuzzy-set [5 6])]

    (is (= (fuzzy-object-derivation fctx fset1 lukasiewicz-norm)
           (make-fuzzy-set {1 1.0 4 0.8 6 0.19999999999999996 2 0.3999999999999999 5 0.5})))
    (is (= (fuzzy-object-derivation fctx fset2 goedel-norm)
           (make-fuzzy-set {1 0.2 4 0.5 6 1 3 1 2 1 5 1})))
    (is (= (fuzzy-attribute-derivation fctx fset3 product-norm)
           (make-fuzzy-set {1 0.2 4 0.5 3 0.6 2 0.5}))))
)


(deftest test-fuzzy-implications
  (let [fset1 (make-fuzzy-set {1 1})
        fset2 (make-fuzzy-set {2 1})
        fset3 (make-fuzzy-set {1 1 2 1})
        fset4 (make-fuzzy-set {4 1})
        fset5 (make-fuzzy-set {1 0.2 2 0.9})
        fset6 (make-fuzzy-set {3 1})]

    (is (= (validity fctx fset1 fset2 product-norm) 0.4))
    (is (= (validity fctx fset3 fset4 product-norm) 1))
    (is (= (validity fctx fset5 fset6 product-norm) 0))
    )
  )
