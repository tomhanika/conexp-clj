;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.util.fcalib
  (:use clojure.test
        [conexp.base :only (defvar- gcd with-testing-data forall set-of)]
        [conexp.fca.contexts :only (make-context diag-context adiag-context)]
        [conexp.fca.implications :only (make-implication canonical-base)]
        conexp.contrib.util.fcalib))

;;

(defvar- testing-contexts
  [(make-context [] [] [])
   (diag-context 3)
   (diag-context 10)
   (adiag-context 3)
   (adiag-context 10)
   (make-context 10 10 (fn [g m] (= 1 (gcd g m))))])

;;

(deftest test-fcalib-context-conversion
  (with-testing-data [ctx testing-contexts]
    (let [ctx (stringify-context ctx)]
      (= ctx (from-fcalib-context (to-fcalib-context ctx))))))

(deftest test-fcalib-implication-conversion
  (with-testing-data [ctx  testing-contexts
                      impl (canonical-base ctx)]
    (= impl (from-fcalib-implication (to-fcalib-implication impl)))))

(deftest test-fcalib-implication-set
  (with-testing-data [ctx testing-contexts]
    (let [ctx      (stringify-context ctx)
          impls    (canonical-base ctx)
          impl-set (to-fcalib-implication-set ctx impls)]
      (and (instance? de.tudresden.inf.tcs.fcalib.ImplicationSet impl-set)
           (forall [impl impl-set]
             (instance? de.tudresden.inf.tcs.fcaapi.FCAImplication impl))
           (= impls
              (set-of (from-fcalib-implication impl) | impl impl-set))))))

;;

nil
