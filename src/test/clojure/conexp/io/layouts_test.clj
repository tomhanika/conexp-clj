;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.layouts-test
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.lattices
        conexp.layouts
        conexp.io.layouts
        conexp.io.util-test)
  (:use clojure.test))

;;;

(def- testing-layouts
  (map (comp standard-layout concept-lattice)
       (random-contexts 20 10)))

(deftest test-layout-oioi
  (with-testing-data [lay testing-layouts,
                      fmt (list-layout-formats)]
    (try
      (out-in-out-in-test lay 'layout fmt)
      (catch IllegalArgumentException _ true))))

;;;

nil
