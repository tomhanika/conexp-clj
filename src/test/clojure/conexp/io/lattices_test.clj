;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.lattices-test
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.lattices
        conexp.io.lattices
        conexp.io.util-test)
  (:use clojure.test))

;;;

(def- testing-lattices
  (map concept-lattice (random-contexts 20 10)))

(deftest test-lattice-oi
  (with-testing-data [lat testing-lattices,
                      fmt (list-lattice-formats)]
    (= lat (out-in lat 'lattice fmt))))

(deftest test-lattice-oioi
  (with-testing-data [lat testing-lattices,
                      fmt (list-lattice-formats)]
    (out-in-out-in-test lat 'lattice fmt)))

;;;

nil
