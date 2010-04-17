;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.tests.fca.lattices
  (:use conexp.base
        conexp.fca.lattices)
  (:use clojure.test))

;;;

(deftest test-make-lattice
  (is (make-lattice #{1 2 3 4} <=))
  (is (make-lattice (subsets #{1 2 3 4}) subset?))
  (is (make-lattice [1 2 3 4] [[1 2] [3 4] [5 6]]))
  (is (thrown? IllegalArgumentException (make-lattice 1 2 3)))
  (is (thrown? IllegalArgumentException (make-lattice {1 2 3 4} {5 6 7 8} <=))))

(deftest test-Lattice-equals
  (is (= (make-lattice #{} #{}) (make-lattice #{} #{})))
  (is (= (make-lattice #{} min max) (make-lattice #{} min max)))
  (is (= (make-lattice [1 2 3 4] min max) (make-lattice [4 3 2 1] min max)))
  (is (not= (make-lattice [1 2 3 4] min max) (make-lattice [1 2 3 4] max min))))

(deftest test-Lattice-hashCode)

;;;

nil
