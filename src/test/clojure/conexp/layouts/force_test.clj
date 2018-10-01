;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.force-test
  (:use conexp.fca.contexts
        conexp.fca.lattices
        conexp.layouts.layered
        conexp.layouts.force)
  (:use clojure.test))

;;;

(deftest test-layout-energy
  (is (pos? (layout-energy
             (simple-layered-layout (concept-lattice
                                     (rand-context [1 2 3 4] 0.5)))))))

(deftest test-force-layout
  (let [lattice (concept-lattice (make-context-from-matrix 5 5
                                                           [0 1 1 0 0
                                                            1 0 1 1 0
                                                            1 1 0 1 1
                                                            0 0 1 1 1
                                                            0 1 1 0 0])),
        layout  (simple-layered-layout lattice),
        layouts (take 10 (iterate #(force-layout % 100) layout))]
    (is (apply > (map layout-energy layouts)))))

;;;

nil
