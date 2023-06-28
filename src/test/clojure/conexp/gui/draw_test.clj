;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.draw-test
  (:use conexp.fca.contexts
        conexp.fca.lattices
        conexp.gui.draw)
  (:use clojure.test))

(def test-context
  (rand-context 4 0.5))

(def test-lattice
  (concept-lattice test-context))

(defn- mock-draw-layout
  "Mock the draw-layout function, so that the GUI is not shown."
  [layout & args]
  {:frame nil :scene nil})

(deftest test-draw-lattice
  "Check that draw-lattice does not throw an exception."
  (with-redefs [draw-layout mock-draw-layout]
    (let [result (draw-lattice test-lattice)]
      (is (= result result)))))

(deftest test-draw-protoconcepts
  "Check that draw-protoconcepts does not throw an exception."
  (with-redefs [draw-layout mock-draw-layout]
    (let [result (draw-protoconcepts test-lattice)]
      (is (= result result)))))
