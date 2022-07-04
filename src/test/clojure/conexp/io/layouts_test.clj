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
        conexp.layouts.base
        conexp.io.layouts
        conexp.io.util-test)
  (:use clojure.test))

;;;

(def- testing-layouts
  (map (comp standard-layout concept-lattice)
       (random-contexts 20 10)))

(deftest test-layouts-oioi
  (with-testing-data [lay testing-layouts,
                      fmt (list-layout-formats)]
    (out-in-out-in-test lay 'layout fmt)))

(def- testing-layouts-with-valuations
  (map #(update-valuations % (comp count first)) testing-layouts))

(deftest test-layout-with-valuation-oi
  (with-testing-data [lay testing-layouts-with-valuations,
                      ;; TODO: add :simple format
                      fmt (remove #{:text :simple} (list-layout-formats))]
    (= (valuations lay) (valuations (out-in lay 'layout fmt)))))

(deftest test-layout-annotations
  (with-testing-data [lay testing-layouts
                      fmt (remove #{:text} (list-layout-formats))]
    (= (annotation lay) (annotation (out-in lay 'layout fmt)))))

(def- testing-layouts-with-labels
  [(make-layout {1 [0 0], 2 [0 1]}
                 #{[1 2]}
                 {1 ["1u" nil], 2 ["2u" nil]}
                 {1 ["1l" nil], 2 ["2l" nil]})
   (make-layout {1 [0 0], 2 [0 1]}
                 #{[1 2]}
                 {1 ["1u" nil], 2 ["2u" [0 2]]}
                 {1 ["1l" [0 -1]], 2 ["2l" nil]})])

(deftest test-layout-with-labels
  (with-testing-data [lay testing-layouts-with-labels,
                      ;; TODO: add :simple format
                      fmt (remove #{:text :simple} (list-layout-formats))]
    (= lay (out-in lay 'layout fmt))))

;;;

nil
