;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.draw.control.dim-flux-test
  (:require [clojure.test :refer :all]
            [conexp.fca.contexts :refer [make-context-from-matrix]]
            [conexp.fca.lattices :refer [concept-lattice]]
            [conexp.gui.draw.control.dim-flux]
            [conexp.layouts.base :as lay]
            [conexp.layouts.dim-flux :refer [dim-flux-layout]]))

(def ^:private starting-diagrams
  @#'conexp.gui.draw.control.dim-flux/starting-diagrams)

(def ^:private test-lattice
  (concept-lattice
   (make-context-from-matrix ["Ceres" "Pluto" "Eris" "Haumea" "Makemake"]
                             ["asteroid-belt" "kuiper-belt" "scattered-disc" "moons"]
                             [1 0 0 0
                              0 1 0 1
                              0 0 1 1
                              0 1 0 1
                              0 1 0 0])))

(deftest test-starting-diagrams-are-offered-in-full
  (testing "the editor offers every starting diagram the layout knows"
    (is (= #{:dim-draw :greedy :planarity-enhancer :layered}
           (set (vals starting-diagrams)))))
  (testing "and offers the published algorithm first, as the default selection"
    (is (= :dim-draw (second (first starting-diagrams))))))

(deftest test-every-offered-starting-diagram-draws
  (testing "each entry of the menu is a keyword the layout actually accepts,
            which a typo in either place would break"
    (doseq [[label initial] starting-diagrams]
      (let [layout (dim-flux-layout test-lattice {:initial initial :iterations 10})
            pos    (lay/positions layout)]
        (is (seq pos) (str label " draws nothing"))
        (is (every? (fn [[a b]] (< (second (pos a)) (second (pos b))))
                    (lay/connections layout))
            (str label " does not give a line diagram"))))))

;;;

nil
