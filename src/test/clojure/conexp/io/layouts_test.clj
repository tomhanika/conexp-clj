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
        conexp.io.util-test
        conexp.io.contexts
        conexp.layouts.dim-draw
        [conexp.io.util :only (tmpfile)])
  (:use clojure.test))

;;;

(def- testing-layouts
  (map (comp standard-layout concept-lattice)
       (random-contexts 20 10)))

(deftest test-layouts-oioi
  (with-testing-data [lay testing-layouts,
                      fmt (list-layout-formats)]
    (out-in-out-in-test lay 'layout fmt)))

(deftest test-layouts-oi
  (with-testing-data [lay testing-layouts,
                      fmt (remove #{:simple :text} (list-layout-formats))]
    (let [out-in-lay (out-in lay 'layout fmt)]
      ;; only test equality of lattice, positions and connections, as upper- and lower-labels are not saved in json layout format
      (and (= (poset lay) (poset out-in-lay))
           (= (positions lay) (positions out-in-lay))
           (= (connections lay) (connections out-in-lay))))))

(def- testing-layouts-with-valuations
  (map #(update-valuations % (comp count first)) testing-layouts))

(deftest test-layout-with-valuation-oi
  (with-testing-data [lay testing-layouts-with-valuations,
                      fmt (remove #{:text :simple} (list-layout-formats))]
    (= (valuations lay) (valuations (out-in lay 'layout fmt)))))

(deftest test-layout-annotations
  (with-testing-data [lay testing-layouts
                      fmt (remove #{:text} (list-layout-formats))]
    (= (annotation lay) (annotation (out-in lay 'layout fmt)))))

(deftest test-drawio-xml
  (let [ctx1 (read-context "testing-data/Forum-Romanum.ctx")
        ctx2 (read-context "testing-data/living_beings_and_water.cxt")
        ctx3 (read-context "testing-data/bodiesofwater.cxt")
        lat1 (concept-lattice ctx1)
        lat2 (concept-lattice ctx2)
        lat3 (concept-lattice ctx3)
        l1 (dim-draw-layout lat1)
        l2 (dim-draw-layout lat2)
        l3 (dim-draw-layout lat3)
        tmp (.getAbsolutePath ^java.io.File (tmpfile))]

    (write-layout :xml l1 tmp)
    (is (= (slurp tmp) (slurp "testing-data/Forum-Romanum.xml")))

    (write-layout :xml l2 tmp)
    (is (= (slurp tmp) (slurp "testing-data/Living-Beings-and-Water.xml")))

    (write-layout :xml l3 tmp)
    (is (= (slurp tmp) (slurp "testing-data/Bodies-of-Water.xml"))))
)



;;;

nil
