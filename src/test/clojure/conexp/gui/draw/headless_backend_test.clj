;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.draw.headless-backend-test
  "Proves the drawing code is decoupled from the Swing backend: a real lattice
  diagram is constructed through the Backend / AbstractScene / AbstractNode
  protocols using the Swing-free headless backend, with no GUI object created."
  (:require [clojure.test :refer [deftest is testing]]
            [conexp.fca.contexts :refer [make-context-from-matrix]]
            [conexp.fca.lattices :refer [concept-lattice]]
            [conexp.layouts :refer [standard-layout]]
            [conexp.layouts.base :refer [positions connections]]
            [conexp.gui.draw.backend :as backend]
            [conexp.gui.draw.headless-backend :refer [headless-backend]]
            [conexp.gui.draw.nodes-and-connections :refer [node? connection?]]
            [conexp.gui.draw.scene-layouts :refer [draw-on-scene
                                                   get-diagram-from-scene
                                                   get-layout-from-scene]])
  (:import [conexp.gui.draw.headless_backend DataScene DataNode]))

(deftest test-headless-backend-decoupling
  (let [ctx (make-context-from-matrix [0 1 2] [0 1 2] [1 0 0, 1 1 0, 1 1 1])
        lat (concept-lattice ctx)
        lay (standard-layout lat)]
    (binding [backend/*backend* headless-backend]
      (let [scn      (draw-on-scene lay)
            children (get-diagram-from-scene scn)]
        (testing "the diagram is built with the headless backend -- no Swing objects"
          (is (instance? DataScene scn))
          (is (every? #(instance? DataNode %) children)))
        (testing "the diagram has the right nodes and connections"
          (is (= (count (positions lay))   (count (filter node? children))))
          (is (= (count (connections lay)) (count (filter connection? children)))))
        (testing "the layout round-trips through the headless scene"
          (is (= (set (keys (positions lay)))
                 (set (keys (positions (get-layout-from-scene scn)))))))))))

;;;

nil
