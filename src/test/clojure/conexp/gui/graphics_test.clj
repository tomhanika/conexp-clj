;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.graphics-test
  "Tests for the bundled G graphics library the lattice editor draws on.

  The editor cannot be driven from a test, but a scene can be built and
  searched without a display, which covers the object and segment collections
  and the picking methods that walk them."
  (:require [clojure.test :refer :all])
  (:import [no.geosoft.cc.graphics GObject GScene GSegment GWindow]))

(defn- scene-with-a-square
  "A scene holding one object with a single square segment from (0,0) to
  (20,20), in a world 100 by 100."
  []
  (let [window  (GWindow.)
        scene   (GScene. window "test")
        object  (GObject. "square")
        segment (GSegment.)]
    (.add scene object)
    (.addSegment object segment)
    (.setGeometry segment (int-array [0 0 20 0 20 20 0 20 0 0]))
    (.setWorldExtent scene 0.0 0.0 100.0 100.0)
    (.setSize (.getCanvas window) 100 100)
    (.refresh scene)
    [scene object segment]))

(deftest test-object-holds-its-segments-and-children
  (let [[_ object segment] (scene-with-a-square)
        child (GObject. "child")]
    (is (= [segment] (vec (.getSegments object))))
    (.add object child)
    (is (= [child] (vec (.getChildren object))))
    (.remove object child)
    (is (empty? (.getChildren object)))))

(deftest test-picking-a-point
  (testing "a point on the outline finds the segment and its object"
    (let [[_ object segment] (scene-with-a-square)]
      (is (= [segment] (vec (.findSegments object 10 0))))
      (is (= [object] (vec (.findAll object 10 0))))))
  (testing "a point well away from it finds nothing"
    (let [[_ object _] (scene-with-a-square)]
      (is (empty? (.findSegments object 90 90)))
      (is (empty? (.findAll object 90 90))))))

(deftest test-picking-a-rectangle
  (let [[_ object segment] (scene-with-a-square)]
    (testing "a rectangle meeting the outline finds it"
      (is (= [segment] (vec (.findSegments object 0 0 30 30)))))
    (testing "a rectangle missing it does not"
      (is (empty? (.findSegments object 60 60 90 90))))))

;;;

nil
