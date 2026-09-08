;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.latdraw-test
  "Tests for the bundled LatDraw library of R. Freese, which the Freese layout
  is built on.  `conexp.layouts.freese-test` covers the layout itself; this
  covers the parts of the library reachable from it that the layout does not
  exercise."
  (:require [clojure.test :refer :all])
  (:import [org.latdraw.diagram Diagram]
           [org.latdraw.orderedset OrderedSet NonOrderedSetException]))

;; 0 < 1 < 2 < 3 with a further element 4 between 0 and 3
(def ^:private labels ["0" "1" "2" "3" "4"])
(def ^:private upper-covers [["1" "4"] ["2"] ["3"] [] ["3"]])

(defn- diagram [] (Diagram. "test" labels upper-covers))

(defn- element-labels
  [^Diagram d]
  (sort (map #(.. ^org.latdraw.diagram.Vertex % getUnderlyingElem getUnderlyingObject)
             (.getVertices d))))

(deftest test-diagram-holds-the-whole-ordered-set
  (is (= ["0" "1" "2" "3" "4"] (element-labels (diagram)))))

(deftest test-interval
  (testing "the interval between two elements holds exactly the elements
            between them.  This used to throw NoSuchElementException: the loop
            collecting the upper covers of an element tested the iterator of the
            enclosing loop instead of its own, so it ran past the end of the
            covers whenever the filter was the longer of the two."
    (let [d (diagram)]
      (are [top bottom expected]
           (= expected (element-labels (.interval d top bottom)))
        "3" "0" ["0" "1" "2" "3" "4"]
        "2" "1" ["1" "2"]
        "3" "1" ["1" "2" "3"]
        "3" "4" ["3" "4"])))
  (testing "an interval whose bounds are the wrong way round is rejected"
    (is (thrown? NonOrderedSetException (.interval (diagram) "1" "2")))))

(deftest test-cyclic-input-is-rejected
  (testing "a cycle is not an ordered set"
    (is (thrown? NonOrderedSetException
                 (OrderedSet. "cycle" ["a" "b"] [["b"] ["a"]]))))
  (testing "neither is nothing at all"
    (is (thrown? NonOrderedSetException (OrderedSet. "empty" [] [])))))

(deftest test-covers-may-be-any-collection
  (testing "the upper covers arrive as a Collection and were cast to List,
            so a set of covers used to throw"
    (is (= ["0" "1" "2" "3" "4"]
           (element-labels (Diagram. "sets" labels (map set upper-covers)))))))

;;;

nil
