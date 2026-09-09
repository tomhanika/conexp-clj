;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.gcanvas-buttons-test
  "Tests the translation from AWT mouse events to GWindow button constants.

  The lattice editor cannot be clicked from a test, so the mapping is exercised
  on synthetic events instead.  It used to read the deprecated modifier mask and
  compare it for equality, which reported any click with a modifier held as a
  button 3 click."
  (:require [clojure.test :refer :all])
  (:import [java.awt.event InputEvent MouseEvent]
           [javax.swing JPanel]))

;; a lightweight component, so the events can be built without a display

(def ^:private button-event-of
  (doto (.getDeclaredMethod no.geosoft.cc.graphics.GCanvas "buttonEventOf"
                            (into-array Class [MouseEvent Integer/TYPE
                                               Integer/TYPE Integer/TYPE]))
    (.setAccessible true)))

(def ^:private dragged-button-event-of
  (doto (.getDeclaredMethod no.geosoft.cc.graphics.GCanvas "draggedButtonEventOf"
                            (into-array Class [MouseEvent Integer/TYPE
                                               Integer/TYPE Integer/TYPE]))
    (.setAccessible true)))

(defn- press
  "A press event for `button`, with `modifiers` also held."
  [button modifiers]
  (MouseEvent. (JPanel.) MouseEvent/MOUSE_PRESSED 0 modifiers 10 10 1 false button))

(defn- drag
  "A drag event with `modifiers` held; a drag reports no button of its own."
  [modifiers]
  (MouseEvent. (JPanel.) MouseEvent/MOUSE_DRAGGED 0 modifiers 10 10 0 false
               MouseEvent/NOBUTTON))

(defn- mapped [^java.lang.reflect.Method m event]
  (.invoke m nil (into-array Object [event (int 1) (int 2) (int 3)])))

(deftest test-press-and-release-map-by-button
  (are [button expected] (= expected (mapped button-event-of (press button 0)))
    MouseEvent/BUTTON1 1
    MouseEvent/BUTTON2 2
    MouseEvent/BUTTON3 3))

(deftest test-a-held-modifier-does-not-change-the-button
  (testing "shift, control and alt held during the click.  Comparing the whole
            modifier mask for equality used to report all of these as button 3."
    (doseq [modifier [InputEvent/SHIFT_DOWN_MASK
                      InputEvent/CTRL_DOWN_MASK
                      InputEvent/ALT_DOWN_MASK]]
      (is (= 1 (mapped button-event-of (press MouseEvent/BUTTON1 modifier)))
          (str "button 1 with modifier " modifier))
      (is (= 2 (mapped button-event-of (press MouseEvent/BUTTON2 modifier)))
          (str "button 2 with modifier " modifier)))))

(deftest test-drag-maps-by-the-button-held
  (are [modifiers expected] (= expected (mapped dragged-button-event-of (drag modifiers)))
    InputEvent/BUTTON1_DOWN_MASK 1
    InputEvent/BUTTON2_DOWN_MASK 2
    InputEvent/BUTTON3_DOWN_MASK 3
    (bit-or InputEvent/BUTTON1_DOWN_MASK InputEvent/SHIFT_DOWN_MASK) 1
    (bit-or InputEvent/BUTTON2_DOWN_MASK InputEvent/CTRL_DOWN_MASK) 2
    0 3))

;;;

nil
