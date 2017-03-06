;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.draw.buttons
 "Provides simple helper functions to create the buttons of the lattice editor."
  (:import [javax.swing JPanel JButton JTextField JLabel
                        JSeparator SwingConstants Box JComboBox
                        JSlider SpinnerNumberModel JSpinner]
           [java.awt Dimension Component]))

;;;

(def *item-width*
  "Width of items in toolbar."
  100)

(def *item-height*
  "Heights of items on toolbar."
  25)

(def *toolbar-width*
  "Width of toolbar containing buttons, labels and so on."
  (+ 20 *item-width*))

(defn make-padding
  "Adds a padding to buttons."
  [buttons]
  (.add buttons (Box/createRigidArea (Dimension. 0 2))))

(defn make-separator
  "Adds a separator to buttons."
  [buttons]
  (let [sep (JSeparator. SwingConstants/HORIZONTAL)]
    (.setMaximumSize sep (Dimension. *item-width* 1))
    (.add buttons sep)
    sep))

(defn make-button
  "Uniformly creates buttons for lattice editor."
  [buttons text]
  (let [button (JButton. text)]
    (.add buttons button)
    (.setAlignmentX button Component/CENTER_ALIGNMENT)
    (.setMaximumSize button (Dimension. *item-width* *item-height*))
    button))

(defn make-label
  "Uniformly creates labels for lattice editor."
  [buttons text]
  (let [label (JLabel. text)]
    (.add buttons label)
    (.setMaximumSize label (Dimension. *item-width* *item-height*))
    (.setAlignmentX label Component/CENTER_ALIGNMENT)
    (.setHorizontalAlignment label SwingConstants/CENTER)
    label))

(defn make-labeled-text-field
  "Uniformly creates a text field for lattice editor."
  [buttons label text]
  (let [^JTextField text-field (JTextField. text),
        ^JLabel label (JLabel. label),
        ^JPanel panel (JPanel.)]
    (doto panel
      (.add label)
      (.add text-field)
      (.setMaximumSize (Dimension. *item-width* *item-height*)))
    (.setPreferredSize label (Dimension. (* 0.4 *item-width*) (* 0.8 *item-height*)))
    (.setPreferredSize text-field (Dimension. (* 0.5 *item-width*) (* 0.8 *item-height*)))
    (.add buttons panel)
    text-field))

(defn make-combo-box
  "Uniformly creates a combo box from the given choices. First item is
  selected by default."
  [buttons choices]
  (let [^JComboBox combo-box (JComboBox. (into-array String choices))]
    (doto combo-box
      (.setMaximumSize (Dimension. *item-width* *item-height*)))
    (.add buttons combo-box)
    combo-box))

(defn make-slider
  "Uniformly creates a slider."
  [buttons min max init]
  (let [^JSlider slider (JSlider. JSlider/HORIZONTAL (int min) (int max) (int init))]
    (.setMaximumSize slider (Dimension. *item-width* *item-height*))
    (.add buttons slider)
    slider))

(defn make-spinner
  "Uniformly creates a spinner."
  [buttons min max init step]
  (let [^SpinnerNumberModel
        model (SpinnerNumberModel. (double init)
                                   (double min)
                                   (double max)
                                   (double step)),
        ^JSpinner
        spinner (JSpinner. model)]
    (.setMaximumSize spinner (Dimension. *item-width* *item-height*))
    (.add buttons spinner)
    spinner))

;;;

nil
