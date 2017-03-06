;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.draw.control.util
  (:use [conexp.base :exclude (select)]
        conexp.contrib.gui.util)
  (:use seesaw.core)
  (:import [javax.swing JPanel JButton JTextField JLabel
                        JSeparator SwingConstants Box JComboBox
                        JSlider SpinnerNumberModel JSpinner
                        BoxLayout JFrame JComponent]
           [java.awt Dimension Component event.ActionEvent]))

;;;

(def ^:dynamic *item-width*
  "Width of items in toolbar."
  100)

(def ^:dynamic *item-height*
  "Heights of items on toolbar."
  25)

(def ^:dynamic *toolbar-width*
  "Width of toolbar containing buttons, labels and so on."
  (+ 20 *item-width*))

;;;

(defn make-padding
  "Adds a padding to buttons."
  [^JComponent buttons]
  (.add buttons (Box/createRigidArea (Dimension. 0 2))))

(defn make-separator
  "Adds a separator to buttons."
  [^JComponent buttons]
  (let [sep (JSeparator. SwingConstants/HORIZONTAL)]
    (.setMaximumSize sep (Dimension. *item-width* 1))
    (.add buttons sep)
    sep))

(defn make-button
  "Uniformly creates buttons for lattice editor."
  [^JComponent buttons, ^String text]
  (let [button (JButton. text)]
    (.add buttons button)
    (.setAlignmentX button Component/CENTER_ALIGNMENT)
    (.setMaximumSize button (Dimension. *item-width* *item-height*))
    button))

(defn make-label
  "Uniformly creates labels for lattice editor."
  [^JComponent buttons, ^String text]
  (let [label (JLabel. text)]
    (.add buttons label)
    (.setMaximumSize label (Dimension. *item-width* *item-height*))
    (.setAlignmentX label Component/CENTER_ALIGNMENT)
    (.setHorizontalAlignment label SwingConstants/CENTER)
    label))

(defn make-labeled-text-field
  "Uniformly creates a text field for lattice editor."
  [^JComponent buttons, ^String label, ^String text]
  (let [^JTextField text-field (JTextField. text),
        ^JLabel label (JLabel. label),
        ^JPanel panel (JPanel.)]
    (doto panel
      (.setLayout (BoxLayout. panel BoxLayout/X_AXIS))
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
  [^JComponent buttons, choices]
  (let [^JComboBox combo-box (JComboBox. ^"[Ljava.lang.String;" (into-array String choices))]
    (doto combo-box
      (.setMaximumSize (Dimension. *item-width* *item-height*)))
    (.add buttons combo-box)
    combo-box))

(defn make-slider
  "Uniformly creates a slider."
  [^JComponent buttons, min max init]
  (let [^JSlider slider (JSlider. JSlider/HORIZONTAL (int min) (int max) (int init))]
    (.setMaximumSize slider (Dimension. *item-width* *item-height*))
    (.add buttons slider)
    slider))

(defn make-spinner
  "Uniformly creates a spinner."
  [^JComponent buttons, min max init step]
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

(defn make-panel
  "Uniformly creates a panel."
  [^JComponent buttons]
  (let [^JPanel panel (JPanel.)]
    (.setMaximumSize panel (Dimension. *item-width* *item-height*))
    (.add buttons panel)
    panel))

;;;

(defn control-choice
  "Creates a choice control in buttons from given choices.
  The choices should be given as \"key1\" \"choice1\" \"key2\"
  \"choice2\" ... The choice control returned is a function of three
  arguments [frame scene buttons] and can be used with
  with-layout-modifiers, for example."
  [& choices]
  (assert (second choices))
  (assert (even? (count choices)))
  (let [choices (apply hash-map choices)]
    (fn [^JFrame frame, scene buttons]
      (let [^JPanel
            base-pane   (make-panel buttons),
            _           (.setLayout base-pane (BoxLayout. base-pane BoxLayout/Y_AXIS)),
            ^JComboBox
            combo-box   (make-combo-box base-pane (keys choices)),
            _           (make-padding base-pane),
            ^JPanel
            choice-pane (make-panel base-pane)
            _           (.setLayout choice-pane (BoxLayout. choice-pane BoxLayout/Y_AXIS))]
        (.setMaximumSize base-pane nil)
        (.setMaximumSize choice-pane nil)
        (listen combo-box :action
                (fn [evt]
                  (let [selected (.getSelectedItem ^JComboBox (.getSource ^ActionEvent evt)),
                        control  (get choices selected)]
                    (.removeAll choice-pane)
                    (control frame scene choice-pane)
                    (.validate frame))))
        (.setSelectedIndex combo-box 0)))))

(defmacro with-layout-modifiers
  "Installs given methods to scene with buttons."
  [frame scene buttons & methods]
  `(do
     (make-padding ~buttons)
     ~@(map (fn [method#]
              `(~method# ~frame ~scene ~buttons))
            (interpose (fn [_ _ buttons]
                         (make-padding buttons)
                         (make-separator buttons)
                         (make-padding buttons))
                       methods))))

;;;

nil
