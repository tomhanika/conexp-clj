;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.draw.control.force-layout
  (:use conexp.layouts.force
        conexp.contrib.draw.scene-layouts
        conexp.contrib.draw.control.util
        conexp.contrib.gui.util)
  (:use seesaw.core)
  (:import [javax.swing JTextField JButton]))

;;;

(defn- improve-with-force
  "Improves layout on scene with force layout."
  [scn iterations r a g]
  (binding [*repulsive-amount* r,
            *attractive-amount* a,
            *gravitative-amount* g]
    (let [layout (if (<= iterations 0)
                   (force-layout (get-layout-from-scene scn))
                   (force-layout (get-layout-from-scene scn) iterations))]
      (update-layout-of-scene scn layout)
      (fit-scene-to-layout scn layout))))

(defn improve-layout-by-force
  "Improves layout on screen by force layout."
  [frame scn buttons]
  (let [^JTextField rep-field    (make-labeled-text-field buttons "rep"  (str *repulsive-amount*)),
        ^JTextField attr-field   (make-labeled-text-field buttons "attr" (str *attractive-amount*)),
        ^JTextField grav-field   (make-labeled-text-field buttons "grav" (str *gravitative-amount*)),
        ^JTextField iter-field   (make-labeled-text-field buttons "iter" (str "300")),
        _                        (make-padding buttons),
        ^JButton button          (make-button buttons "Force"),

        get-force-parameters     (fn []
                                   (let [r (Double/parseDouble (.getText rep-field)),
                                         a (Double/parseDouble (.getText attr-field)),
                                         g (Double/parseDouble (.getText grav-field)),
                                         i (Integer/parseInt (.getText iter-field))]
                                     [r a g i]))]
    (listen button :action
            (fn [_]
              (with-swing-error-msg frame "An Error occured."
                (let [[r a g i] (get-force-parameters)]
                  (improve-with-force scn i r a g)))))))

;;;

nil
