(ns conexp.gui.draw.control.force-layout
  (:require [conexp.gui.draw.control.util :refer :all]
            [conexp.gui.draw.scene-layouts :refer :all]
            [conexp.gui.util :refer :all]
            [conexp.layouts.force :refer :all]
            [seesaw.core :refer [listen]])
  (:import [javax.swing JButton JTextField]))

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
