(ns conexp.gui.draw.control.freese
  (:require [conexp.gui.draw.control.util :refer :all]
            [conexp.gui.draw.scene-layouts :refer :all]
            [conexp.gui.draw.scenes :refer :all]
            [conexp.gui.util :refer :all]
            [conexp.layouts.base :refer :all]
            [conexp.layouts.freese :refer :all]
            [seesaw.core :refer [listen]])
  (:import [javax.swing JButton JSpinner]))

;;; Freese layout

(defn freese
  "Installs Freese Layout control."
  [frame scn buttons]
  (let [^JButton btn    (make-button buttons "Freese"),
        ^JSpinner spn   (make-spinner buttons 0 (* 2 Math/PI) 0 0.01),
        layout          (interactive-freese-layout (lattice (get-layout-from-scene scn))),
        get-value       #(.getValue spn),
        ^JButton rotate (make-button buttons "Rotate"),
        rotate-thread   (atom nil),
        start-rotate    #(when-not @rotate-thread
                           (update-layout-of-scene scn (layout (get-value)))
                           (fit-scene-to-layout scn)
                           (reset! rotate-thread
                                   (Thread. (fn []
                                              (doseq [angle (drop-while (let [y (get-value)]
                                                                          (fn [x] (<= x y)))
                                                                        (cycle (range 0 (* 2 Math/PI) 0.05))),
                                                      :while @rotate-thread]
                                                (Thread/sleep 50)
                                                (do-swing
                                                 (when (.isVisible (scene-canvas scn))
                                                   (.setValue spn angle)))))))
                           (.start ^Thread @rotate-thread)),
        stop-rotate     #(when @rotate-thread
                           (.stop ^Thread @rotate-thread)
                           (reset! rotate-thread nil))]
    (listen btn :action
            (fn [_]
              (update-layout-of-scene scn (layout (get-value)))
              (fit-scene-to-layout scn)))
    (listen spn :change
            (fn [_]
              (update-layout-of-scene scn (layout (get-value)))))
    (listen rotate :action
            (fn [_]
              (if @rotate-thread
                (stop-rotate)
                (start-rotate))))
    (listen (scene-canvas scn) :component-hidden
            (fn [_]
              (stop-rotate)))
    (listen frame #{:window-iconified :window-closed}
            (fn [_]
              (stop-rotate)))))

;;;

nil
