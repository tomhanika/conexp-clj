(ns conexp.gui.util
  (:import [java.awt.event ActionListener])
  (:use conexp.util))

(defn add-handler [thing frame function]
  (.addActionListener thing
    (proxy [ActionListener] []
      (actionPerformed [evt]
	(function frame thing)))))

(defn get-component [component predicate]
  (if (predicate component)
    component
    (first-non-nil (map #(get-component % predicate) (.getComponents component)))))
