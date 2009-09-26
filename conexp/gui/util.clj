(ns conexp.gui.util
  (:import [javax.swing JFrame SwingUtilities]
	   [java.awt.event ActionListener])
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

(defn show-in-frame [thing]
  (let [frame (JFrame.)]
    (.add frame thing)
    (.setVisible frame true)
    (.setDefaultCloseOperation frame JFrame/DISPOSE_ON_CLOSE)
    frame))

(defn invoke-later [fn]
  (SwingUtilities/invokeLater fn))

(defn invoke-and-wait [fn]
  (SwingUtilities/invokeAndWait fn))

(defmacro with-swing-threads [& body]
  `(invoke-later #(do ~@body)))