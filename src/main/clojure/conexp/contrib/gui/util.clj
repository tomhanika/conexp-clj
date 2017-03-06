;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; this file contains contributions by Immanuel Albrecht

(ns conexp.contrib.gui.util
  (:import [javax.swing JFrame JPanel JButton ImageIcon JTabbedPane
                        JLabel BorderFactory AbstractButton JFileChooser
                        JOptionPane JComponent JMenuBar]
           [javax.swing.filechooser FileNameExtensionFilter]
           [java.awt Color Dimension Graphics Graphics2D BasicStroke FlowLayout]
           [java.awt.event MouseEvent]
           [java.io File])
  (:use [conexp.base :only (defmacro-, first-non-nil,
                             illegal-argument, unsupported-operation)])
  (:require [clojure.java.io :as io])
  (:use seesaw.core
        [seesaw.util :only (root-cause)]))


;;; Swing handmade concurrency

(defmacro do-swing
  "Executes body within the Swing Dispatch Thread or immediately, if
  execution is already in this thread."
  [& body]
  `(invoke-soon ~@body))

(defmacro do-swing-return
  "Executes body in a thread-safe manner for Swing and returns its value."
  [& body]
  `(let [returnvalue# (promise)]
     (do-swing (deliver returnvalue# (do ~@body)))
     @returnvalue#))

(defmacro defn-swing
  "Defines a function that is surrounded by do-swing-return."
  [name doc params & body]
  (if (vector? doc)
    `(defn ~name ~doc (do-swing-return ~params ~@body))
    `(defn ~name ~doc ~params (do-swing-return ~@body))))

;;; Helper functions

(defmacro with-swing-error-msg
  "Runs given code and catches any thrown exception, which is then
  displayed in a message dialog."
  [frame title & body]
  `(try
     ~@body
     (catch Exception e#
       (let [cause# (root-cause e#)]
         (show!
          (dialog :parent ~frame
                  :size [600 :by 300]
                  :resizable? false
                  :content (vertical-panel :items [[:fill-v 10]
                                                   (.getMessage ^Throwable cause#)
                                                   [:fill-v 10]
                                                   (scrollable (text :text (apply str cause# "\n"
                                                                                  (interpose "\n" (.getStackTrace e#)))
                                                                     :multi-line? true
                                                                     :editable? false
                                                                     :caret-position 0))])
                  :type :error
                  :title ~title))))))

(defn get-component
  "Returns the first component in component satisfing predicate."
  [^java.awt.Container component, predicate]
  (if (predicate component)
    component
    (first-non-nil (map #(get-component % predicate) (.getComponents component)))))

(defn show-in-frame
  "Creates new frame with thing embedded and shows it."
  [thing]
  (-> (frame :content thing
             :size [500 :by 200]
             :on-close :dispose)
      pack!
      show!))

(defn get-image-icon-or-string
  "Returns either the image-icon for the given resource or the given
   alternative string."
  [res alt]
  (let [img (io/resource res),
        ^ImageIcon
        img (and img (ImageIcon. img))]
    (or img alt)))

(defn open-link-in-external-browser
  "Given a string denoting an URL, tries to open that URL in the default browser.  In case
  this does not work, displays an error message attached to frame."
  [frame string]
  (with-swing-error-msg frame "Error"
    (let [desktop (java.awt.Desktop/getDesktop)]
      (when-not (.isSupported desktop java.awt.Desktop$Action/BROWSE)
        (unsupported-operation "Not supported"))
      (.browse (java.awt.Desktop/getDesktop)
               (java.net.URI. string)))))

;;; Menus

(defn add-menus
  "Takes a sequences of menus and adds them to the given frame"
  [frame menus]
  (let [framebar (first (select frame [:JMenuBar]))]
    (config! frame :menubar
             (menubar :items (vec (concat (when framebar (config framebar :items))
                                          menus))))))

(defn remove-menus
  "Removes given menus (as Java objects) from menu-bar of frame."
  [^JFrame frame, menus]
  (do-swing
   (let [^JMenuBar menu-bar (first (select frame [:JMenuBar])),
         new-menus (remove (set menus) (seq (.getComponents menu-bar)))]
     (.removeAll menu-bar)
     (doseq [^JComponent menu new-menus]
       (.add menu-bar menu))
     (.validate frame)
     new-menus)))

;;; Tabs

(defn- ^JTabbedPane get-tabpane
  "Returns tabpane of the given frame."
  [frame]
  (first (select frame [:<javax.swing.JTabbedPane>])))

(defn- make-tab-button
  "Creates and returns a button for a tab component in tabpane to
  close the tab containing component when it is pressed."
  [^JTabbedPane tabpane, component]
  ;; This contains code copied from TabComponentDemo and
  ;; ButtonTabComponent from the Java Tutorial
  (let [^JButton
        tabbutton (button :paint (fn [^JButton this ^Graphics g]
                                   (proxy-super paintComponent g)
                                   (let [^Graphics2D g2 (.create g),
                                         delta 6]
                                     (when (.. this getModel isPressed)
                                       (.translate g2 1 1))
                                     (.setStroke g2 (BasicStroke. 2))
                                     (.setColor g2 Color/BLACK)
                                     (when (.. this getModel isRollover)
                                       (.setColor g2 Color/MAGENTA))
                                     (.drawLine g2 delta delta
                                                (- (. this getWidth) delta 1)
                                                (- (.  this getHeight) delta 1))
                                     (.drawLine g2 (- (. this getWidth) delta 1) delta
                                                delta (- (. this getHeight) delta 1))
                                     (.dispose g2)))
                          :preferred-size [17 :by 17]
                          :tip "Close this tab"
                          :focusable? false)]
    (doto tabbutton
      (listen :action
              (fn [_]
                (.remove tabpane (.indexOfComponent tabpane component))))
      (listen :mouse-entered
              (fn [^MouseEvent evt]
                (let [component (.getComponent evt)]
                  (when (instance? AbstractButton component)
                    (.setBorderPainted ^AbstractButton component true)))))
      (listen :mouse-exited
              (fn [^MouseEvent evt]
                (let [component (.getComponent evt)]
                  (when (instance? AbstractButton component)
                    (.setBorderPainted ^AbstractButton component false)))))
      (.setBorderPainted false)
      (.setContentAreaFilled false))))

(defn- make-tab-head
  "Creates and returns a panel to be used as tab component."
  [^JTabbedPane tabpane, component, title]
  (let [^JPanel head (JPanel.),
        ^JLabel text (JLabel.),
        ^JButton btn (make-tab-button tabpane component)]
    (doto text
      (.setText title)
      (.setBorder (BorderFactory/createEmptyBorder 0 0 0 5)))
    (doto head
      (.setLayout (FlowLayout. FlowLayout/LEFT 0 0))
      (.add text)
      (.add btn)
      (.setOpaque false)
      (.setBorder (BorderFactory/createEmptyBorder 2 0 0 0)))))

(defn add-tab
  "Addes given panel to the tabpane of frame with given title, if given."
  ([^JFrame frame, ^JPanel pane, ^String title]
     (do-swing
      (let [tabpane (get-tabpane frame)]
        (.add tabpane pane)
        (let [index (.indexOfComponent tabpane pane),
              title (if-not (.isEmpty title)
                      (str index " (" title ")")
                      (str index))]
          (.setTabComponentAt tabpane index (make-tab-head tabpane pane title))
          (.setTitleAt tabpane index title)
          (.setSelectedIndex tabpane index)))
      (.validate frame)))
  ([frame pane]
     (add-tab frame pane "")))

(defn get-tabs
  "Returns a vector of all tab of the given frame."
  [frame]
  (let [tabpane (get-tabpane frame)]
    (vec (rest (seq (.getComponents tabpane))))))

(defn add-tab-with-name-icon-tooltip
  "Addes given panel to the tabpane of frame, giving name icon and tooltip"
  [^JFrame frame, ^JPanel pane, name icon tooltip]
  (do-swing
   (.addTab (get-tabpane frame) name icon pane tooltip)
   (.validate frame)))

(defn remove-tab
  "Removes a panel from the windows JTabbedPane."
   [^JFrame frame, ^JPanel pane]
   (do-swing
    (.remove (get-tabpane frame) pane)
    (.validate frame)))

(defn current-tab
  "Returns the currently selected tab and nil if there is none."
  [frame]
  (let [^JTabbedPane tabpane (get-tabpane frame),
        index (.getSelectedIndex tabpane)]
    (if (= -1 index)
      nil
      (.getComponentAt tabpane index))))

(defn current-tab-title
  "Returns the currently selected tab's title and nil if there is none."
  [frame]
  (let [tabpane (get-tabpane frame),
        index (.getSelectedIndex tabpane)]
    (if (= -1 index)
      nil
      (.getTitleAt tabpane index))))


;;; file chooser

(defn- make-file-chooser
  "Creates a JFileChooser with given filters for frame."
  [frame & filters]
  (let [^JFileChooser fc (JFileChooser.)]
    (doseq [[name & endings] filters]
      (let [^FileFilter filter (FileNameExtensionFilter. name (into-array endings))]
        (.addChoosableFileFilter fc filter)))
    fc))

(defn choose-open-file
  "Opens a file chooser for frame with optional extension filters and
  returns the file selected for opening. filters are given as a
  sequence of pairs [name & endings], where name names the type of files
  and endings are file suffixes."
  [frame & filters]
  (let [^JFileChooser fc (apply make-file-chooser frame filters)]
    (when (= (.showOpenDialog fc frame) JFileChooser/APPROVE_OPTION)
      (.getSelectedFile fc))))

(defn choose-save-file
  "Opens a file chooser for frame with optional extension filters and
  returns the file selected for saving. filters are given as a
  sequence of pairs [name & endings], where name names the type of files
  and endings are file suffixes."
  [frame & filters]
  (let [^JFileChooser fc (apply make-file-chooser frame filters)]
    (loop []
      (when (= (.showSaveDialog fc frame) JFileChooser/APPROVE_OPTION)
        (let [^File file (.getSelectedFile fc)]
          (if (not (.exists file))
            file
            (condp = (JOptionPane/showConfirmDialog frame "File exists, overwrite?")
              JOptionPane/YES_OPTION file,
              JOptionPane/NO_OPTION (recur),
              JOptionPane/CANCEL_OPTION nil)))))))

;;;

nil
