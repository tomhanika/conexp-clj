;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; this file contains contributions by Immanuel Albrecht

(ns conexp.contrib.gui.util
  (:import [javax.swing JFrame JMenuBar JMenu JMenuItem Box JToolBar JPanel
                        JButton ImageIcon JSeparator JTabbedPane JSplitPane
                        JLabel JTextArea JScrollPane SwingUtilities BorderFactory
                        AbstractButton SwingConstants JFileChooser JOptionPane
                        ImageIcon JComponent]
           [javax.swing.filechooser FileNameExtensionFilter]
           [javax.imageio ImageIO]
           [java.awt GridLayout BorderLayout Dimension Image Font Color
                     Graphics Graphics2D BasicStroke FlowLayout]
           [java.awt.event KeyEvent ActionListener ActionEvent MouseAdapter MouseEvent]
           [java.awt.image BufferedImage]
           [javax.swing.event ChangeListener ChangeEvent]
           [java.io File])
  (:use [conexp.base :only (defvar, defmacro-, first-non-nil, illegal-argument)])
  (:use seesaw.core)
  (:require [clojure.string :as string]))


;;; Swing handmade concurrency

(defn invoke-later-or-now
  "Calls fn with SwingUtilities/invokeLater."
  [fn]
  (if (SwingUtilities/isEventDispatchThread)
    (fn)
    (SwingUtilities/invokeLater fn)))

(defmacro do-swing
  "Executes body within the Swing Dispatch Thread or immediately, if
  execution is already in this thread."
  [& body]
  `(invoke-later-or-now #(do ~@body)))

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

(defmacro with-action-on
  "Adds an action listener on thing to execute body, with the catched
  ActionEvent Object bound to the variable evt. body will be executed
  in a thread-safe manner."
  [thing & body]
  `(.addActionListener
    ~thing
    (proxy [ActionListener] []
      (actionPerformed [^java.awt.event.ActionEvent ~'evt] ;how to do right?
        (do-swing ~@body)))))

(defmacro with-change-on
  "Adds a change listener on thing to execute body, with the catched
  ChangeEvent Object bound to the variable evt. body will be executed
  in a thread-safe manner."
  [thing & body]
  `(.addChangeListener ~thing
                       (proxy [ChangeListener] []
                         (stateChanged [^ChangeEvent ~'evt]
                           (do-swing ~@body)))))

(defn get-root-cause
  "Returns original message of first exception causing the given one."
  [^Throwable exception]
  (if-let [cause (.getCause exception)]
    (get-root-cause cause)
    (.getMessage exception)))

(defmacro with-swing-error-msg
  "Runs given code and catches any thrown exception, which is then
  displayed in a message dialog."
  [frame title & body]
  `(try
     ~@body
     (catch Exception e#
       (javax.swing.JOptionPane/showMessageDialog
        ~frame
        (scrollable (text :text (apply str (get-root-cause e#) "\n"
                                       (interpose "\n" (.getStackTrace e#)))
                          :multi-line? true))
        ~title
        javax.swing.JOptionPane/ERROR_MESSAGE))))

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

(defn message-box
  "Pops up a swing message box."
  ([text title]
     (JOptionPane/showMessageDialog nil (str text) (str title) 0))
  ([text]
     (JOptionPane/showMessageDialog nil (str text) "Info" 0)))

(defn ^java.net.URL get-resource
  "Returns the URL of the given the resource res if found, nil otherwise."
  [res]
  (let [cl (.getContextClassLoader (Thread/currentThread))]
    (.getResource cl res)))

(defn confirm
  "Opens a message dialog asking for confirmation."
  [frame message]
  (JOptionPane/showConfirmDialog frame message))

(defn get-image-icon-or-string
  "Returns either the image-icon for the given resource or the given
   alternative string."
  [res alt]
  (let [img (get-resource (str "res/" res)),
        ^ImageIcon img (and img (ImageIcon. img))]
    (or img alt)))


;;; widgets

(defn class-to-keyword
  "Takes a class c and returns a corresponding keyword describing
  its class name."
  [c]
  (let [rv          string/reverse,
        classname   (str c),
        keywordname (rv (string/replace-first
                         (rv (string/replace-first classname #"class " ""))
                         #"\."
                         "/"))]
    (keyword keywordname)))

(defn keyword-class
  "Takes an object x and returns a corresponding keyword describing
  its class name."
  [x]
  (class-to-keyword (type x)))

(defn keyword-isa?
  "Returns true iff the keyword-class of obj isa? parent."
  [obj parent]
  (isa? (keyword-class obj) (class-to-keyword parent)))

(defmacro defwidget
  "Defines a widget. The resulting widget will have super-widgets,
  which must be a vector of valid widgets, as super-widgets and fields
  and data fields."
  [name super-widgets fields]
  `(do
     (defrecord ~name ~fields)
     ~@(for [sw super-widgets]
         `(derive (class-to-keyword ~name) (class-to-keyword ~sw)))))


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
   (let [menu-bar (select frame [:JMenuBar]),
         new-menus (remove (set menus) (seq (.getComponents menu-bar)))]
     (.removeAll menu-bar)
     (doseq [^JComponent menu new-menus]
       (.add menu-bar menu))
     (.validate frame)
     new-menus)))

;;; Tool Bar

(defvar ^String default-icon-image (get-resource "images/default.jpg")
  "Default icon image used when no other image is found.")
(defvar icon-size 17
  "Default icon size.")

(defn- ^JButton make-icon
  "Converts hash representing an icon to an actual JButton."
  [frame icon-hash]
  (if (empty? icon-hash)
    (javax.swing.JToolBar$Separator.)
    (let [button (JButton.)]
      (doto button
        (.setName (:name icon-hash))
        (listen :action
                (fn [evt]
                  (with-swing-error-msg frame "Error"
                    ((:handler icon-hash) frame))))
        (.setToolTipText (:name icon-hash)))
      (let [icon (:icon icon-hash)
            image (-> ^BufferedImage
                      (ImageIO/read ^File (if (and icon
                                                   (.exists (File. ^String icon)))
                                            (File. ^String icon)
                                            (File. default-icon-image)))
                      (.getScaledInstance icon-size icon-size
                                          Image/SCALE_SMOOTH))]
        (.setIcon button (ImageIcon. ^Image image)))
      button)))

(defn- collapse-separators
  "Given a sequence of Java objects returns a sequence of the same
  objects where adjacent Separators are collapsed into one."
  [objects]
  (let [partitioned-objects (partition-by class objects)]
    (apply concat (map #(if (instance? javax.swing.JSeparator (first %))
                          (list (first %))
                          %)
                       partitioned-objects))))

(defn add-icons
  "Adds icons (sepcified as hash-maps) to toolbar of frame, returning
  added icons."
  [^JFrame frame, icons]
  (let [our-icons (map #(make-icon frame %) icons)]
    (do-swing
     (let [toolbar (select frame [:JToolBar]),
           new-icons (collapse-separators (concat (.getComponents toolbar)
                                                  our-icons))]
       (.removeAll toolbar)
       (doseq [^JComponent icon new-icons]
         (.add toolbar icon))
       (.validate frame)))
    our-icons))

(defn remove-icons
  "Removes icons (as Java objects) from toolbar of frame. The
  resulting toolbar in frame will have no two equal icons side by
  side."
  [^JFrame frame, icons]
  (do-swing
   (let [^JToolBar toolbar (select frame [:JToolBar]),
         rem-icons (collapse-separators (remove (set icons) (seq (.getComponents toolbar))))]
     (.removeAll toolbar)
     (doseq [^JComponent icon rem-icons]
       (.add toolbar icon))
     (.validate frame))))

;; toolbar shortcut variables for convenience

(defvar | {}
  "Separator for icons in toolbars used in add-icons.")


;;; Tabs

(defn- get-tabpane
  "Returns tabpane of the given frame."
  [frame]
  (get-component frame #(instance? javax.swing.JTabbedPane %)))

(defn- make-tab-button
  "Creates and returns a button for a tab component in tabpane to
  close the tab containing component when it is pressed."
  [^JTabbedPane tabpane, component]
  ;; This contains code copied from TabComponentDemo and
  ;; ButtonTabComponent from the Java Tutorial
  (let [tabbutton      (proxy [JButton] []
                         (paintComponent [^Graphics g]
                           (let [^JButton this this]
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
                               (.dispose g2))))),
        mouse-listener (proxy [MouseAdapter] []
                         (mouseEntered [^MouseEvent evt]
                           (let [component (.getComponent evt)]
                             (when (instance? AbstractButton component)
                               (.setBorderPainted ^AbstractButton component true))))
                         (mouseExited [^MouseEvent evt]
                           (let [component (.getComponent evt)]
                             (when (instance? AbstractButton component)
                               (.setBorderPainted ^AbstractButton component false)))))]
    (doto tabbutton
      (with-action-on (.remove tabpane (.indexOfComponent tabpane component)))
      (.addMouseListener mouse-listener)
      (.setPreferredSize (Dimension. 17 17))
      (.setToolTipText "Close this tab")
      (.setContentAreaFilled false)
      (.setBorderPainted false)
      (.setFocusable false))))

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
      (let [^JTabbedPane tabpane (get-tabpane frame)]
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
  "Returns hashmap from numbers to tab contents of given frame."
  [frame]
  (let [^JTabbedPane tabpane (get-tabpane frame)]
    (vec (rest (seq (.getComponents tabpane))))))

(defn add-tab-with-name-icon-tooltip
  "Addes given panel to the tabpane of frame, giving name icon and tooltip"
  [^JFrame frame, ^JPanel pane, name icon tooltip]
  (do-swing
   (.addTab ^JTabbedPane (get-tabpane frame) name icon pane tooltip)
   (.validate frame)))

(defn remove-tab
  "Removes a panel from the windows JTabbedPane."
   [^JFrame frame, ^JPanel pane]
   (do-swing
    (.remove ^JTabbedPane (get-tabpane frame) pane)
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
  (let [^JTabbedPane tabpane (get-tabpane frame),
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
            (condp = (confirm frame "File exists, overwrite?")
              JOptionPane/YES_OPTION file,
              JOptionPane/NO_OPTION (recur),
              JOptionPane/CANCEL_OPTION nil)))))))

;;;

nil
