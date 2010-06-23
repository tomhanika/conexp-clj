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
	                AbstractButton SwingConstants JFileChooser JOptionPane]
	   [javax.swing.filechooser FileNameExtensionFilter]
	   [javax.imageio ImageIO]
	   [java.awt GridLayout BorderLayout Dimension Image Font Color
	             Graphics Graphics2D BasicStroke FlowLayout]
	   [java.awt.event KeyEvent ActionListener MouseAdapter MouseEvent]
	   [java.io File])
  (:use [conexp.base :only (defvar first-non-nil with-swing-error-msg illegal-argument)])
  (:use [clojure.contrib.seq :only (indexed)]
	clojure.contrib.swing-utils)
  (:require [clojure.contrib.string :as string]))


;;; Helper functions

(defn- add-handler
  "Adds an ActionListener to thing that calls function with frame when
  activated (i.e. when actionPerformed is called)."
  [thing frame function]
  (.addActionListener thing
    (proxy [ActionListener] []
      (actionPerformed [evt]
	(with-swing-error-msg frame "Error"
	  (function frame))))))

(defn implements-interface?
  "Returns true iff given class implements given interface."
  [class interface]
  (some #(= interface %) (.getInterfaces class)))

(defn get-component
  "Returns the first component in component satisfing predicate."
  [component predicate]
  (if (predicate component)
    component
    (first-non-nil (map #(get-component % predicate) (.getComponents component)))))

(defn show-in-frame
  "Creates new frame with thing embedded and shows it."
  [thing]
  (let [frame (JFrame.)]
    (.add frame thing)
    (.setVisible frame true)
    (.setSize frame (Dimension. 500 200))
    (.setDefaultCloseOperation frame JFrame/DISPOSE_ON_CLOSE)
    frame))


;;; Swing handmade concurrency

(defn invoke-later
  "Calls fn with SwingUtilities/invokeLater."
  [fn]
  (SwingUtilities/invokeLater fn))

(defn invoke-later-or-now
  "Calls fn with SwingUtilities/invokeLater."
  [fn]
  (if (SwingUtilities/isEventDispatchThread)
    (fn)
    (SwingUtilities/invokeLater fn)))

(defn invoke-and-wait
  "Calls fn with SwingUtilities/invokeAndWait if necessary."
  [fn]
  (if (SwingUtilities/isEventDispatchThread)
    (fn)
    (SwingUtilities/invokeAndWait fn)))

(defmacro with-swing-threads
  "Executes body with invoke-later to make it thread-safe to Swing."
  [& body]
  `(invoke-later #(do ~@body)))

(defmacro with-swing-threads*
  "Executes body if in Swing thread or with invoke-later to make it
  thread-safe to Swing otherwise."
  [& body]
  `(invoke-later-or-now #(do ~@body)))

(defmacro do-swing-threads
  "Executes body with invoke-and-wait to make it thread-safe to Swing."
  [& body]
  `(invoke-and-wait #(do ~@body)))

(defmacro do-swing-return
  "Executes body with invoke-and-wait to make it thread-safe to Swing,
   returning the value of the last statement using a promise!"
  [& body]
  `(let [returnvalue# (promise)]
     (invoke-and-wait #(deliver returnvalue# (do ~@body)))
     @returnvalue#))

(defmacro defn-swing
  "Defines a function that is surrounded by do-swing-return"
  [name doc params & body]
  (if (vector? doc)
    `(defn ~name ~doc (do-swing-return ~params ~@body))
    `(defn ~name ~doc ~params
       (do-swing-return ~@body))))

(defmacro defmethod-swing
  "Defines a multi function method that is surrounded by do-swing-return"
  [name dispatch-val params & body]
  `(defmethod ~name ~dispatch-val ~params (do-swing-return ~@body)))

(defmacro defn-swing-threads*
  "Defines a function that is surrounded by with-swing-threads*"
  [name doc params & body]
  (if (vector? doc)
    `(defn ~name ~doc (with-swing-threads* ~params ~@body))
    `(defn ~name ~doc ~params
       (with-swing-threads* ~@body))))

(defmacro defmethod-swing-threads*
  "Defines a multi function method that is surrounded by do-swing-return"
  [name dispatch-val params & body]
  `(defmethod ~name ~dispatch-val ~params (with-swing-threads* ~@body)))


;;; type checking

(defn keyword-class
  "Takes an object x and returns a corresponding keyword describing
  its class name."
  [x]
  (let [rv          string/reverse,
        classname   (str (type x)),
        keywordname (rv (string/replace-first-re
                         #"\." "/" 
                         (rv (string/replace-first-re #"class " "" classname))))]
    (keyword keywordname)))

(defn keyword-isa?
  "Returns true iff the keyword-class of obj isa? parent."
  [obj parent]
  (isa? (keyword-class obj) parent))

;;;

(defn get-resource
  "Returns the resource res if found, nil otherwise."
  [res]
  (let [cl (.getContextClassLoader (Thread/currentThread))]
    (.getResource cl res)))

(defn confirm
  "Opens a message dialog asking for confirmation."
  [frame message]
  (JOptionPane/showConfirmDialog frame message))


;;; Menus

(defn- get-menubar
  "Returns menubar of given frame."
  [frame]
  (get-component frame #(= (class %) JMenuBar)))

(declare hash-map->menu)

(defn- hash-map->menu-item
  "Converts a hash to a JMenuItem for the given frame."
  [frame hash]
  (cond
   (empty? hash) (JSeparator.),
   (contains? hash :content) (hash-map->menu frame hash),
   :else
   (let [menu-item (JMenuItem. (:name hash))]
     (if (contains? hash :handler)
       (add-handler menu-item frame (:handler hash))
       (.setEnabled menu-item false))
     ;; also enable hotkeys and images
     menu-item)))

(defn- hash-map->menu
  "Converts a hash representing a menu into an actual JMenu for a given frame."
  [frame hash-menu]
  (if (instance? java.awt.Component hash-menu)
    hash-menu
    (let [menu (JMenu. (:name hash-menu))]
      (doseq [entry (:content hash-menu)]
	(.add menu (hash-map->menu-item frame entry)))
      menu)))

(defn add-menus
  "Adds the menus (specified as hash-maps) to the frame in front of
  the first Box$Filler found in the menu-bar of frame. Returns the
  menus added."
  [frame menus]
  (let [our-menus (map #(hash-map->menu frame %) menus)]
    (do-swing
     (let [menu-bar (get-menubar frame)
	   [menus-before menus-after] (split-with #(not (instance? javax.swing.Box$Filler %))
						  (seq (.getComponents menu-bar)))]
       (.removeAll menu-bar)
       (doseq [menu (concat menus-before our-menus menus-after)]
	 (.add menu-bar menu))
       (.validate frame)))
    our-menus))

(defn remove-menus
  "Removes given menus (as Java objects) from menu-bar of frame."
  [frame menus]
  (do-swing
   (let [menu-bar (get-menubar frame),
	 new-menus (remove (set menus) (seq (.getComponents menu-bar)))]
     (.removeAll menu-bar)
     (doseq [menu new-menus]
       (.add menu-bar menu))
     (.validate frame)
     new-menus)))

;; menu shortcut variables for convenience

(defvar --- {}
  "Separator for menu entries used in add-menus.")

(defvar === (Box/createHorizontalGlue)
  "Separator between menus used in add-menus.")


;;; Tool Bar

(defn- get-toolbar
  "Returns toolbar of given frame."
  [frame]
  (get-component frame #(= (class %) JToolBar)))

(defvar *default-icon-image* (get-resource "images/default.jpg")
  "Default icon image used when no other image is found.")
(defvar *icon-size* 17
  "Default icon size.")

(defn- make-icon
  "Converts hash representing an icon to an actual JButton."
  [frame icon-hash]
  (if (empty? icon-hash)
    (javax.swing.JToolBar$Separator.)
    (let [button (JButton.)]
      (doto button
	(.setName (:name icon-hash))
	(add-handler frame (:handler icon-hash))
	(.setToolTipText (:name icon-hash)))
      (let [icon (:icon icon-hash)
	    image (-> (ImageIO/read (if (and icon (.exists (File. icon)))
				      (File. icon)
				      *default-icon-image*))
		      (.getScaledInstance *icon-size*
					  *icon-size*
					  Image/SCALE_SMOOTH))]
	(.setIcon button (ImageIcon. image)))
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
  [frame icons]
  (let [our-icons (map #(make-icon frame %) icons)]
    (do-swing
     (let [toolbar (get-toolbar frame),
	   new-icons (collapse-separators (concat (.getComponents toolbar)
						  our-icons))]
       (.removeAll toolbar)
       (doseq [icon new-icons]
	 (.add toolbar icon))
       (.validate frame)))
    our-icons))

(defn remove-icons
  "Removes icons (as Java objects) from toolbar of frame. The
  resulting toolbar in frame will have no two equal icons side by
  side."
  [frame icons]
  (do-swing
   (let [toolbar (get-toolbar frame),
	 rem-icons (collapse-separators (remove (set icons) (seq (.getComponents toolbar))))]
     (.removeAll toolbar)
     (doseq [icon rem-icons]
       (.add toolbar icon))
     (.validate frame))))

;; toolbar shortcut variables for convenience

(defvar | {}
  "Separator for icons in toolbars used in add-icons.")


;;; Tabs

(defn- get-tabpane
  "Returns tabpane of the given frame."
  [frame]
  (get-component frame #(= (class %) javax.swing.JTabbedPane)))

(defn- make-tab-button
  "Creates and returns a button for a tab component in tabpane to
  close the tab containing component when it is pressed."
  [#^JTabbedPane tabpane, component]
  ;; This contains code copied from TabComponentDemo and
  ;; ButtonTabComponent from the Java Tutorial
  (let [tabbutton      (proxy [JButton] []
		         (paintComponent [#^Graphics g]
		           (proxy-super paintComponent g)
			   (let [#^Graphics2D g2 (.create g),
				 delta 6]
			     (when (.. this getModel isPressed)
			       (.translate g2 1 1))
			     (.setStroke g2 (BasicStroke. 2))
			     (.setColor g2 Color/BLACK)
			     (when (.. this getModel isRollover)
			       (.setColor g2 Color/MAGENTA))
			     (.drawLine g2 delta delta
					(- (. this getWidth) delta 1) (- (. this getHeight) delta 1))
			     (.drawLine g2 (- (. this getWidth) delta 1) delta
					delta (- (. this getHeight) delta 1))
			     (.dispose g2)))),
	mouse-listener (proxy [MouseAdapter] []
			 (mouseEntered [#^MouseEvent evt]
			   (let [component (.getComponent evt)]
			     (when (instance? AbstractButton component)
			       (.setBorderPainted #^AbstractButton component true))))
			 (mouseExited [#^MouseEvent evt]
			   (let [component (.getComponent evt)]
			     (when (instance? AbstractButton component)
			       (.setBorderPainted #^AbstractButton component false)))))]
    (doto tabbutton
      (add-action-listener (fn [evt]
			     (.remove tabpane (.indexOfComponent tabpane component))))
      (.addMouseListener mouse-listener)
      (.setPreferredSize (Dimension. 17 17))
      (.setToolTipText "Close this tab")
      (.setContentAreaFilled false)
      (.setBorderPainted false)
      (.setFocusable false))))

(defn- make-tab-head
  "Creates and returns a panel to be used as tab component."
  [#^JTabbedPane tabpane, component, title]
  (let [#^JPanel head (JPanel.),
	#^JLabel text (JLabel.),
	#^JButton btn (make-tab-button tabpane component)]
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
  ([frame pane title]
     (do-swing
      (let [#^JTabbedPane tabpane (get-tabpane frame)]
	(.add tabpane pane)
	(let [index (.indexOfComponent tabpane pane)]
	  (.setTabComponentAt tabpane index (make-tab-head tabpane pane title))
	  (.setSelectedIndex tabpane index)))
      (.validate frame)))
  ([frame pane]
     (add-tab frame pane "")))

(defn get-tabs
  "Returns hashmap from numbers to tab contents of given frame."
  [frame]
  (let [#^JTabbedPane tabpane (get-tabpane frame)]
    (into {} (indexed (seq (.getComponents tabpane))))))

(defn add-tab-with-name-icon-tooltip
  "Addes given panel to the tabpane of frame, giving name icon and tooltip"
  [frame pane name icon tooltip]
  (do-swing
   (.addTab (get-tabpane frame) name icon pane tooltip)
   (.validate frame)))

(defn remove-tab
  "Removes a panel from the windows JTabbedPane."
   [frame pane]
  (do-swing
   (.remove (get-tabpane frame) pane)
   (.validate frame)))

(defn current-tab
  "Returns the currently selected tab and nil if there is none."
  [frame]
  (let [#^JTabbedPane tabpane (get-tabpane frame),
	index (.getSelectedIndex tabpane)]
    (if (= -1 index)
      nil
      (.getComponentAt tabpane index))))

;; remove-tabs
;; find-tabs-by

;;; file chooser

(defn- make-file-chooser
  "Creates a JFileChooser with given filters for frame."
  [frame & filters]
  (let [#^JFileChooser fc (JFileChooser.)]
    (doseq [[name & endings] filters]
      (let [#^FileFilter filter (FileNameExtensionFilter. name (into-array endings))]
	(.addChoosableFileFilter fc filter)))
    fc))

(defn choose-open-file
  "Opens a file chooser for frame with optional extension filters and
  returns the file selected for opening. filters are given as a
  sequence of pairs [name & endings], where name names the type of files
  and endings are file suffixes."
  [frame & filters]
  (let [#^JFileChooser fc (apply make-file-chooser frame filters)]
    (when (= (.showOpenDialog fc frame) JFileChooser/APPROVE_OPTION)
      (.getSelectedFile fc))))

(defn choose-save-file
  "Opens a file chooser for frame with optional extension filters and
  returns the file selected for saving. filters are given as a
  sequence of pairs [name & endings], where name names the type of files
  and endings are file suffixes."
  [frame & filters]
  (let [#^JFileChooser fc (apply make-file-chooser frame filters)]
    (loop []
      (when (= (.showSaveDialog fc frame) JFileChooser/APPROVE_OPTION)
	(let [#^File file (.getSelectedFile fc)]
	  (if (not (.exists file))
	    file
	    (condp = (confirm frame "File exists, overwrite?")
	      JOptionPane/YES_OPTION file,
	      JOptionPane/NO_OPTION (recur),
	      JOptionPane/CANCEL_OPTION nil)))))))

;;;

nil
