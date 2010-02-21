;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.util
  (:import [javax.swing JFrame JMenuBar JMenu JMenuItem Box JToolBar JPanel
	                JButton ImageIcon JSeparator JTabbedPane JSplitPane
	                JLabel JTextArea JScrollPane SwingUtilities]
	   [javax.imageio ImageIO]
	   [java.awt GridLayout BorderLayout Dimension Image Font Color]
	   [java.awt.event KeyEvent ActionListener]
	   [java.io File])
  (:use [conexp.base :only (defvar first-non-nil)]
	[clojure.contrib.seq-utils :only (indexed)]))


;;; Helper functions

(defn add-handler
  "Adds an ActionListener to thing that calls function with frame and
  thing when activated (i.e. when actionPerformed is called)."
  ;; shall we also pass the given event?
  [thing frame function]
  (.addActionListener thing
    (proxy [ActionListener] []
      (actionPerformed [evt]
	(function frame thing)))))

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
    (.setDefaultCloseOperation frame JFrame/DISPOSE_ON_CLOSE)
    frame))

(defn invoke-later
  "Calls fn with SwingUtilities/invokeLater."
  [fn]
  (SwingUtilities/invokeLater fn))

(defn invoke-and-wait
  "Calls fn with SwingUtilities/invokeAndWait."
  [fn]
  (SwingUtilities/invokeAndWait fn))

(defmacro with-swing-threads
  "Executes body with invoke-later to make it thread-safe to Swing."
  [& body]
  `(invoke-later #(do ~@body)))

(defn get-resource
  "Returns the resource res if found, nil otherwise."
  [res]
  (let [cl (.getContextClassLoader (Thread/currentThread))]
    (.getResource cl res)))


;;; Menus

(defn- get-menubar
  "Returns menubar of given frame."
  [frame]
  (get-component frame #(= (class %) JMenuBar)))

(declare hash-to-menu)

(defn- hash-to-menu-item
  "Converts a hash to a JMenuItem for the given frame."
  [frame hash]
  (cond
   (empty? hash) (JSeparator.),
   (contains? hash :content) (hash-to-menu frame hash),
   :else
   (let [menu-item (JMenuItem. (:name hash))]
     (if (contains? hash :handler)
       (add-handler menu-item frame (:handler hash))
       (.setEnabled menu-item false))
     ;; also enable hotkeys and images
     menu-item)))

(defn- hash-to-menu
  "Converts a hash representing a menu into an actual JMenu for a given frame."
  [frame hash-menu]
  ;; this function is not thread safe!
  (cond
    (instance? java.awt.Component hash-menu)
    hash-menu,
    :else
    (let [menu (JMenu. (:name hash-menu))]
      (doseq [entry (:content hash-menu)]
	(.add menu (hash-to-menu-item frame entry)))
      menu)))

(defn- add-menus-to-menubar
  "Adds menubar consisting of menus to menu-bar."
  ;; this function is not thread safe!
  [frame menus]
  (let [menu-bar (get-menubar frame)]
    (doseq [menu menus]
      (.add menu-bar (hash-to-menu frame menu)))
    menu-bar))

(defn add-menus
  "Adds the additional menus to the frame in front of the first Box.Filler
  found in the menu-bar of frame."
  [frame menus]
  (with-swing-threads
    (let [menu-bar (get-menubar frame)
	  menu-bar-as-seq (.getComponents menu-bar)
	  menu-entries-before-filler (take-while #(not (instance? javax.swing.Box$Filler %))
						 menu-bar-as-seq)
	  menu-entries-from-filler   (drop-while #(not (instance? javax.swing.Box$Filler %))
						 menu-bar-as-seq)]
      (.removeAll menu-bar)
      (add-menus-to-menubar frame menu-entries-before-filler)
      (add-menus-to-menubar frame menus)
      (add-menus-to-menubar frame menu-entries-from-filler)
      (.validate frame))))

(defvar --- {}
  "Separator for menu entries used in add-menus.")
(defvar === (Box/createHorizontalGlue)
  "Separator between menus used in add-menus.")


;;; Tool Bar

(defn- get-toolbar
  "Returns toolbar of given frame."
  [frame]
  (get-component frame #(= (class %) JToolBar)))

(defvar *default-icon* (get-resource "images/default.jpg")
  "Default icon image used when no other image is found.")
(defvar *icon-size* 17
  "Default icon size.")

(defn- make-icon
  "Converts hash representing an icon to an actual JButton."
  [frame icon-hash]
  (let [button (JButton.)]
    (doto button
      (.setName (:name icon-hash))
      (add-handler frame (:handler icon-hash))
      (.setToolTipText (:name icon-hash)))
    (let [icon (:icon icon-hash)
	  image (-> (ImageIO/read (if (and icon (.exists (File. icon)))
				    (File. icon)
				    *default-icon*))
		    (.getScaledInstance *icon-size*
					*icon-size*
					Image/SCALE_SMOOTH))]
	(.setIcon button (ImageIcon. image)))
    button))

(defn- add-to-toolbar
  "Adds given icons to toolbar of given frame."
  [frame toolbar icons]
  (doseq [icon icons]
    (cond
      (empty? icon)
      (.addSeparator toolbar)
      :else
      (.add toolbar (make-icon frame icon))))
  toolbar)

(defn add-icons
  "Adds icons to toolbar of frame."
  [frame icons]
  (with-swing-threads
    (add-to-toolbar frame (get-toolbar frame) icons)
    (.validate frame)))

(defvar | {}
  "Separator for icons in toolbars used in add-icons.")


;;; Tabs

(defn- get-tabpane
  "Returns tabpane of the given frame."
  [frame]
  (get-component frame #(= (class %) javax.swing.JTabbedPane)))

(defn add-tab
  "Addes given panel to the tabpane of frame with given title, if given."
  ([frame pane title]
     (with-swing-threads
       (let [#^JTabbedPane tabpane (get-tabpane frame)]
	 (.addTab tabpane title pane))
       (.validate frame)))
  ([frame pane]
     (add-tab frame pane "")))

(defn get-tabs
  "Returns hashmap from numbers to tab contents of given frame."
  [frame]
  (let [#^JTabbedPane tabpane (get-tabpane frame)]
    (into {} (indexed (seq (.getComponents tabpane))))))

;;;

nil
