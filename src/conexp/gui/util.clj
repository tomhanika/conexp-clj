(ns conexp.gui.util
  (:import [javax.swing JFrame JMenuBar JMenu JMenuItem Box JToolBar JPanel
	                JButton ImageIcon JSeparator JTabbedPane JSplitPane
	                JLabel JTextArea JScrollPane SwingUtilities]
	   [javax.imageio ImageIO]
	   [java.awt GridLayout BorderLayout Dimension Image Font Color]
	   [java.awt.event KeyEvent ActionListener]
	   [java.io File])
  (:use [conexp.base :only (defvar first-non-nil)]))


;;; Helper functions

(defn add-handler
  "Adds an ActionListener to thing that calls function with frame and
  thing when activated (i.e. when actionPerformed is called)."
  ; shall we also pass the given event?
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

(defn- make-menu
  "Converts a hash representing a menu into an actual JMenu for a given frame."
  [frame hash-menu]
  (cond
    (instance? java.awt.Component hash-menu)
    hash-menu
    :else
    (let [menu (JMenu. (:name hash-menu))]
      (doseq [entry (:content hash-menu)]
	(cond
	  (empty? entry)
	  (.addSeparator menu)
	  :else
	  (let [menu-item (JMenuItem. (:name entry))]
	    (add-handler menu-item frame (:handler entry))
	    (.add menu menu-item))))
      menu)))

(defn- add-to-menubar
  "Adds menubar consiting of menus to menu-bar."
  [frame menu-bar menus]
  (doseq [menu (map #(make-menu frame %) menus)]
    (.add menu-bar menu))
  menu-bar)

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
      (add-to-menubar frame menu-bar menu-entries-before-filler)
      (add-to-menubar frame menu-bar menus)
      (add-to-menubar frame menu-bar menu-entries-from-filler)
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
  "Addes given panel to the tabpane of frame."
  [frame pane]
  (with-swing-threads
    (.add (get-tabpane frame) pane)
    (.validate frame)))


nil
