(ns conexp.gui.base
  (:import [javax.swing JFrame JMenuBar JMenu JMenuItem Box JToolBar JPanel
	                JButton ImageIcon JSeparator JTabbedPane JSplitPane
	                JLabel]
	   [javax.imageio ImageIO]
	   [java.awt GridLayout BorderLayout Dimension Image]
	   [java.awt.event KeyEvent ActionListener]
	   [java.io File])
  (:use clojure.contrib.repl-utils
	clojure.contrib.core
	conexp.util
	conexp.gui.util))
     
;;; Menus

(defn get-menubar [frame]
  (get-component frame #(= (class %) JMenuBar)))

(defn make-menu
  "Converts a hash repsenting a menu into an actual JMenu for a given frame.
To be extended ... TODO"
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

(defn add-to-menubar
  "Adds menubar consiting of menus to menu-bar."
  [frame menu-bar menus]
  (doseq [menu (map #(make-menu frame %) menus)]
    (.add menu-bar menu))
  menu-bar)

(defn add-menus
  "Adds the additional menus to the frame in front of the first Box.Filler
found in the menu-bar of frame."
  [frame menus]
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
    (.validate frame)))

; shortcuts for menu creating
(def --- {})
(def === (Box/createHorizontalGlue))

(def *main-menu* {:name "Main" :content [{:name "Add Menu" ; just for fun
					  :handler (fn [frame _]
						     (add-menus frame
								[{:name "?" :content []}]))}
					 ---
					 {:name "Quit"
					  :handler (fn [frame _] (.dispose frame))}]})

(def *help-menu* {:name "Help" :content []})

(def *standard-menus* [*main-menu* === *help-menu*])


;;; Tool Bar

(defn get-toolbar [frame]
  (get-component frame #(= (class %) JToolBar)))

(def *default-icon* (File. "images/default.jpg"))
(def *icon-size* 17)

(defn make-icon [frame icon-hash]
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

(defn add-to-toolbar [frame toolbar icons]
  (doseq [icon icons]
    (cond
      (empty? icon)
      (.addSeparator toolbar)
      :else
      (.add toolbar (make-icon frame icon))))
  toolbar)

(defn add-icons [frame icons]
  (add-to-toolbar frame (get-toolbar frame) icons)
  (.validate frame))

(def *quit-icon* {:name "Quit" :icon "???" :handler (fn [frame _] (.dispose frame))})

; shortcuts for toolbar creation
(def | {})

(def *standard-icons* [*quit-icon* |])


;;; Tabs

(defn get-tabpane [frame]
  (get-component frame #(= (class %) javax.swing.JTabbedPane)))

;;; Clojure REPL


;;; Conexp Main Frame

(defn conexp-main-frame []
  (let [main-frame (JFrame. "conexp-clj")]
    (doto main-frame
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setSize 1000 800)
      (.setJMenuBar (JMenuBar.))
      (.setContentPane (JPanel. (BorderLayout.))))
    (add-menus main-frame *standard-menus*)
    (let [toolbar (JToolBar.)]
      (.setFloatable toolbar false)
      (.. main-frame getContentPane (add toolbar BorderLayout/PAGE_START))
      (add-icons main-frame *standard-icons*))
    (let [tabbed-pane (JTabbedPane.)
	  clj-repl    (JPanel.)
	  split-pane  (JSplitPane. JSplitPane/VERTICAL_SPLIT)]
      (doto tabbed-pane
	(.addTab  "Beispiel-Tab" (JLabel. "Hier kommen dann Tabs hin.")))
      (doto clj-repl
	(.add (JLabel. "Hier kommt eine REPL hin.")))
      (doto split-pane
	(.setTopComponent tabbed-pane)
	(.setBottomComponent clj-repl)
	(.setOneTouchExpandable true)
	(.setResizeWeight 0.8)
	(.setDividerLocation 1000))
      (doto (.getContentPane main-frame)
	(.add split-pane BorderLayout/CENTER)))
    main-frame))
