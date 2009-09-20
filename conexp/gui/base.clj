(ns conexp.gui.base
  (:import [javax.swing JFrame JMenuBar JMenu JMenuItem Box JToolBar JPanel
	                JButton ImageIcon JSeparator]
	   [java.awt GridLayout BorderLayout Dimension]
	   [java.awt.event KeyEvent ActionListener])
  (:use clojure.contrib.repl-utils
	clojure.contrib.core))

;;; Helper

(defn add-handler [thing frame function]
  (.addActionListener thing
    (proxy [ActionListener] []
      (actionPerformed [evt]
	(function frame thing)))))

;;; Menus

(defn make-menu
  "Converts a hash repsenting a menu into an actual JMenu for a given frame."
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

(defn add-additional-menus
  "Adds the additional menus to the frame in front of the first Box.Filler
found in the menu-bar of frame."
  [frame menus]
  (let [menu-bar (.getJMenuBar frame)
	menu-bar-as-seq (map #(.getComponent menu-bar %) (range (.getMenuCount menu-bar)))
	menu-entries-before-filler (take-while #(not (instance? javax.swing.Box$Filler %))
					       menu-bar-as-seq)
	menu-entries-from-filler   (drop-while #(not (instance? javax.swing.Box$Filler %))
					       menu-bar-as-seq)
	new-menu-bar (JMenuBar.)]
    (add-to-menubar frame new-menu-bar menu-entries-before-filler)
    (add-to-menubar frame new-menu-bar menus)
    (add-to-menubar frame new-menu-bar menu-entries-from-filler)
    (.setJMenuBar frame new-menu-bar)
    (.validate frame)))

(def *main-menu* {:name "Main" :content [{:name "Add Menu" ; just for fun
					  :handler (fn [frame _]
						     (add-additional-menus frame
									   [{:name "?" :content []}]))}
					 {} ; Separator
					 {:name "Quit"
					  :handler (fn [frame _] (.dispose frame))}]})

(def *help-menu* {:name "Help" :content []})

(def *standard-menus* [*main-menu* (Box/createHorizontalGlue) *help-menu*])


;;; Tool Bar

(defn get-toolbar [frame]
  (first (for [comp (.. frame getContentPane getComponents) 
	       :when (instance? javax.swing.JToolBar comp)]
	   comp)))

(defn make-icon [frame icon-hash]
  (cond
    (empty? icon-hash) (JSeparator.)
    :else
    (let [button (JButton.)]
      (doto button
	(.setName (:name icon-hash))
	(add-handler frame (:handler icon-hash))
	(.setToolTipText (:name icon-hash)))
      (if-let [icon (:icon icon-hash)]
	(.setIcon button (-?> (.getResource (class Object) icon)
			      (ImageIcon.))))
      button)))

(defn add-to-toolbar [frame toolbar icons]
  (doseq [icon (map #(make-icon frame %) icons)]
    (.add toolbar icon))
  toolbar)

(defn add-additional-icons [frame icons]
  (add-to-toolbar frame (get-toolbar frame) icons)
  (.validate frame))

(def *quit-icon* {:name "Quit" :icon "???" :handler (fn [frame _] (.dispose frame))})

(def *standard-icons* [*quit-icon*])

;;; Tabs


;;; Clojure REPL


;;; Conexp Main Frame

(defn conexp-main-frame []
  (let [main-frame (JFrame. "conexp-clj")]
    (doto main-frame
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setSize 1000 800)
      (.setJMenuBar (JMenuBar.))
      (.setContentPane (JPanel. (BorderLayout.)))
      (.. getContentPane (add (JToolBar.))))
    (add-additional-menus main-frame *standard-menus*)
    (println main-frame)
    (add-additional-icons main-frame *standard-icons*)
    main-frame))






