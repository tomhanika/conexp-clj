(ns conexp.gui.base
  (:import [javax.swing JFrame JMenuBar JMenu JMenuItem Box]
	   [java.awt GridLayout]
	   [java.awt.event KeyEvent ActionListener])
  (:use clojure.contrib.repl-utils))

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
	    (.addActionListener menu-item
              (proxy [ActionListener] []
		(actionPerformed [evt]
				 ((:handler entry) frame evt))))
	    (.add menu menu-item))))
      menu)))

(defn add-to-menubar
  "Adds menubar consiting of menus to frame."
  ([frame menus]
     (add-to-menubar frame (JMenuBar.) menus))
  ([frame menu-bar menus]
     (doseq [menu (map #(make-menu frame %) menus)]
       (.add menu-bar menu))
     menu-bar))

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


;;; Conexp Main Frame

(let [main-frame (JFrame. "conexp-clj")]
  (defn conexp-main-frame []
    (doto main-frame
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setJMenuBar (add-to-menubar main-frame [*main-menu* (Box/createGlue) *help-menu*]))
      (.setSize 1000 800))))
