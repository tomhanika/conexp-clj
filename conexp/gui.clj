(ns conexp.gui
  (:import [javax.swing JFrame JMenuBar JMenu JMenuItem]
	   [java.awt GridLayout]
	   [java.awt.event KeyEvent ActionListener])
  (:use clojure.contrib.repl-utils))

;;; Menus

(defn make-menu 
  "Converts a hash repsenting a menu into an actual JMenu for a given frame."
  [frame hash-menu]
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
    menu))

(defn add-to-menubar 
  "Adds menubar consiting of menus to frame."
  ([frame menus]
     (add-to-menubar frame (JMenuBar.) menus))
  ([frame menu-bar menus]
     (doseq [menu (map #(make-menu frame %) menus)]
       (.add menu-bar menu))
     menu-bar))

(defn add-additional-menus [frame menus]
  (add-to-menubar frame (.getJMenuBar frame) menus)
  (.setJMenuBar frame (.getJMenuBar frame))) ;; hackish

(def *main-menu* {:name "Main" :content [{:name "Add Menu" ; just for fun
					  :handler (fn [frame _] 
						     (add-additional-menus frame 
									   [{:name "?" :content []}]))}
					 {} ; Separator
					 {:name "Quit" 
					  :handler (fn [frame _] (.dispose frame))}]})
(def *help-menu* {:name "Help" :content []})


;;; Actual Conexp GUI

(let [main-frame (JFrame. "conexp-clj")]
  (defn conexp-gui []
    (doto main-frame
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setJMenuBar (add-to-menubar main-frame [*main-menu* *help-menu*]))
      (.setSize 1000 800)
      (.setVisible true))))
