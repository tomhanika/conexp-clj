(ns conexp.gui.base
  (:import [javax.swing JFrame JMenuBar JMenu JMenuItem Box JToolBar JPanel
	                JButton ImageIcon JSeparator JTabbedPane JSplitPane
	                JLabel JTextArea JScrollPane]
	   [java.awt GridLayout BorderLayout Dimension Image Font Color])
  (:use clojure.contrib.repl-utils
	clojure.contrib.core
	conexp.util
	conexp.gui.util
	conexp.gui.repl
	conexp.gui.plugins))

;;; Menus

(def *main-menu* {:name "Main",
		  :content [{:name "Add Menu", ; just for fun
			     :handler (fn [frame _]
					(add-menus frame
						   [{:name "?",
						     :content []}]))},
			    ---
			    {:name "Quit",
			     :handler (fn [frame _]
					(.dispose frame))}]})

(def *help-menu* {:name "Help",
		  :content []})

(def *standard-menus* [*main-menu* === *help-menu*])


;;; Toolbar

(def *quit-icon* {:name "Quit",
		  :icon "???",
		  :handler (fn [frame _] (.dispose frame))})

(def *standard-icons* [*quit-icon* |])


;;; Clojure REPL

(defn make-repl 
  "Creates a default Clojure REPL."
  []
  (let [rpl (make-clojure-repl)]
    (doto rpl
      (.setFont (Font. "Monospaced" Font/PLAIN 16))
      (.setBackground Color/BLACK)
      (.setForeground Color/WHITE)
      (.setCaretColor Color/RED))
    (JScrollPane. rpl)))


;;; Plugin Manager
;; TODO: Write a function which adds a plugin manager to a given frame
;; (defn add-plugin-manager [frame]
;;   ... add menu for plugins ...)

;;; Conexp Main Frame

(defn conexp-main-frame 
  "Returns main frame for conexp standard gui."
  []
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
	  clj-repl    (make-repl)
	  split-pane  (JSplitPane. JSplitPane/VERTICAL_SPLIT)]
      (doto split-pane
	(.setTopComponent tabbed-pane)
	(.setBottomComponent clj-repl)
	(.setOneTouchExpandable true)
	(.setResizeWeight 0.8)
	(.setDividerLocation 1000))
      (doto (.getContentPane main-frame)
	(.add split-pane BorderLayout/CENTER))
      (add-tab main-frame (JLabel. "Test Tab")))
    main-frame))

nil
