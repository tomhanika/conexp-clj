(ns conexp.gui.base
  (:import [javax.swing JFrame JMenuBar JMenu JMenuItem JToolBar JPanel
	                JButton JSeparator JTabbedPane JSplitPane
	                JLabel JTextArea JScrollPane]
	   [java.awt GridLayout BorderLayout Dimension Font Color])
  (:use	conexp.gui.util
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

(defn add-plugin-manager
  "Adds a plugin-manager and all a corrsponding menu to frame."
  ;; unfinished
  [frame]
  (let [plugin-manager (make-plugin-manager frame)]
    ;; ... add menu for plugins ...
    ))


;;; Conexp Main Frame

(defn conexp-main-frame 
  "Returns main frame for conexp standard gui."
  []
  (let [main-frame (JFrame. "conexp-clj")]
    ;; main setup (including menu)
    (doto main-frame
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setSize 1000 800)
      (.setJMenuBar (JMenuBar.))
      (.setContentPane (JPanel. (BorderLayout.)))
      (add-menus *standard-menus*)
      (add-plugin-manager))

    ;; toolbar setup
    (let [toolbar (JToolBar.)]
      (.setFloatable toolbar false)
      (.. main-frame getContentPane (add toolbar BorderLayout/PAGE_START))
      (add-icons main-frame *standard-icons*))

    ;; tabbed-pane setup
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
      ;; for testing
      (add-tab main-frame (JLabel. "Test Tab")))

    main-frame))


;;;

nil
