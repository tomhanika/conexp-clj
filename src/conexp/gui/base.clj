(ns conexp.gui.base
  (:import [javax.swing JFrame JMenuBar JMenu JMenuItem JToolBar JPanel
	                JButton JSeparator JTabbedPane JSplitPane
	                JLabel JTextArea JScrollPane]
	   [java.awt GridLayout BorderLayout Dimension])
  (:use [conexp.base :only (defvar-)]
        conexp.gui.util
	conexp.gui.repl
	conexp.gui.plugins))


;;; Menus

(defvar- *main-menu* {:name "Main",
		      :content [---
				{:name "Quit",
				 :handler (fn [frame _]
					    (.dispose frame))}]}
  "Main menu for conexp-clj standard GUI.")

(defvar- *help-menu* {:name "Help",
		      :content [{:name "License"}
				---
				{:name "About"}]}
  "Help menu for conexp-clj standard GUI.")

(defvar- *standard-menus* [*main-menu* === *help-menu*]
  "Standard menus for conexp-clj GUI.")


;;; Toolbar

(defvar- *quit-icon* {:name "Quit",
		      :icon "???",
		      :handler (fn [frame _]
				 (.dispose frame))}
  "Quit icon.")

(defvar- *standard-icons* [*quit-icon* |]
  "Standard icons for conexp-clj GUI.")


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
	  clj-repl    (make-repl main-frame)
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
