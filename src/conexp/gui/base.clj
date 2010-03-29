;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.base
  (:import [javax.swing JFrame JMenuBar JMenu JMenuItem JToolBar JPanel
	                JButton JSeparator JTabbedPane JSplitPane
	                JLabel JTextArea JScrollPane]
	   [java.awt GridLayout BorderLayout Dimension])
  (:use [conexp.base :only (defvar-)]
        conexp.gui.util
	conexp.gui.repl
	conexp.gui.plugins
	[conexp.gui.plugins.base :only (load-plugin)]
	[conexp.gui.editors.contexts :only (context-editor)]
	[conexp.gui.editors.lattices :only (lattice-editor)]))


;;; Menus

(defvar- *main-menu* {:name "Main",
		      :content [---
				{:name "Quit",
				 :handler (fn [#^JFrame frame _]
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
		      :handler (fn [#^JFrame frame _]
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
	(.setDividerLocation 640))
      (doto (.getContentPane main-frame)
	(.add split-pane BorderLayout/CENTER)))

    ;; standard plugins
    (let [pm (get-plugin-manager main-frame)]
      (load-plugin pm context-editor)
      (load-plugin pm lattice-editor))

    main-frame))


;;;

nil
