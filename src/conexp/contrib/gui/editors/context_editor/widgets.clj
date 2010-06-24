;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; This file has been written by Immanuel Albrecht, with modifications by DB

(ns conexp.contrib.gui.editors.context-editor.widgets
  (:import [javax.swing JSplitPane JScrollPane JOptionPane JToolBar JButton]
           [java.awt Toolkit Dimension]
           [java.awt.event ActionListener]
           [java.awt.datatransfer DataFlavor StringSelection])
  (:use clojure.contrib.swing-utils
        [clojure.contrib.string :only (join split-lines split)])
  (:use [conexp.base :exclude (join)]
        conexp.contrib.gui.util
        conexp.contrib.gui.util.hookable))


;;; clipboard functions

(defn-swing get-clipboard-contents
  "Returns the contents of the system clipboard"
  []
  (let [toolkit      (Toolkit/getDefaultToolkit),
        clipboard    (.getSystemClipboard toolkit),
        transferable (.getContents clipboard nil)]
    (if (.isDataFlavorSupported transferable DataFlavor/stringFlavor)
      (.getTransferData transferable DataFlavor/stringFlavor)
      nil)))

(defn-swing set-clipboard-contents
  "Set the contents of the system clipboard to the given string
  contents."
  [contents]
  (let [toolkit   (Toolkit/getDefaultToolkit),
        clipboard (.getSystemClipboard toolkit),
        data      (StringSelection. (str contents))]
    (.setContents clipboard data nil)))


;;; managed/unmanaged interop

(defwidget widget [] [widget])
(defwidget control [widget] [widget control])

(defn- managed-by-conexp-gui-editors-util?
  "Returns true if the object given as parameter is managed by the
   conexp.contrib.gui.editors.util module."
  [thing]
  (or (and (map? thing)
           (contains? thing :managed-by-conexp-gui-editors-util))
      (keyword-isa? thing widget)))

(defn get-widget
  "Returns the appropriate java root widget for managed java code or
   just the input parameter for other objects."
  [obj]
  (if (managed-by-conexp-gui-editors-util? obj)
    (:widget obj)
    obj))

(defn get-control
  "Returns the appropriate java control widget for managed java code or
   just the input parameter for other objects."
  [obj]
  (if (or (keyword-isa? obj control)
          (managed-by-conexp-gui-editors-util? obj))
    (:control obj)
    obj))

(defn-swing get-size
  "Returns the size of the given widget."
  [obj]
  (assert (keyword-isa? obj widget))
  (bean (.getSize (get-widget obj))))

(defn-swing set-size
  "Sets the size of the given widget obj."
  [obj width height]
  (assert (keyword-isa? obj widget))
  (.setSize (get-widget obj)
            (Dimension. width height)))

(defn set-width
  "Sets the width of the given widget obj."
  [obj width]
  (assert (keyword-isa? obj widget))
  (let [height (:height (get-size obj))]
    (set-size obj width height)))

(defn set-height
  "Sets the height of the given widget."
  [obj height]
  (assert (keyword-isa? obj widget))
  (let [width (:width (get-size obj))]
    (set-size obj width height)))

(defn add-control-mouse-listener
  "Adds a mouse-listener proxy to the given control."
  [control proxy]
  (.addMouseListener (get-control control) proxy)
  (.addMouseMotionListener (get-control control) proxy))


;;; Button

(defwidget button [widget] [widget])

(defn-swing set-handler
  "Sets the action handler for the button object. handler must be a
  function of no arguments."
  [obutton handler]
  (assert (keyword-isa? obutton button))
  (let [button (get-widget obutton),
        current-handlers (.getActionListeners button),
        listeners (seq current-handlers)]
    (doseq [l listeners]
      (.removeActionListener button l))
    (let [action (proxy [ActionListener] []
                   (actionPerformed [event]
                     (with-swing-threads (handler))))]
      (.addActionListener button action))))

(defn-swing make-button
  "Creates a managed button object."
  [name]
  (let [jbutton (JButton. name),
        widget  (button. jbutton)]
    widget))


;;;  Split Pane

(defwidget split-pane [widget] [widget])

(defn-swing set-divider-location
  "Sets the location of the divider."
  [osplit-pane location]
  (assert (keyword-isa? osplit-pane split-pane))
  (let [split-pane (get-widget osplit-pane)]
    (.setDividerLocation split-pane location)))

(defn-swing make-split-pane
  "Creates a managed split pane object. direction is either :horiz
  or :vert, topleft is the top (left) widget and bottomright is the
  bottom (right) widget."
  [direction topleft bottomright]
  (let [jsplit-pane (JSplitPane. (direction {:horiz JSplitPane/HORIZONTAL_SPLIT
                                             :vert JSplitPane/VERTICAL_SPLIT})
                                 (get-widget topleft)
                                 (get-widget bottomright)),
        widget  (split-pane. jsplit-pane)]
    widget))


;;;  Toolbar

(defwidget toolbar-control [control] [widget control])

(defn-swing set-orientation
  "Sets the toolbars orientation. orientation is either :horiz
  or :vert."
  [otoolbar orientation]
  (assert (keyword-isa? otoolbar toolbar-control))
  (.setOrientation (get-control otoolbar)
                   ({:horiz JToolBar/HORIZONTAL
                     :vert JToolBar/VERTICAL}
                    orientation)))

(defn-swing add-button
  "Adds a button to the toolbar."
  [otoolbar button]
  (assert (keyword-isa? otoolbar toolbar-control))
  (.add (get-control otoolbar)
        (get-widget button)))

(defn-swing add-separator
  "Adds a separator space to the toolbar."
  [otoolbar]
  (assert (keyword-isa? otoolbar toolbar-control))
  (.addSeparator (get-control otoolbar)))

(defn-swing set-floatable
  "Sets the floatable mode of the toolbar control."
  [otoolbar floatable]
  (assert (keyword-isa? otoolbar toolbar-control))
  (.setFloatable (get-control otoolbar)
                 (boolean floatable)))

(defn-swing make-toolbar-control
  "Creates a toolbar control in Java."
  [orientation]
  (let [toolbar    (JToolBar. ({:horiz JToolBar/HORIZONTAL
                                :vert JToolBar/VERTICAL}
                               orientation)),
        scrollpane (JScrollPane. toolbar
                                 JScrollPane/VERTICAL_SCROLLBAR_ALWAYS
                                 JScrollPane/HORIZONTAL_SCROLLBAR_NEVER)
        widget     (toolbar-control. scrollpane toolbar)]
    widget))

;;;

nil
