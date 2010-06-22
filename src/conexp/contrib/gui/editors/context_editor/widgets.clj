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
           [java.awt.event ActionListener MouseEvent InputEvent]
           [java.awt.datatransfer DataFlavor StringSelection]
           [javax.swing.event MouseInputAdapter])
  (:use clojure.contrib.swing-utils
        [clojure.contrib.string :only (join split-lines split)])
  (:use [conexp.base :exclude (join)]
        conexp.contrib.gui.util
        conexp.contrib.gui.util.hookable))


;;; General purpose & macros

(defmacro apply-exprs
  "Takes an object and a seq of vectors, such that each vectors first element
   is a function that will be called with the object as first parameter and
   the rest of the vector as subsequent parameters."
  [object vectors]
  `(doseq [call# ~vectors]
     (apply (first call#) ~object (rest call#))))

(defmacro when-mask
  "Takes two bitmasks and checks whether their bitwise-and is not 0,
   if so, calls all other expressions in an implicit do."
  [bit-mask-1 bit-mask-2 & exprs]
  `(when (not= 0 (bit-and ~bit-mask-1 ~bit-mask-2))
     ~@exprs))


;;; java & swing utils

(defn message-box
  "Pops up a swing message box."
  ([text title]
     (with-swing-threads
       (JOptionPane/showMessageDialog nil (str text) (str title) 0)))
  ([text]
     (with-swing-threads
       (JOptionPane/showMessageDialog nil (str text) "Info" 0))))


;;; clipboard functions

(defn-swing get-clipboard-contents
  "Returns the contents of the system clipboard"
  []
  (let [toolkit (Toolkit/getDefaultToolkit),
        clipboard (.getSystemClipboard toolkit),
        transferable (.getContents clipboard nil)]
    (if (.isDataFlavorSupported transferable DataFlavor/stringFlavor)
      (.getTransferData transferable DataFlavor/stringFlavor)
      nil)))

(defn-swing-threads* set-clipboard-contents
  "Set the contents of the system clipboard to the given string
  contents."
  [contents]
  (let [toolkit (Toolkit/getDefaultToolkit),
        clipboard (.getSystemClipboard toolkit),
        data (StringSelection. (str contents))]
    (.setContents clipboard data nil)))


;;; mouse-click-interface-helpers

(defn proxy-mouse-listener
  "Returns a proxy class that will implement the MouseListener
   interface and call the given 1-ary functions accordingly.
   The passed parameter will be a map mapping :button (button
   that triggered the event), :buttons-down (buttons that were
   down before), :modifiers and :position according to the event
   information.

   Parameters:
     pressed   _mouse button down event
     released  _mouse button up event
     entered   _mouse enters widget
     exited    _mouse exits widget
     clicked   _mouse button clicked
     moved     _mouse has been moved
     dragged   _mouse has been dragged"
  [pressed released entered exited clicked moved dragged]
  (let [translate-button {MouseEvent/NOBUTTON 0
                          MouseEvent/BUTTON1  1
                          MouseEvent/BUTTON2  2
                          MouseEvent/BUTTON3  3},
        translate-modifier (fn [m]
                             (union (when-mask m InputEvent/ALT_DOWN_MASK #{:alt})
                                    (when-mask m InputEvent/ALT_GRAPH_DOWN_MASK #{:alt-gr})
                                    (when-mask m InputEvent/CTRL_DOWN_MASK #{:control})
                                    (when-mask m InputEvent/SHIFT_DOWN_MASK #{:shift})
                                    (when-mask m InputEvent/META_DOWN_MASK #{:meta}))),
        translate-pressed-btns (fn [m]
                                 (union (when-mask m InputEvent/BUTTON1_DOWN_MASK #{1})
                                        (when-mask m InputEvent/BUTTON2_DOWN_MASK #{2})
                                        (when-mask m InputEvent/BUTTON3_DOWN_MASK #{3})))
        translate-event (fn [event]
                          {:button (translate-button (.getButton event))
                           :modifiers (translate-modifier (.getModifiersEx event))
                           :position [(.getX event) (.getY event)]
                           :buttons-down (translate-pressed-btns (.getModifiersEx event))})]
    (proxy [MouseInputAdapter] []
      (mousePressed [event]
        (with-swing-threads* (pressed (translate-event event))))
      (mouseReleased [event]
        (with-swing-threads* (released (translate-event event))))
      (mouseEntered [event]
        (with-swing-threads* (entered (translate-event event))))
      (mouseExited [event]
        (with-swing-threads* (exited (translate-event event))))
      (mouseClicked [event]
        (with-swing-threads* (clicked (translate-event event))))
      (mouseMoved [event]
        (with-swing-threads* (moved (translate-event event))))
      (mouseDragged [event]
        (with-swing-threads* (dragged (translate-event event)))))))



;;; managed/unmanaged interop

(defrecord widget [widget])
(defrecord control [widget control])
(derive ::control ::widget)

(defn managed-by-conexp-gui-editors-util?
  "Returns true if the object given as parameter is managed by the
   conexp.contrib.gui.editors.util module."
  [thing]
  (or (and (map? thing)
           (contains? thing :managed-by-conexp-gui-editors-util))
      (isa? (class-to-keyword (type thing)) ::widget)))

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
  (let [t (class-to-keyword (type obj))]
    (if (isa? t ::control)
      (:control obj)
      (if (managed-by-conexp-gui-editors-util? obj)
        (:control obj)
        obj))))

(defn-typecheck-swing get-size ::widget
  "Returns the size of the given widget."
  [obj]
  (bean (.getSize (get-widget obj))))

(defn-typecheck-swing-threads* set-size ::widget
  "Sets the size of the given widget obj."
  [obj width height]
  (.setSize (get-widget obj)
            (Dimension. width height)))

(defn-typecheck set-width ::widget
  "Sets the width of the given widget obj."
  [obj width]
  (let [height (:height (get-size obj))]
    (set-size obj width height)))

(defn-typecheck set-height ::widget
  "Sets the height of the given widget."
  [obj height]
  (let [width (:width (get-size obj))]
    (set-size obj width height)))

(defn add-control-mouse-listener
  "Adds a mouse-listener proxy to the given control."
  [control proxy]
  (.addMouseListener (get-control control) proxy)
  (.addMouseMotionListener (get-control control) proxy))


;;; Button

(defrecord button [widget])
(derive ::button ::widget)

(defn-typecheck-swing-threads* set-handler ::button
  "Sets the action handler for the button object. handler must be a
  function of no arguments."
  [obutton handler]
  (let [button (get-widget obutton),
        current-handlers (.getActionListeners button),
        listeners (seq current-handlers)]
    (doseq [l listeners]
      (.removeActionListener button l))
    (let [action (proxy [ActionListener] []
                   (actionPerformed [event]
                     (with-swing-threads* (handler))))]
      (.addActionListener button action))))

(defn-swing make-button
  "Creates a managed button object. setup is an optional number of
   vectors that may contain additional tweaks that are called after
   widget creation"
  [name & setup]
  (let [jbutton (JButton. name),
        widget  (button. jbutton)]
    (apply-exprs widget setup)
    widget))


;;;  Split Pane

(defrecord split-pane [widget])
(derive ::split-pane ::widget)

(defn-typecheck-swing-threads* set-divider-location ::split-pane
  "Sets the location of the divider."
  [osplit-pane location]
  (let [split-pane (get-widget osplit-pane)]
    (.setDividerLocation split-pane location)))

(defn-swing make-split-pane
  "Creates a managed split pane object.

   Parameters:
     direction   _may be eighter :horiz or :vert
     topleft     _top or left widget
     bottomright _bottom or right widget
     & setup     _an optional number of vectors that may contain additional
                  tweaks that are called after widget creation, i.e.
                  [:set-divider-location 150] will call the
                  :set-divider-location map with 150 as single parameter
  "
  [direction topleft bottomright & setup]
  (let [ jsplit-pane (JSplitPane. (direction {:horiz JSplitPane/HORIZONTAL_SPLIT
                                    :vert JSplitPane/VERTICAL_SPLIT})
                       (get-widget topleft)
                       (get-widget bottomright))
         widget  (split-pane. jsplit-pane)]
    (apply-exprs widget setup)
    widget ))


;;;  Toolbar

(defrecord toolbar-control [widget control])
(derive ::toolbar-control ::control)

(defn-typecheck-swing-threads* set-orientation ::toolbar-control
  "Sets the toolbars orientation. orientation is either :horiz
  or :vert."
  [otoolbar orientation]
  (.setOrientation (get-control otoolbar)
                   ({:horiz JToolBar/HORIZONTAL
                     :vert JToolBar/VERTICAL}
                    orientation)))

(defn-typecheck-swing-threads* add-button ::toolbar-control
  "Adds a button to the toolbar."
  [otoolbar button]
  (.add (get-control otoolbar)
        (get-widget button)))

(defn-typecheck-swing-threads* add-separator ::toolbar-control
  "Adds a separator space to the toolbar."
  [otoolbar]
  (.addSeparator (get-control otoolbar)))

(defn-typecheck-swing-threads* set-floatable ::toolbar-control
  "Sets the floatable mode of the toolbar control."
  [otoolbar floatable]
  (.setFloatable (get-control otoolbar)
                 (boolean floatable)))

(defn-swing make-toolbar-control
  "Creates a toolbar control in Java. orientation is either :horiz
  or :vert and setup is an optional number of vectors that may contain
  additional tweaks that are called after widget creation"
  [orientation & setup]
  (let [toolbar    (JToolBar. ({:horiz JToolBar/HORIZONTAL
                                :vert JToolBar/VERTICAL}
                               orientation)),
        scrollpane (JScrollPane. toolbar
                                 JScrollPane/VERTICAL_SCROLLBAR_ALWAYS
                                 JScrollPane/HORIZONTAL_SCROLLBAR_NEVER)
        widget     (toolbar-control. scrollpane toolbar)]
    (apply-exprs widget setup)
    widget))

;;;

nil
