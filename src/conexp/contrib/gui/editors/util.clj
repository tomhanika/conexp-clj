;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.gui.editors.util
  (:import [javax.swing JSplitPane JRootPane JTextArea JTable JList JTree
             JScrollPane JOptionPane KeyStroke JComponent AbstractAction
             JToolBar JButton UIManager]
    [javax.swing.tree DefaultTreeModel DefaultMutableTreeNode 
      TreeSelectionModel]
    [java.util Vector]
    [java.awt Toolkit Dimension]
    [java.awt.event KeyEvent ActionEvent ActionListener MouseEvent 
      MouseListener InputEvent]
    [java.awt.datatransfer DataFlavor StringSelection]
    [javax.swing.event TreeSelectionListener TableModelListener]
    [javax.swing.table DefaultTableModel])
  (:use clojure.contrib.swing-utils
    conexp.contrib.gui.util
    conexp.util
    conexp.util.hookable
    conexp.util.typecheck
    [clojure.contrib.string :only (join split-lines split)]
    [clojure.set :only (union)]))


;;;
;;; General purpose & macros
;;;


(defmacro rho
  "Takes an block that defines a function by omitting its last parameter
    and turns it into a normal function that takes one parameter"
  [ & body ]
  `(fn [x#] (~@body x#)))

(defn *comp
  "Takes some functions and returns the covariant function composition
    of them"
  [& fns]
  (apply comp (reverse fns)))

(defmacro do-map-tree
  "Returns a function that maps a named list tree using the given functions
   to a new tree structure, utilizing side effects heavily.
   (This is Java inspired madness....)
   (A list tree is a tree which nodes are of the form (name child1 child2)
    (where the childs are optional) such that the whole tree is represented
    by its root node.)
 
   Parameters:
     mk        _function that maps a node name to the new node data structure
     do-add    _function that takes as arguments the root and the child node
                (as side effect!!)"
  [mk do-add]
  `(fn mapper# [tree#]
     (let [root# (~mk (first tree#))
            childs# (rest tree#)]
       (doseq [x# childs#] (~do-add root# (mapper# x#)))
       root#)))

(defmacro do-map-tree*
  "Maps a named list tree using the given functions to a new tree structure,
   utilizing side effects heavily. (This is Java inspired madness....)
   (A list tree is a tree which nodes are of the form (name child1 child2)
    (where the childs are optional) such that the whole tree is represented
    by its root node.)
 
   Parameters:
     mk        _function that maps a node name to the new node data structure
     do-add    _function that takes as arguments the root and the child node
                (as side effect!!)
     tree      _the tree that is mapped"
  [mk do-add tree]
  `((do-map-tree ~mk ~do-add) ~tree))

(defmacro apply-exprs
  "Takes an object and a seq of vectors, such that each vectors first element
   is a function that will be called with the object as first parameter and
   the rest of the vector as subsequent parameters."
  [object vectors]
  `(doseq [call# ~vectors] (apply (first call#) ~object (rest call#))))


(defn extract-array
  "Takes a java-array and turns it into a list"
  [array]
  (seq array))

(defmacro when-mask
  "Takes two bitmasks and checks whether their bitwise-and is not 0,
   if so, calls all other expressions in an implicit do."
  [bit-mask-1 bit-mask-2 & exprs]
  `(when (not= 0 (bit-and ~bit-mask-1 ~bit-mask-2)) ~@exprs))


;;;
;;; java & swing utils
;;;

(defn message-box
  "Pops up a swing message box.

   Parameters:
     text    _message text to be displayed
     title   _optional title of the message box-equal
  "
  ( [text title] (with-swing-threads
                   (JOptionPane/showMessageDialog nil (str text) (str title) 0)))
  ( [text] (with-swing-threads
             (JOptionPane/showMessageDialog nil (str text) "Info" 0))))

;;
;; clipboard functions
;;

(defn-swing get-clipboard-contents
  "Returns the contents of the system clipboard"
  []
  (let [toolkit (Toolkit/getDefaultToolkit)
         clipboard (.getSystemClipboard toolkit)
         transferable (.getContents clipboard nil)]
    (if (.isDataFlavorSupported transferable DataFlavor/stringFlavor)
      (.getTransferData transferable DataFlavor/stringFlavor)
      nil)))

(defn-swing-threads* set-clipboard-contents
  "Set the contents of the system clipboard to the given string
  Parameters:
    contents    _contents that the system clipboard will be set to
                 (implicitly converted by str)"
  [contents]
  (let [ toolkit (Toolkit/getDefaultToolkit)
         clipboard (.getSystemClipboard toolkit)
         data (StringSelection. (str contents))]
    (.setContents clipboard data nil)))

;;
;; resources
;;

(defn-swing get-ui-resource
  "Returns the resource associated with object.

  Parameters:
    object   _object identifying the resource"
  [object]
  (UIManager/get object))

(defn-swing get-ui-icon
  "Returns the icon resource associated with object.

  Parameters:
    object   _object identifying the resource"
  [object]
  (UIManager/getIcon object))

;;
;; mouse-click-interface-helpers
;;

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
     clicked   _mouse button clicked"
  [pressed released entered exited clicked]
  (let [ translate-button {MouseEvent/NOBUTTON 0
                           MouseEvent/BUTTON1  1
                           MouseEvent/BUTTON2  2
                           MouseEvent/BUTTON3  3}
         translate-modifier (fn [m]
                              (union 
                                (when-mask m InputEvent/ALT_DOWN_MASK #{:alt})
                                (when-mask m InputEvent/ALT_GRAPH_DOWN_MASK #{:alt-gr})
                                (when-mask m InputEvent/CTRL_DOWN_MASK #{:control})
                                (when-mask m InputEvent/SHIFT_DOWN_MASK #{:shift})
                                (when-mask m InputEvent/META_DOWN_MASK #{:meta})))
         translate-pressed-btns (fn [m]
                                  (union 
                                    (when-mask m InputEvent/BUTTON1_DOWN_MASK #{1})
                                    (when-mask m InputEvent/BUTTON2_DOWN_MASK #{2})
                                    (when-mask m InputEvent/BUTTON3_DOWN_MASK #{3})))
         translate-event (fn [event] 
                           {:button (translate-button (.getButton event))
                            :modifiers (translate-modifier (.getModifiersEx event))
                            :position [(.getX event) (.getY event)]
                            :buttons-down (translate-pressed-btns (.getModifiersEx event))})]

    (proxy [MouseListener] []
      (mousePressed [event] 
        (with-swing-threads* (pressed (translate-event event))))
      (mouseReleased [event]
        (with-swing-threads* (released (translate-event event))))
      (mouseEntered [event]
        (with-swing-threads* (entered (translate-event event))))
      (mouseExited [event]
        (with-swing-threads* (exited (translate-event event))))
      (mouseClicked [event]
        (with-swing-threads* (clicked (translate-event event)))))))


;;
;; managed/unmanaged interop
;;

(defrecord widget [widget])
(defrecord control [widget control])
(derive ::control ::widget)



(defn managed-by-conexp-gui-editors-util? 
  "Returns true if the object given as parameter is managed by the
   conexp.contrib.gui.editors.util module.
   
   Parameters:
     thing       _object to check whether it is managed."
  [thing]
  (or
    (and (map? thing)
    (contains? thing :managed-by-conexp-gui-editors-util))
    (isa? (class-to-keyword (type thing)) ::widget)))

(defn get-widget
  "Returns the appropriate java root widget for managed java code or
   just the input parameter for other objects.

   Parameters:
     obj         _object representing some java object"
  [obj]
  (if (managed-by-conexp-gui-editors-util? obj) (:widget obj) obj))

(defn get-control
  "Returns the appropriate java control widget for managed java code or
   just the input parameter for other objects.

   Parameters:
     obj         _object representing some java object"
  [obj]
  (let [t (class-to-keyword (type obj))]
    (if (isa? t ::control) (:control obj)
      (if (managed-by-conexp-gui-editors-util? obj) (:control obj) obj))))


(defn-typecheck-swing get-size ::widget
  "Returns the size of the given widget.

   Parameters:
     obj         _widget"
  [obj]
  (bean (.getSize (get-widget obj))))

(defn-typecheck-swing-threads* set-size ::widget
  "Sets the size of the given widget.

   Parameters:
     obj         _widget
     width       _width
     height      _height"
  [obj width height]
  (.setSize (get-widget obj) (Dimension. width height)))

(defn-typecheck set-width ::widget
  "Sets the width of the given widget.

   Parameters:
     obj         _widget
     width       _width"
  [obj width]
  (let [ height (:height (get-size obj)) ]
    (set-size obj width height)))

(defn-typecheck set-height ::widget
  "Sets the height of the given widget.

   Parameters:
     obj         _widget
     height      _height"
  [obj height]
  (let [ width (:width (get-size obj)) ]
    (set-size obj width height)))

(defn add-control-mouse-listener
  "Adds a mouse-listener proxy to the given control.

  Parameters:
   control  _object that will be passed to get-control
   proxy    _proxy that will be added"
  [control proxy]
  (.addMouseListener (get-control control) proxy))

;;
;;
;; Button
;;
;;
;;
;;

(defrecord button [widget])
(derive ::button ::widget)

(defn-typecheck-swing-threads* set-handler ::button
  "Sets the action handler for the button object.
  Parameters:
    obutton    _button object
    handler    _0-ary function that will be called on button press"
  [obutton handler]
  (let [ button (get-widget obutton)
         current-handlers (.getActionListeners button)
         listeners (extract-array current-handlers) ]
    (doseq [l listeners] (.removeActionListener button l))
    (let [action (proxy [ActionListener] []
                   (actionPerformed [event] 
                     (with-swing-threads* (handler))))]
      (.addActionListener button action))))


(defn-swing make-button
  "Creates a managed button object.

  Parameters:
    name     _Buttons name
    icon     _Buttons icon
    & setup  _an optional number of vectors that may contain additional
                  tweaks that are called after widget creation"
  [name icon & setup]
  (let [ jbutton (JButton. name icon)
         widget (button. jbutton)]
      (apply-exprs widget setup)
      widget ))

   

;;
;;
;;  Split Pane
;;
;;
;;
;;

(defrecord split-pane [widget])
(derive ::split-pane ::widget)

(defn-typecheck-swing-threads* set-divider-location ::split-pane
  "Sets the location of the divider.
   Parameters:
     osplit-pane _split-pane object
     location    _location of the divider (nbr of pixels from left/top)"
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


;;
;;
;; Tree Control
;;
;;
;;

(defrecord tree-control [widget control root model])
(derive ::tree-control ::control)


(defn-typecheck set-tree ::tree-control
  "Sets the tree that the control displays.

   Parameters:
     otree-control _tree-control object
     tree          _a tree represented by the root node and its children,
                    where each node is represented by a list that has
                    the node's name as first element and all its child
                    trees as rest, where each child is again represented by
                    its respective root node"
  [otree-control tree]
  (let [ t (conj (rest tree) :root)
         treeroot (:root otree-control)
         treemodel (:model otree-control) ]
    (with-swing-threads*
      (.removeAllChildren treeroot)
      (do-map-tree* (fn [x] (if (= :root x) treeroot 
                              (DefaultMutableTreeNode. x)))
        .add t)
      (.reload treemodel))))


(defn-typecheck-swing-threads* set-selection-mode ::tree-control
  "Sets the tree control's selection mode.

   Parameters:
     otree-control _tree-control object
     mode          _either :single, :multi (contiguous multiselection) or
                    :free (free multiselection)"
  [otree-control mode]
  (let [ treecontrol (get-control otree-control)
         selection-model (.getSelectionModel treecontrol)]
    (.setSelectionMode selection-model 
      ({:single TreeSelectionModel/SINGLE_TREE_SELECTION
        :multi TreeSelectionModel/CONTIGUOUS_TREE_SELECTION
        :free TreeSelectionModel/DISCONTIGUOUS_TREE_SELECTION} 
        mode))))


(defn-typecheck-swing-threads* set-selection-handler ::tree-control
  "Sets the selection-handler for the tree-control which is
   called every time the selection changes to handler.

   Parameters:
     otree-control _tree-control object
     handler       _function that will be called with a list of the selected
                     nodes represented each by a list of labels starting
                     from the root."
  [otree-control handler]
  (let [ treecontrol (get-control otree-control)
         current-handlers (.getTreeSelectionListeners treecontrol)
         listeners (extract-array current-handlers) ]
    (doseq [l listeners] (.removeTreeSelectionListener treecontrol l))
    (let [listener (proxy [TreeSelectionListener] []
                     (valueChanged [event] 
                       (with-swing-threads*
                         (let [ paths (.getSelectionPaths treecontrol)
                                treepaths (map (*comp
                                                 (rho .getPath)
                                                 extract-array
                                                 (rho map 
                                                   (rho .getUserObject)))
                                            (extract-array paths))]
                           (handler treepaths) ))))]
      (.addTreeSelectionListener treecontrol listener))))
         

(defn-swing make-tree-control
  "Creates a scrollable tree control object.

   Parameters:
     name        _the name of the root node of the tree
     & setup     _an optional number of vectors that may contain additional
                  tweaks that are called after widget creation
  " [name & setup] 
  (let [ treeroot     (DefaultMutableTreeNode. (str name))
         treemodel    (DefaultTreeModel. treeroot)
         treecontrol  (JTree. treemodel)
         pane         (JScrollPane. treecontrol 
                        JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED 
                        JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED)
         widget   (tree-control. pane treecontrol treeroot treemodel) ]
    (apply-exprs widget setup)
    widget ))


;;
;;
;;  Table
;;
;;
;;

(defrecord table-control [widget control hooks model])
(derive ::table-control ::control)
(derive ::table-control :conexp.util.hookable/hookable)


(defn-typecheck-swing get-row-count ::table-control
  "Returns the number of rows of the table control.

  Parameters:
    otable-control    _table-control object"
  [otable-control]
  (.getRowCount (get-control otable-control)))


(defn-typecheck-swing get-column-count ::table-control
  "Returns the number of columns of the table control.

  Parameters:
    otable-control    _table-control object"
  [otable-control]
  (.getColumnCount (get-control otable-control)))


(defn-typecheck-swing set-column-count ::table-control
  "Sets the number of columns of the table control.

  Parameters:
    otable-control    _table-control object
    count             _number of columns"
  [otable-control count]
  (.setColumnCount (:model otable-control) count))


(defn-typecheck-swing set-row-count ::table-control
  "Sets the number of rows of the table control.

  Parameters:
    otable-control    _table-control object
    count             _number of rows"
  [otable-control count]
  (.setRowCount (:model otable-control) count))



(defn-typecheck-swing-threads* register-keyboard-action ::table-control
  "Registers a keyboard action on the table.
  Parameters:
    otable     _table-control object
    handler    _function that will be called when the keystroke is hit,
                it will be given the table-control object as single parameter
    name       _name of the handler (implicit str)
    keystroke  _a corresponding keystroke object
    condition  _can be either :focus, :widget, or :ancestor
                (determines which widget has to be focused to trigger the
                 action)"
  [otable handler name keystroke condition]
  (let [ java-condition ({:focus JComponent/WHEN_FOCUSED
                          :widget JComponent/WHEN_IN_FOCUSED_WINDOW
                          :ancestor JComponent/WHEN_ANCESTOR_OF_FOCUSED_COMPONENT
                          } condition)
         widget otable
         table (get-control otable)
         action (proxy [AbstractAction ActionListener] []
                  (actionPerformed [event] 
                    (handler widget)))
         input-map (.getInputMap table java-condition)
         action-map (.getActionMap table)
         cmd-name (str name)]             
    (.put input-map keystroke cmd-name)
    (.put action-map cmd-name action)))

(defn-typecheck-swing get-column-index ::table-control
  "Returns the tables model index of the specified column in the view.

  Parameters:
    otable    _table-control object
    column    _viewport column"
  [otable column]
  (let [ table (get-control otable)
         col-count (.getColumnCount table) ]
    (if (>= column col-count) column
      (let [ col-model (.getColumnModel table)
             table-col (.getColumn col-model column) ]
        (.getModelIndex table-col)))))

(defn-typecheck-swing get-index-column ::table-control
 "Returns the column in the view that corresponds to the specified
  table model index.

  Parameters:
    otable    _table-control object
    index     _table model index"
  [otable index]
  (let [ table (get-control otable)
         col-model (.getColumnModel table)
         cols (range (.getColumnCount col-model))
         matching (filter (fn [x] (= index (.getModelIndex 
                                             (.getColumn col-model x))))
                    cols)
         found  (first matching)]
    (if found found index)))


(defn-typecheck get-row-index ::table-control
  "Returns the tables model index of the specified row in view.
  Parameters:
    row      _viewport row"
  [otable row]
  row)


(defn-typecheck get-index-row ::table-control
  "Returns the row in the view that corresponds
  to the specified table model index.

  Parameters:
    otable    _table-control object
    index     _table model index"
  [otable index]
  index)

(defn-typecheck get-row-index-permutator ::table-control
  "Returns a function that will map the current view rows to
   the according index values.
  
  Parameters:
    otable    _table-control object"
  [otable] identity)


(defn-typecheck-swing get-column-index-permutator ::table-control
  "Returns a function that will map the current view
    columns to the according index values.
  
  Parameters:
    otable    _table-control object"
  [otable]
  (let [ table (get-control otable)
         col-count (.getColumnCount table)
         col-range (range col-count)
         col-model (.getColumnModel table)
         col-map  (zipmap col-range (map (*comp
                                           (rho .getColumn col-model)
                                           (rho .getModelIndex))
                                      col-range)) ]
    (fn [i] (if (>= i col-count) i (col-map i)))))


(defn-typecheck-swing get-value-at-view ::table-control
  "Returns the value of the cell at specified position in view.

   Parameters:
     otable     _table-control object
     row        _row of the cell
     column     _column of the cell"
  [otable row column]
  (let [ table (get-control otable) ]
    (.getValueAt table row column)))


(defn-typecheck-swing get-value-at-index ::table-control
  "Returns the value of the cell at specified position in the table model.

   Parameters:
     otable     _table-control object
     row        _row of the cell
     column     _column of the cell"
  [otable row column]
  (let [ irow (get-index-row otable row)
         icolumn (get-index-column otable column)]
    (get-value-at-view otable irow icolumn)))



(defn-typecheck-swing-threads* set-resize-mode ::table-control
   "Sets the behaviour of the table on resize.
    Parameters:
      otable    _table-control object
      mode      _either :all, :last, :next, :off, or :subseq"
  [otable mode]
  (.setAutoResizeMode (get-control otable)
    ({:all JTable/AUTO_RESIZE_ALL_COLUMNS
      :last JTable/AUTO_RESIZE_LAST_COLUMN
      :next JTable/AUTO_RESIZE_NEXT_COLUMN
      :off JTable/AUTO_RESIZE_OFF
      :subseq JTable/AUTO_RESIZE_SUBSEQUENT_COLUMNS} mode)))


(defn-typecheck-swing-threads* set-cell-selection-mode ::table-control
  "Sets the cell selection mode.
   Parameters:
     otable    _table-control object
     mode      _either :none, :rows, :columns or :cells"
  [otable mode]
  (let [table (get-control otable)]
    (cond (= mode :none) (.setCellSelectionEnabled table false)
      (= mode :rows) (doto table (.setColumnSelectionAllowed false)
                       (.setRowSelectionAllowed true))
      (= mode :columns) (doto table (.setRowSelectionAllowed false)
                          (.setColumnSelectionAllowed true))
      (= mode :cells) (.setCellSelectionEnabled table true))))
  
(defn-typecheck-swing set-value-at-view ::table-control
  "Sets the value of a cell in the table according to a view position.
   Parameters:
     otable     _table-control object
     row        _integer identifying a row
     column     _integer identifying a column
     contents   _the new contents"
  [otable row column contents]
  (let [ columns (get-column-count otable)
         rows  (get-row-count otable) ]
    (if (>= column columns)
      (call-hook otable "extend-columns-to" (+ 1 column)))
    (if (>= row rows)
      (call-hook otable "extend-rows-to" (+ 1 row)))
    (.setValueAt (get-control otable) (str contents) row column)))


(defn-typecheck set-value-at-index ::table-control
  "Sets the value of a cell in the table according to the model index.
   Parameters:
     otable     _table-control object
     row        _integer identifying a row
     column     _integer identifying a column
     contents   _the new contents"
  [otable row column contents]
  (set-value-at-view otable (get-index-row otable row)
    (get-index-column otable column) contents))


(defn-typecheck update-value-at-index ::table-control
  "Sets the value of a cell in the table according to the model index,
   if it is different from the current cells value.
   Parameters:
     otable     _table-control object
     row        _integer identifying a row
     column     _integer identifying a column
     contents   _the new contents"
  [otable row column contents]
  (let [ current (get-value-at-index otable row column) ]
    (if (not= current contents)
      (set-value-at-index otable row column contents))))


(defn-typecheck-swing-threads* paste-from-clipboard ::table-control
  "Pastes the current system clipboard contents into the table.

   Parameters:
     obj        _table-control object"
  [obj] 
  (let [control (get-control obj)
         sel-columns ((*comp (rho .getSelectedColumns)  extract-array)
                       control)
         sel-rows ((*comp (rho .getSelectedRows) extract-array)
                    control)
         col-index (get-column-index-permutator obj)
         row-index (get-row-index-permutator obj)]
    (if (or (empty? sel-columns) (empty? sel-rows)) nil
      (let [data (str (get-clipboard-contents))
             startx (first sel-columns)
             starty (first sel-rows)
             cells (vec (map (*comp (rho split #"\t") (rho vec))
                          (split-lines data)))
             lns (range (count cells))]
        (doseq [r lns]  (doseq [c (range (count (cells r)))]
                          (set-value-at-index obj
                            (row-index (+ starty r))
                            (col-index (+ startx c))
                            ((cells r) c))))))))

(defn-typecheck-swing-threads* copy-to-clipboard ::table-control
  "Copies the selected cells from the table widget to the system
   clipboard.
  
   Parameters:
     obj       _table-control object"
  [obj] 
  (let [control (get-control obj)
         sel-columns ((*comp (rho .getSelectedColumns) extract-array)
                       control)
         sel-rows ((*comp (rho .getSelectedRows) extract-array)
                    control)
         sel-pairs (map (fn [y] (map (fn [x] (list y x)) sel-columns))
                     sel-rows)
         sel-values (map (fn [l] (map (fn [p] (str (get-value-at-view obj
                                                     (first p) 
                                                     (second p)))) l))
                      sel-pairs)
         sel-lines (map (rho join "\t") sel-values)
         selection (join "\n" sel-lines)]
    (set-clipboard-contents selection)))


(defn- table-change-hook
  [otable column first-row last-row type] 
  (if (and
        (= 0 type)
        (<= 0 (min column first-row last-row))
        (= first-row last-row))
    (let [ current-value (get-value-at-index otable first-row column)
           good-value (call-hook otable "cell-value" first-row column
                        current-value)]
      (if (not= current-value good-value)
        (set-value-at-index otable first-row column good-value)))))



(defn-swing make-table-control
  "Creates a table control in Java.

  Parameters:
     & setup     _an optional number of vectors that may contain additional
                  tweaks that are called after widget creation"
  [ & setup ]
  (let [ model (DefaultTableModel.)
         table (JTable. model)
         pane  (JScrollPane. table 
                 JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED 
                 JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED)

         keystroke-copy (KeyStroke/getKeyStroke KeyEvent/VK_C
                          ActionEvent/CTRL_MASK false)
         keystroke-paste (KeyStroke/getKeyStroke KeyEvent/VK_V
                           ActionEvent/CTRL_MASK false)


         hooks (:hooks (make-hookable))
         widget (table-control. pane table hooks model) 
         change-listener (proxy [TableModelListener] [] 
                           (tableChanged [event]
                             (with-swing-threads*
                               (let [ column (.getColumn event)
                                      first  (.getFirstRow event)
                                      last   (.getLastRow event)
                                      type   (.getType event)]
                                 (call-hook widget "table-changed"
                                   column first last type)))))
         defaults [ [set-resize-mode :off] 
                    [set-cell-selection-mode :cells]
                    [register-keyboard-action 
                      copy-to-clipboard "Copy" keystroke-copy :focus] 
                    [register-keyboard-action 
                      paste-from-clipboard "Paste" keystroke-paste :focus]]
         ignore-event (fn [x] nil)
         show-data (fn [x] (message-box (str x)))
         cell-permutor (proxy-mouse-listener ignore-event ignore-event
                         ignore-event ignore-event ignore-event)]
    (add-hook widget "table-changed" 
      (fn [c f l t] (table-change-hook widget c f l t))
      "This hook is called whenever a table widget is changed,
       Parameters:
             column   _the column index of the changed area
             first    _the first row index of the changed area 
             last     _the last row index of the changed area
             type     _(-1,0, or 1) delete, update, insert")       
    (add-hook widget "extend-columns-to" (rho set-column-count widget)
      "This hook is called whenever the table needs to extend its
       columns in order to write to a cell. The hook is supposed
       to call set-column-count on the table widget.
       Parameters:
             columns  _the requested column count")
    (add-hook widget "extend-rows-to" (rho set-row-count widget)
      "This hook is called whenever the table needs to extend its
       rows in order to write to a cell. The hook is supposed
       to call set-row-count on the table widget.
       Parameters:
             rows  _the requested column count")
    (add-hook widget "cell-value" (fn [_ _ contents] contents)
      "This hook is called to validate the cells value. The hook is
       supposed to return a valid cell value for that cell index.
       Parameters:
             row      _the cell's row index
             column   _the cell's column index
             contents _the requested (new) contents of the cell")
    (.addTableModelListener model change-listener)
    (apply-exprs widget defaults)
    (apply-exprs widget setup)
    (add-control-mouse-listener widget cell-permutor)
    widget ))


;;
;;
;;  Toolbar
;;
;;
;;

(defrecord toolbar-control [widget control])
(derive ::toolbar-control ::control)

(defn-typecheck-swing-threads* set-orientation ::toolbar-control
  "Sets the toolbars orientation.

  Parameters:
    otoolbar      _toolbar-control object
    orientation   _either :horiz or :vert"
  [otoolbar orientation]
  (.setOrientation (get-control otoolbar) ({:horiz JToolBar/HORIZONTAL
                                            :vert JToolBar/VERTICAL} 
                                            orientation)))

  
(defn-typecheck-swing-threads* add-button ::toolbar-control
  "Adds a button to the toolbar

  Parameters:
    otoolbar  _toolbar-control object
    button    _the button widget"
  [otoolbar button]
  (.add (get-control otoolbar) (get-widget button)))


(defn-typecheck-swing-threads* add-separator ::toolbar-control
  "Adds a separator space to the toolbar

  Parameters:
    otoolbar  _toolbar-control object"
  [otoolbar]
  (.addSeparator (get-control otoolbar)))

(defn-typecheck-swing-threads* set-floatable ::toolbar-control
  "Sets the floatable mode of the toolbar control.

  Parameters:
    otoolbar  _toolbar-control object
    floatable _make floatable or not"
  [otoolbar floatable]
  (.setFloatable (get-control otoolbar) (boolean floatable)))



(defn-swing make-toolbar-control
  "Creates a toolbar control in Java.

  Parameters:
     orientation _either :horiz or :vert
     & setup     _an optional number of vectors that may contain additional
                  tweaks that are called after widget creation"
  [orientation & setup]
  (let [ toolbar (JToolBar. ({:horiz JToolBar/HORIZONTAL
                                      :vert JToolBar/VERTICAL} 
                                      orientation))
         scrollpane (JScrollPane. toolbar
                      JScrollPane/VERTICAL_SCROLLBAR_ALWAYS
                      JScrollPane/HORIZONTAL_SCROLLBAR_NEVER)
         widget (toolbar-control. scrollpane toolbar) ]
    (apply-exprs widget setup)
    widget))


nil

