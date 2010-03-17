;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.editors.util
  (:import [javax.swing JSplitPane JRootPane JTextArea JTable JList JTree
             JScrollPane JOptionPane KeyStroke JComponent AbstractAction
             JToolBar JButton UIManager]
    [javax.swing.tree DefaultTreeModel DefaultMutableTreeNode 
      TreeSelectionModel]
    [java.util Vector]
    [java.awt Toolkit]
    [java.awt.event KeyEvent ActionEvent ActionListener]
    [java.awt.datatransfer DataFlavor StringSelection]
    [javax.swing.event TreeSelectionListener TableModelListener]
    [javax.swing.table DefaultTableModel])
  (:use clojure.contrib.swing-utils
    conexp.gui.util
    conexp.util
    conexp.util.hookable
    [clojure.contrib.string :only (join split-lines split)]))

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

(defn unroll-parameters
  "Takes a vector that has a function as first argument and does a function
   call giving all other vector elements as parameters."
  [call_seq]
  (let [ call_vec (vec call_seq)
         length   (count call_vec)]
    (when (< 0 length) (apply (call_vec 0) (rest call_vec)))))

(defn unroll-parameters-fn-map
  "Takes a vector that has a function as first argument and does a function
   call giving all other vector elements as parameters, looking up the function
   in the function map"
  [fn_map call_seq]
  (let [ length   (count call_seq)]
    (when (< 0 length) (apply (fn_map (first call_seq)) (rest call_seq)))))

(defmacro apply-exprs
  "Takes an object and a seq of vectors, such that each vectors first element
   is a function that will be called with the object as first parameter and
   the rest of the vector as subsequent parameters."
  [object vectors]
  `(doseq [call# ~vectors] (apply (first call#) ~object (rest call#))))

(defn print-doc* [v]
  (println "-------------------------")
  (println " " (:doc (meta v))))

(defmacro doc*
  "Own implementation of doc that allows to document 'anonymous' functions
   which are bound to something via def"
  [v]
  `(if (contains? ^~v :alternate-doc) (print-doc* ~v) (doc ~v)))

(defmacro doc!
  "Own implementation of doc that also can take arguments that are not bound"
  [v]
  `(do
     (def object-to-be-documented# ~v)
     (doc* object-to-be-documented#)
     (ns-unmap *ns* 'object-to-be-documented#)))

(defmacro fn-doc
  "Create a documented anonymous function."
  [doc & rest]
  `(with-meta (fn ~@rest) {:doc ~doc :alternate-doc 0}))

(defmacro fn-swing-doc
  "Create a documented anonymous function."
  [doc & rest]
  `(with-meta (fn-swing ~@rest) {:doc ~doc :alternate-doc 0}))

(defmacro fn-swing-threads*-doc
  "Create a documented anonymous function."
  [doc & rest]
  `(with-meta (fn-swing-threads* ~@rest) {:doc ~doc :alternate-doc 0}))


(defn extract-array
  "Takes a java-array and turns it into a list"
  [array]
  (map (rho aget array) (range (alength array))))

(defmacro !!
  "Takes a map, a key and arbitrary additional parameters and calls
   the function that is the value of the map under the key with
   the given additional parameters"
  [map key & parameters]
  `((~map ~key) ~@parameters))

(defmacro !!!
  "Takes a map, a key and arbitrary additional parameters and calls
   the function that is the value of that map, which is the map under
   the key :handler, under the key with the given additional parameters"
  [map key & parameters]
  `(((~map :handler) ~key) ~@parameters))

(defmacro !!!!
  "Takes a widget, a key and arbitrary additional parameters and calls
   the handler function of the widget that is associated with the key
   with the widget as first parameter and the given additional parameters"
  [map key & parameters]
  `(((~map :handler) ~key) ~map ~@parameters))


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
;; managed/unmanaged interop
;;

(deftype widget [widget])
(deftype control [widget control])
(derive ::control ::widget)



(defn managed-by-conexp-gui-editors-util? 
  "Returns true if the object given as parameter is managed by the
   conexp.gui.editors.util module.
   
   Parameters:
     thing       _object to check whether it is managed."
  [thing]
  (or
    (and (map? thing)
    (contains? thing :managed-by-conexp-gui-editors-util))
    (isa? (type thing) ::widget)))

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
  (let [t (type obj)]
    (if (isa? t ::control) (:control obj)
      (if (managed-by-conexp-gui-editors-util? obj) (:control obj) obj))))



;;
;;
;; Button
;;
;;
;;
;;

(deftype button [widget])
(derive ::button ::widget)

(inherit-multimethod set-handler ::button
  "Sets the action handler for the button object.
  Parameters:
    obutton    _button object
    handler    _0-ary function that will be called on button press")

(defmethod-swing-threads* 
  set-handler ::button
  [obutton handler]
  (let [ button (get-widget obutton)
         current-handlers (.getActionListeners button)
         listeners (extract-array current-handlers) ]
    (doseq [l listeners] (.removeActionListener button l))
    (let [action (proxy [ActionListener] []
                   (actionPerformed [event] 
                     (handler)))]
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
         widget (button jbutton)]
      (apply-exprs widget setup)
      widget ))

   

;;
;;
;;  Split Pane
;;
;;
;;
;;

(deftype split-pane [widget])
(derive ::split-pane ::widget)

(inherit-multimethod set-divider-location ::split-pane
  "Sets the location of the divider.
   Parameters:
     osplit-pane _split-pane object
     location    _location of the divider (nbr of pixels from left/top)")

(defmethod-swing-threads* 
  set-divider-location ::split-pane
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
         widget  (split-pane jsplit-pane)]
    (apply-exprs widget setup)
    widget ))


;;
;;
;; Tree Control
;;
;;
;;

(deftype tree-control [widget control root model])
(derive ::tree-control ::control)


(inherit-multimethod set-tree ::tree-control
  "Sets the tree that the control displays.

   Parameters:
     otree-control _tree-control object
     tree          _a tree represented by the root node and its children,
                    where each node is represented by a list that has
                    the node's name as first element and all its child
                    trees as rest, where each child is again represented by
                    its respective root node")

(defmethod 
  set-tree ::tree-control 
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


(inherit-multimethod set-selection-mode ::tree-control
  "Sets the tree control's selection mode.

   Parameters:
     otree-control _tree-control object
     mode          _either :single, :multi (contiguous multiselection) or
                    :free (free multiselection)")

(defmethod-swing-threads* 
  set-selection-mode ::tree-control
  [otree-control mode]
  (let [ treecontrol (get-control otree-control)
         selection-model (.getSelectionModel treecontrol)]
    (.setSelectionMode selection-model 
      ({:single TreeSelectionModel/SINGLE_TREE_SELECTION
        :multi TreeSelectionModel/CONTIGUOUS_TREE_SELECTION
        :free TreeSelectionModel/DISCONTIGUOUS_TREE_SELECTION} 
        mode))))


(inherit-multimethod set-selection-handler ::tree-control
  "Sets the selection-handler for the tree-control which is
   called every time the selection changes to handler.

   Parameters:
     otree-control _tree-control object
     handler       _function that will be called with a list of the selected
                     nodes represented each by a list of labels starting
                     from the root.")

(defmethod-swing-threads*
  set-selection-handler ::tree-control
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
         widget   (tree-control pane treecontrol treeroot treemodel) ]
    (apply-exprs widget setup)
    widget ))


;;
;;
;;  Table
;;
;;
;;

(deftype table-control [widget control hooks model])
(derive ::table-control ::control)
(derive ::table-control :conexp.util.hookable/hookable)

;; (defn-swing do-mk-table-control
;;   "Creates a table control in Java.

;;   Parameters:
;;      & setup     _an optional number of vectors that may contain additional
;;                   tweaks that are called after widget creation"
;;   [& setup]
;;   (let [ self  (promise)
;;          model (DefaultTableModel.)
;;          table (JTable. model)
;;          keystroke-copy (KeyStroke/getKeyStroke KeyEvent/VK_C
;;                           ActionEvent/CTRL_MASK false)
;;          keystroke-paste (KeyStroke/getKeyStroke KeyEvent/VK_V
;;                            ActionEvent/CTRL_MASK false)
;;          pane  (JScrollPane. table 
;;                  JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED 
;;                  JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED)
;;          widget {:managed-by-conexp-gui-editors-util "table-control"

;;          :widget   pane
;;          :control  table
;;          :model    model
;;          :handler  (ref {})

(inherit-multimethod get-row-count ::table-control
  "Returns the number of rows of the table control.

  Parameters:
    otable-control    _table-control object")

(defmethod-swing 
  get-row-count ::table-control
  [otable-control]
  (.getRowCount (get-control otable-control)))


(inherit-multimethod get-column-count ::table-control
  "Returns the number of columns of the table control.

  Parameters:
    otable-control    _table-control object")

(defmethod-swing 
  get-column-count ::table-control
  [otable-control]
  (.getColumnCount (get-control otable-control)))


(inherit-multimethod set-column-count ::table-control
  "Sets the number of columns of the table control.

  Parameters:
    otable-control    _table-control object
    count             _number of columns")

(defmethod-swing 
  set-column-count ::table-control
  [otable-control count]
  (.setColumnCount (:model otable-control) count))


(inherit-multimethod set-row-count ::table-control
  "Sets the number of rows of the table control.

  Parameters:
    otable-control    _table-control object
    count             _number of rows")

(defmethod-swing 
  set-row-count ::table-control
  [otable-control count]
  (.setRowCount (:model otable-control) count))



(inherit-multimethod register-keyboard-action ::table-control
  "Registers a keyboard action on the table.
  Parameters:
    otable     _table-control object
    handler    _function that will be called when the keystroke is hit,
                it will be given the table-control object as single parameter
    name       _name of the handler (implicit str)
    keystroke  _a corresponding keystroke object
    condition  _can be either :focus, :widget, or :ancestor
                (determines which widget has to be focused to trigger the
                 action)")

(defmethod-swing-threads*
  register-keyboard-action ::table-control
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

(inherit-multimethod get-column-index ::table-control
  "Returns the tables model index of the specified column in the view.

  Parameters:
    otable    _table-control object
    column    _viewport column")

(defmethod-swing
  get-column-index ::table-control
  [otable column]
  (let [ table (get-control otable)
         col-count (.getColumnCount table) ]
    (if (>= column col-count) column
      (let [ col-model (.getColumnModel table)
             table-col (.getColumn col-model column) ]
        (.getModelIndex table-col)))))

(inherit-multimethod get-index-column ::table-control
 "Returns the column in the view that corresponds to the specified
  table model index.

  Parameters:
    otable    _table-control object
    index     _table model index")

(defmethod-swing
  get-index-column ::table-control
  [otable index]
  (let [ table (get-control otable)
         col-model (.getColumnModel table)
         cols (range (.getColumnCount col-model))
         matching (filter (fn [x] (= index (.getModelIndex 
                                             (.getColumn col-model x))))
                    cols)
         found  (first matching)]
    (if found found index)))


(inherit-multimethod get-row-index ::table-control
  "Returns the tables model index of the specified row in view.
  Parameters:
    row      _viewport row")

(defmethod 
  get-row-index ::table-control
  [otable row]
  row)


(inherit-multimethod get-index-row ::table-control
  "Returns the row in the view that corresponds
  to the specified table model index.

  Parameters:
    otable    _table-control object
    index     _table model index")

(defmethod 
  get-index-row ::table-control
  [otable index]
  index)


(inherit-multimethod get-row-index-permutator ::table-control
  "Returns a function that will map the current view rows to
   the according index values.
  
  Parameters:
    otable    _table-control object")

(defmethod
  get-row-index-permutator ::table-control
  [otable] identity)


(inherit-multimethod get-column-index-permutator ::table-control
  "Returns a function that will map the current view
    columns to the according index values.
  
  Parameters:
    otable    _table-control object")

(defmethod-swing
  get-column-index-permutator ::table-control
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


(inherit-multimethod get-value-at-view ::table-control
  "Returns the value of the cell at specified position in view.

   Parameters:
     otable     _table-control object
     row        _row of the cell
     column     _column of the cell")

(defmethod-swing
  get-value-at-view ::table-control
  [otable row column]
  (let [ table (get-control otable) ]
    (.getValueAt table row column)))


(inherit-multimethod get-value-at-index ::table-control
  "Returns the value of the cell at specified position in the table model.

   Parameters:
     otable     _table-control object
     row        _row of the cell
     column     _column of the cell")

(defmethod
  get-value-at-index ::table-control
  [otable row column]
  (let [ irow (get-index-row otable row)
         icolumn (get-index-column otable column)]
    (get-value-at-view otable irow icolumn)))



 (inherit-multimethod set-resize-mode ::table-control
   "Sets the behaviour of the table on resize.
    Parameters:
      otable    _table-control object
      mode      _either :all, :last, :next, :off, or :subseq")

(defmethod-swing-threads*
  set-resize-mode ::table-control
  [otable mode]
  (.setAutoResizeMode (get-control otable)
    ({:all JTable/AUTO_RESIZE_ALL_COLUMNS
      :last JTable/AUTO_RESIZE_LAST_COLUMN
      :next JTable/AUTO_RESIZE_NEXT_COLUMN
      :off JTable/AUTO_RESIZE_OFF
      :subseq JTable/AUTO_RESIZE_SUBSEQUENT_COLUMNS} mode)))


(inherit-multimethod set-cell-selection-mode ::table-control
  "Sets the cell selection mode.
   Parameters:
     otable    _table-control object
     mode      _either :none, :rows, :columns or :cells")

(defmethod-swing-threads*
  set-cell-selection-mode ::table-control
  [otable mode]
  (let [table (get-control otable)]
    (cond (= mode :none) (.setCellSelectionEnabled table false)
      (= mode :rows) (doto table (.setColumnSelectionAllowed false)
                       (.setRowSelectionAllowed true))
      (= mode :columns) (doto table (.setRowSelectionAllowed false)
                          (.setColumnSelectionAllowed true))
      (= mode :cells) (.setCellSelectionEnabled table true))))
  
(inherit-multimethod set-value-at-view ::table-control
  "Sets the value of a cell in the table according to a view position.
   Parameters:
     otable     _table-control object
     row        _integer identifying a row
     column     _integer identifying a column
     contents   _the new contents")

(defmethod-swing
  set-value-at-view ::table-control
  [otable row column contents]
  (let [ columns (get-column-count otable)
         rows  (get-row-count otable) ]
    (if (>= column columns)
      (call-hook otable "extend-columns-to" (+ 1 column)))
    (if (>= row rows)
      (call-hook otable "extend-rows-to" (+ 1 row)))
    (.setValueAt (get-control otable) (str contents) row column)))


(inherit-multimethod set-value-at-index ::table-control
  "Sets the value of a cell in the table according to the model index.
   Parameters:
     otable     _table-control object
     row        _integer identifying a row
     column     _integer identifying a column
     contents   _the new contents")

(defmethod
  set-value-at-index ::table-control
  [otable row column contents]
  (set-value-at-view otable (get-index-row otable row)
    (get-index-column otable column) contents))


(inherit-multimethod update-value-at-index ::table-control
  "Sets the value of a cell in the table according to the model index,
   if it is different from the current cells value.
   Parameters:
     otable     _table-control object
     row        _integer identifying a row
     column     _integer identifying a column
     contents   _the new contents")

(defmethod
  update-value-at-index ::table-control
  [otable row column contents]
  (let [ current (get-value-at-index otable row column) ]
    (if (not= current contents)
      (set-value-at-index otable row column contents))))


(inherit-multimethod paste-from-clipboard ::table-control
  "Pastes the current system clipboard contents into the table.

   Parameters:
     otable    _table-control object")

(defmethod-swing-threads*
  paste-from-clipboard ::table-control
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

(inherit-multimethod copy-to-clipboard ::table-control
  "Copies the selected cells from the table widget to the system
   clipboard.
  
   Parameters:
     otable    _table-control object")

(defmethod-swing-threads* 
  copy-to-clipboard ::table-control
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

;;          }
;;          defaults [ [:set-resize-mode :off] 
;;                     [:set-cell-selection-mode :cells]
;;                     [:register-keyboard-action 
;;                       table-to-clipboard!
;;                       "Copy" keystroke-copy :focus] 
;;                     [:register-keyboard-action 
;;                       clipboard-to-table!
;;                       "Paste" keystroke-paste :focus]
;;                     [:set-handler :set-cell-value
;;                       (fn-swing-threads*-doc "Sets the value of a cell in the table.
;;   Parameters:
;;     widget     _the widget object
;;     row        _integer identifying a row
;;     column     _integer identifying a column
;;     contents   _the new contents"
;;                         [widget row column contents]
;;                         (let [ columns (!! widget :get-column-count)
;;                                rows    (!! widget :get-row-count)]
;;                           (do
;;                             (if (>= column columns)
;;                               (!!!! widget :extend-columns-to (+ 1 column)))
;;                             (if (>= row rows)
;;                               (!!!! widget :extend-rows-to (+ 1 row)))
;;                             (.setValueAt (get-control widget) 
;;                               (str contents) 
;;                               row column))))]
;;                     [:set-handler :set-cell-value-hook
;;                       (fn-doc "This hook takes requested contents and turns them into
;;   allowed contents.

;;   Parameters:
;;     widget     _the widget object
;;     row        _integer identifying a row
;;     column     _integer identifying a column
;;     contents   _the desired new contents

;;   Returns the new allowed contents for the cell (as string)."
;;                         [widget row column contents]
;;                         contents)]
;;                     [:set-handler :extend-columns-to
;;                       (fn-swing-doc "Extends the current table to have (at least) the
;;   specified number of columns.
  
;;   Parameters:
;;     widget     _the widget object
;;     columns    _desired number of columns"
;;                         [widget columns]
;;                         (let [old-columns (!! widget :get-column-count)]
;;                           (if (< old-columns columns)
;;                             (do
;;                               (!! widget :set-column-count columns)
;;                               (!!!! widget :extend-columns-hook old-columns columns)))))]
;;                     [:set-handler :extend-rows-to
;;                       (fn-swing-doc "Extends the current table to have (at least) the
;;   specified number of rows.
  
;;   Parameters:
;;     widget     _the widget object
;;     rows    _desired number of rows"
;;                         [widget rows]
;;                         (let [old-rows (!! widget :get-row-count)]
;;                           (if (< old-rows rows)
;;                             (do
;;                               (!! widget :set-row-count rows)
;;                               (!!!! widget :extend-rows-hook old-rows rows)))))]                    
;;                     [:set-handler :extend-rows-hook 
;;                       (fn-doc "Dummy extend hook" [widget old-rows new-rows] nil)]
;;                     [:set-handler :extend-columns-hook 
;;                       (fn-doc "Dummy extend hook" [widget old-columns new-columns] nil)]
;;                     [:set-handler :table-changed-hook
;;                       (fn-doc "Table changed hook, we only catch single-cell-change-events here.
;;   Parameters:
;;     widget    _the tables widget object
;;     column    _the altered column, -1 means all columns
;;     first-row _the first altered row, -1 means all rows
;;     last-row  _the last altered row, -1 means all rows
;;     type      _0 means update, 1 insert and -1 delete" 
;;                         [widget column first-row last-row type] 
;;                         (if (and
                          
;;                               (= 0 type)
;;                               (<= 0 (min column first-row last-row))
;;                               (= first-row last-row))
;;                            (let [ current-value (!! widget :get-value-at-index first-row column)
;;                                    good-value (!!!! widget :set-cell-value-hook 
;;                                                 first-row column current-value)]
;;                               (if (not= current-value good-value)
;;                                 (!! widget :set-value-at-index first-row column good-value)))))]

;;                     ]]
;;     (do
;;       (deliver self widget)
;;       (let [change-listener (proxy [TableModelListener] [] 
;;                               (tableChanged [event]
;;                                 (with-swing-threads*
;;                                   (let [ column (.getColumn event)
;;                                          first  (.getFirstRow event)
;;                                          last   (.getLastRow event)
;;                                          type   (.getType event)]
;;                                     (!!!! widget :table-changed-hook 
;;                                       column first last type)))))]
;;         (.addTableModelListener model change-listener))
;;       (doseq [x defaults] (unroll-parameters-fn-map widget x))
;;       (doseq [x setup] (unroll-parameters-fn-map widget x))
;;       widget )))

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
         widget (table-control pane table hooks model) 
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
                      paste-from-clipboard "Paste" keystroke-paste :focus]]]
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
    widget ))

;;
;; old
;;


(defn-swing-threads* clipboard-to-table!
  "Takes a table widget as parameter and will paste the current
   clipboard contents in the cell block marked by the current
   selected cell.

  Parameters:
    obj     _a table widget" [obj] 
  (let [control (get-control obj)
         sel-columns ((*comp (rho .getSelectedColumns)  extract-array)
                       control)
         sel-rows ((*comp (rho .getSelectedRows) extract-array)
                    control)
         col-index (!! obj :get-column-index-permutator)
         row-index (!! obj :get-row-index-permutator)]
    (if (or (empty? sel-columns) (empty? sel-rows)) nil
      (let [data (str (get-clipboard-contents))
             startx (first sel-columns)
             starty (first sel-rows)
             cells (vec (map (*comp (rho split #"\t") (rho vec))
                          (split-lines data)))
             lns (range (count cells))]
        (doseq [r lns]  (doseq [c (range (count (cells r)))]
                          (!! obj :set-value-at-index
                            (row-index (+ starty r))
                            (col-index (+ startx c))
                            ((cells r) c))))))))

(defn-swing-threads* table-to-clipboard!
  "Takes a table widget as parameter and copies the current
   selected cells to the clipboard.

  Parameters:
    obj      _a table widget" [obj] 
  (let [control (get-control obj)
         sel-columns ((*comp (rho .getSelectedColumns) extract-array)
                       control)
         sel-rows ((*comp (rho .getSelectedRows) extract-array)
                    control)
         sel-pairs (map (fn [y] (map (fn [x] (list y x)) sel-columns))
                     sel-rows)
         sel-values (map (fn [l] (map (fn [p] (str (!! obj :get-value-at-view 
                                                     (first p) 
                                                     (second p)))) l))
                      sel-pairs)
         sel-lines (map (rho join "\t") sel-values)
         selection (join "\n" sel-lines)]
    (set-clipboard-contents selection)))

(defn-swing do-mk-table-control
  "Creates a table control in Java.

  Parameters:
     & setup     _an optional number of vectors that may contain additional
                  tweaks that are called after widget creation"
  [& setup]
  (let [ self  (promise)
         model (DefaultTableModel.)
         table (JTable. model)
         keystroke-copy (KeyStroke/getKeyStroke KeyEvent/VK_C
                          ActionEvent/CTRL_MASK false)
         keystroke-paste (KeyStroke/getKeyStroke KeyEvent/VK_V
                           ActionEvent/CTRL_MASK false)
         pane  (JScrollPane. table 
                 JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED 
                 JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED)
         widget {:managed-by-conexp-gui-editors-util "table-control"

         :widget   pane
         :control  table
         :model    model
         :handler  (ref {})

         :get-row-count
         (fn-swing-doc "Gets the row count of the table"
           [] (.getRowCount table))

         :get-column-count
         (fn-swing-doc "Gets the column count of the table"
           [] (.getColumnCount table))

         :set-handler
         (fn-doc "Set the widgets handler function.

  Parameters:
    key        _a key that identifies the handler
    handler    _the new handler function"
           [key handler]
           (let [ widget @self
                  handler-map (widget :handler) ]
             (dosync (commute handler-map conj {key handler}))))

         :set-handler-and-wait
         (fn-doc "Set the widgets handler function,
  and waits for the completion of the setup.

  Parameters:
    key        _a key that identifies the handler
    handler    _the new handler function"
           [key handler]
           (let [ widget @self
                  handler-map (widget :handler)
                  dowaitfor (promise)]
             (dosync (commute handler-map conj {key handler})
               (deliver dowaitfor nil))
             @dowaitfor))
  
         :register-keyboard-action
         (fn-swing-threads*-doc "Registers a keyboard action on the table.
  Parameters:
    handler    _function that will be called when the keystroke is hit,
                it will be given the widget as single parameter
    name       _name of the handler (implicit str)
    keystroke  _a corresponding keystroke object
    condition  _can be either :focus, :widget, or :ancestor
                (determines which widget has to be focused to trigger the
                 action)"
           [handler name keystroke condition]
           (let [ java-condition ({:focus JComponent/WHEN_FOCUSED
                                   :widget JComponent/WHEN_IN_FOCUSED_WINDOW
                                   :ancestor JComponent/WHEN_ANCESTOR_OF_FOCUSED_COMPONENT
                                   } condition)
                  widget @self
                  action (proxy [AbstractAction ActionListener] []
                           (actionPerformed [event] 
                             (handler widget)))
                  input-map (.getInputMap table java-condition)
                  action-map (.getActionMap table)
                  cmd-name (str name)]             
             (do
               (.put input-map keystroke cmd-name)
               (.put action-map cmd-name action))))

         :set-column-count 
         (fn-swing-threads*-doc "Sets the number of columns of the table.
   Parameters:
     count     _number of columns in the altered table"
           [count]

           (.setColumnCount model count))

         :set-row-count 
         (fn-swing-threads*-doc "Sets the number of rows of the table.
   Parameters:
     count     _number of rows in the altered table"
           [count]
           (.setRowCount model count))

         :get-value-at-index
         (fn-doc "Gets the value of the cell at specified model index.

  Parameters:
    row        _row of the cell
    column     _column of the cell
  Returns the cell value"
           [row column]
           (let [widget @self]
             (!! widget :get-value-at-view (!! widget :get-index-row row)
               (!! widget :get-index-column column))))


         :get-value-at-view
         (fn-swing-doc "Gets the value of the cell at specified position.

  Parameters:
    row        _row of the cell
    column     _column of the cell
  Returns the cell value"
           [row column]
           (.getValueAt table row column))

         :get-row-index
         (fn-doc "Returns the tables model index of the specified row in view.
  Parameters:
    row      _viewport row
  Returns the model row-index"
           [row] row)

         :get-row-index-permutator
         (fn-doc "Returns a function that will map the current view rows to
   the according index values."
           [] identity)

         :get-column-index-permutator
         (fn-swing-doc "Returns a function that will map the current view
   columns to the according index values."
           [] 
           (let [ col-count (.getColumnCount table)
                  col-range (range col-count)
                  col-model (.getColumnModel table)
                  col-map  (zipmap col-range (map (*comp
                                                    (rho .getColumn col-model)
                                                    (rho .getModelIndex))
                                               col-range)) ]
             (fn [i] (if (>= i col-count) i (col-map i)))))

         :get-column-index
         (fn-swing-doc "Returns the tables model index of the specified
   column in the view.

  Parameters:
    column    _viewport column
  Returns the model index"
           [column]
           (let [ col-count (.getColumnCount table) ]
             (if (>= column col-count) column
               (let [ col-model (.getColumnModel table)
                      table-col (.getColumn col-model column)]
                 (.getModelIndex table-col)))))
         
         :get-index-column
         (fn-swing-doc "Returns the column in the view that corresponds
  to the specified table model index.

  Parameters:
    index     _table model index"
           [index]
           (let [ col-model (.getColumnModel table)
                  cols (range (.getColumnCount col-model))
                  matching (filter 
                             (fn [x] (= index (.getModelIndex (.getColumn col-model x))))
                             cols)
                  found  (first matching)]
             (if found found index)))

         :get-index-row
         (fn-doc "Returns the row in the view that corresponds
  to the specified table model index.

  Parameters:
    index     _table model index"
           [index]
           index)


         :set-resize-mode
         (fn-swing-threads*-doc "Sets the behaviour of the table on resize.
   Parameters:
     mode      _either :all, :last, :next, :off, or :subseq"
           [mode]

           (.setAutoResizeMode table ({:all JTable/AUTO_RESIZE_ALL_COLUMNS
                                       :last JTable/AUTO_RESIZE_LAST_COLUMN
                                       :next JTable/AUTO_RESIZE_NEXT_COLUMN
                                       :off JTable/AUTO_RESIZE_OFF
                                       :subseq JTable/AUTO_RESIZE_SUBSEQUENT_COLUMNS} mode)))

         :set-cell-selection-mode
         (fn-swing-threads*-doc "Sets the cell selection mode.
   Parameters:
     mode      _either :none, :rows, :columns or :cells"
           [mode]

           (cond (= mode :none) (.setCellSelectionEnabled table false)
             (= mode :rows) (doto table (.setColumnSelectionAllowed false)
                              (.setRowSelectionAllowed true))
             (= mode :columns) (doto table (.setRowSelectionAllowed false)
                                 (.setColumnSelectionAllowed true))
             (= mode :cells) (.setCellSelectionEnabled table true)))

         :set-value-at-index
         (fn-doc "Sets the value of a model-indexed cell in the table.
  Parameters:
    row        _integer identifying a row
    column     _integer identifying a column
    contents   _the new contents"
           [row column contents]
           (let [widget @self]
             (!! widget :set-value-at-view (!! widget :get-index-row row)
               (!! widget :get-index-column column) contents)))

         :update-value-at-index
         (fn-doc "Sets the value of a model-indexed cell in the table, if it is
  different from the current cells value.
  Parameters:
    row        _integer identifying a row
    column     _integer identifying a column
    contents   _the new contents"
           [row column contents]
           (let [widget @self
                 r (!! widget :get-index-row row)
                 c (!! widget :get-index-column column)
                 current (!! widget :get-value-at-view r c)]
             (if (not= current contents)
               (!! widget :set-value-at-view r c contents))))


         :set-value-at-view
         (fn-doc "Sets the value of a cell in the table.
  Parameters:
    row        _integer identifying a row
    column     _integer identifying a column
    contents   _the new contents"
           [row column contents]
           (!!!! @self :set-cell-value row column contents))
         }
         defaults [ [:set-resize-mode :off] 
                    [:set-cell-selection-mode :cells]
                    [:register-keyboard-action 
                      table-to-clipboard!
                      "Copy" keystroke-copy :focus] 
                    [:register-keyboard-action 
                      clipboard-to-table!
                      "Paste" keystroke-paste :focus]
                    [:set-handler :set-cell-value
                      (fn-swing-threads*-doc "Sets the value of a cell in the table.
  Parameters:
    widget     _the widget object
    row        _integer identifying a row
    column     _integer identifying a column
    contents   _the new contents"
                        [widget row column contents]
                        (let [ columns (!! widget :get-column-count)
                               rows    (!! widget :get-row-count)]
                          (do
                            (if (>= column columns)
                              (!!!! widget :extend-columns-to (+ 1 column)))
                            (if (>= row rows)
                              (!!!! widget :extend-rows-to (+ 1 row)))
                            (.setValueAt (get-control widget) 
                              (str contents) 
                              row column))))]
                    [:set-handler :set-cell-value-hook
                      (fn-doc "This hook takes requested contents and turns them into
  allowed contents.

  Parameters:
    widget     _the widget object
    row        _integer identifying a row
    column     _integer identifying a column
    contents   _the desired new contents

  Returns the new allowed contents for the cell (as string)."
                        [widget row column contents]
                        contents)]
                    [:set-handler :extend-columns-to
                      (fn-swing-doc "Extends the current table to have (at least) the
  specified number of columns.
  
  Parameters:
    widget     _the widget object
    columns    _desired number of columns"
                        [widget columns]
                        (let [old-columns (!! widget :get-column-count)]
                          (if (< old-columns columns)
                            (do
                              (!! widget :set-column-count columns)
                              (!!!! widget :extend-columns-hook old-columns columns)))))]
                    [:set-handler :extend-rows-to
                      (fn-swing-doc "Extends the current table to have (at least) the
  specified number of rows.
  
  Parameters:
    widget     _the widget object
    rows    _desired number of rows"
                        [widget rows]
                        (let [old-rows (!! widget :get-row-count)]
                          (if (< old-rows rows)
                            (do
                              (!! widget :set-row-count rows)
                              (!!!! widget :extend-rows-hook old-rows rows)))))]                    
                    [:set-handler :extend-rows-hook 
                      (fn-doc "Dummy extend hook" [widget old-rows new-rows] nil)]
                    [:set-handler :extend-columns-hook 
                      (fn-doc "Dummy extend hook" [widget old-columns new-columns] nil)]
                    [:set-handler :table-changed-hook
                      (fn-doc "Table changed hook, we only catch single-cell-change-events here.
  Parameters:
    widget    _the tables widget object
    column    _the altered column, -1 means all columns
    first-row _the first altered row, -1 means all rows
    last-row  _the last altered row, -1 means all rows
    type      _0 means update, 1 insert and -1 delete" 
                        [widget column first-row last-row type] 
                        (if (and
                          
                              (= 0 type)
                              (<= 0 (min column first-row last-row))
                              (= first-row last-row))
                           (let [ current-value (!! widget :get-value-at-index first-row column)
                                   good-value (!!!! widget :set-cell-value-hook 
                                                first-row column current-value)]
                              (if (not= current-value good-value)
                                (!! widget :set-value-at-index first-row column good-value)))))]

                    ]]
    (do
      (deliver self widget)
      (let [change-listener (proxy [TableModelListener] [] 
                              (tableChanged [event]
                                (with-swing-threads*
                                  (let [ column (.getColumn event)
                                         first  (.getFirstRow event)
                                         last   (.getLastRow event)
                                         type   (.getType event)]
                                    (!!!! widget :table-changed-hook 
                                      column first last type)))))]
        (.addTableModelListener model change-listener))
      (doseq [x defaults] (unroll-parameters-fn-map widget x))
      (doseq [x setup] (unroll-parameters-fn-map widget x))
      widget )))

;;
;;
;;  Toolbar
;;
;;
;;

(defn-swing do-mk-toolbar-control
  "Creates a toolbar control in Java.

  Parameters:
     orientation _either :horiz or :vert
     & setup     _an optional number of vectors that may contain additional
                  tweaks that are called after widget creation"
  [orientation & setup]
  (let [ self (promise)
         toolbar (JToolBar. ({:horiz JToolBar/HORIZONTAL
                                      :vert JToolBar/VERTICAL} 
                                      orientation))
         widget {:managed-by-conexp-gui-editors-util "toolbar-control"

         :widget toolbar
         :control toolbar

         :set-orientation 
         (fn-swing-threads*-doc "Sets the toolbars orientation.

  Parameters:
    orientation   _either :horiz or :vert" 
           [orientation]
           (.setOrientation toolbar ({:horiz JToolBar/HORIZONTAL
                                      :vert JToolBar/VERTICAL} 
                                      orientation)))

         :add-button
         (fn-swing-threads*-doc "Adds a button to the toolbar

  Parameters:
    button    _the button widget"
           [button]
           (.add toolbar (get-widget button)))

         :add-separator
         (fn-swing-threads*-doc "Adds a separator to the toolbar."
           []
           (.addSeparator toolbar))
         
         }

         defaults []]
    (do
      (deliver self widget)
      (doseq [x defaults] (unroll-parameters-fn-map widget x))
      (doseq [x setup] (unroll-parameters-fn-map widget x))
      widget )))

nil

