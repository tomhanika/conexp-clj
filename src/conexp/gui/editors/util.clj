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
    [clojure.contrib.string :only (join split-lines split)]))

;;;
;;; General purpose & macros
;;;


(defmacro lambda
  "Takes the free argument as first parameter and the function value as
    following parameters as function body, i.e.


     ( lambda (x y) (do (print x) (* x y)) ) 

    will produce a function that takes 2 parameters, prints the first one
    and returns the product of them.
   " 
  [parameter & body]
  `(fn [~@parameter] ~@body ))

(defmacro rho
  "Takes an block that defines a function by omitting its last parameter
    and turns it into a normal function that takes one parameter"
  [ & body ]
  `(fn [x#] (~@body x#)))

(defmacro *comp
  "Takes some functions and returns the covariant function composition
    of them"
  [& fns]
  `(comp ~@(reverse fns)))

(defmacro dosync-wait
  "Returns a dosync block that will block until the operation
   has been carried out, the last value of the body will
   be returned."
  [ & body]
  `(let [waiting# (promise)]
     (do
       (dosync (deliver waiting# (do ~@body)))
       (deref waiting#))))

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
       (do
         (doseq [x# childs#] (~do-add root# (mapper# x#)))
         root#))))

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

(defmacro unroll-parameters
  "Takes a vector that has a function as first argument and does a function
   call giving all other vector elements as parameters."
  [call_vec]
  `(let [ vec# ~call_vec
          count# (count vec#)
          ]
     (cond 
       (= count# 0) nil
       (= count# 1) ((vec# 0))
       (= count# 2) ((vec# 0) (vec# 1))
       (= count# 3) ((vec# 0) (vec# 1) (vec# 2))
       (= count# 4) ((vec# 0) (vec# 1) (vec# 2) (vec# 3))
       (= count# 5) ((vec# 0) (vec# 1) (vec# 2) (vec# 3) (vec# 4))
       (= count# 6) ((vec# 0) (vec# 1) (vec# 2) (vec# 3) (vec# 4) (vec# 5)))))

(defmacro unroll-parameters-fn-map
  "Takes a vector that has a function as first argument and does a function
   call giving all other vector elements as parameters, looking up the function
   in the function map"
  [fn_map call_vec]
  `(let [ vec# ~call_vec
          count# (count vec#)
          ]
     (cond 
       (= count# 0) nil
       (= count# 1) ((~fn_map (vec# 0)))
       (= count# 2) ((~fn_map (vec# 0)) (vec# 1))
       (= count# 3) ((~fn_map (vec# 0)) (vec# 1) (vec# 2))
       (= count# 4) ((~fn_map (vec# 0)) (vec# 1) (vec# 2) (vec# 3))
       (= count# 5) ((~fn_map (vec# 0)) (vec# 1) (vec# 2) (vec# 3) (vec# 4))
       (= count# 6) ((~fn_map (vec# 0)) (vec# 1) (vec# 2) (vec# 3) (vec# 4)
                      (vec# 5)))))

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

(defmacro map!
  "Just like map, but not lazy."
  [f & colls]
   `(seq (map ~f ~@colls)))


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


(defn managed-by-conexp-gui-editors-util? 
  "Returns true if the object given as parameter is managed by the
   conexp.gui.editors.util module.
   
   Parameters:
     thing       _object to check whether it is managed."
  [thing]
  (and (map? thing)
    (contains? thing :managed-by-conexp-gui-editors-util)))

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
  (if (managed-by-conexp-gui-editors-util? obj) (:control obj) obj))


;;
;;
;; Button
;;
;;
;;
;;
(defn-swing do-mk-button
  "Creates a managed button object.

  Parameters:
    name     _Buttons name
    icon     _Buttons icon
    & setup  _an optional number of vectors that may contain additional
                  tweaks that are called after widget creation"
  [name icon & setup]
  (let [ button (JButton. name icon)
         widget { :managed-by-conexp-gui-editors-util "button"
         :widget button
         
         :set-handler
         (fn-swing-threads*-doc "Sets the action handler for the button object.
  Parameters:
    handler    _0-ary function that will be called on button press"
           [handler]
           (let [current-handlers (.getActionListeners button)
                  listeners (extract-array current-handlers) ]
             (doseq [l listeners] (.removeActionListener button l)))
           (let [action (proxy [ActionListener] []
                           (actionPerformed [event] 
                             (handler)))]
             (.addActionListener button action)))


         }]
  (do
      (doseq [x setup] (unroll-parameters-fn-map widget x)) 
      widget )))

   

;;
;;
;;  Split Pane
;;
;;
;;
;;

(defn-swing do-mk-split-pane
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
  (let [split-pane (JSplitPane. (direction {:horiz JSplitPane/HORIZONTAL_SPLIT
                                  :vert JSplitPane/VERTICAL_SPLIT})
                     (get-widget topleft)
                     (get-widget bottomright))
         widget  {:managed-by-conexp-gui-editors-util "split-pane"
         :widget split-pane
         :set-divider-location 
         (fn-doc "Sets the location of the divider.
   Parameters:
     location   _location of the divider (nbr of pixels from left/top)"
           [location]
           (with-swing-threads* 
             (.setDividerLocation split-pane location))) 
         }
         ]
    (do
      (doseq [x setup] (unroll-parameters-fn-map widget x))
      widget )))

;;
;;
;; Tree Control
;;
;;
;;

(defn-swing do-mk-tree-control
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
         widget   {:managed-by-conexp-gui-editors-util "tree-control"
         :widget  pane
         :root    treeroot
         :model   treemodel
         :control treecontrol

         :set-tree
         (fn-doc "Sets the tree that the control displays.

   Parameters:
     tree       _a tree represented by the root node and its children,
                 where each node is represented by a list that has
                 the node's name as first element and all its child
                 trees as rest, where each child is again represented by
                 its respective root node"
           [tree]
           (let [t (conj (rest tree) :root)]
             (with-swing-threads*
               (.removeAllChildren treeroot)
               (do-map-tree* (fn [x] (if (= :root x) treeroot 
                                       (DefaultMutableTreeNode. x)))
                 .add t)
               (.reload treemodel)) ))

         :set-selection-mode
         (fn-swing-threads*-doc
           "Sets the tree control's selection mode.

   Parameters:
     mode       _either :single, :multi (contiguous multiselection) or
                 :free (free multiselection)"
           [mode]           
           (let [selection-model (.getSelectionModel treecontrol)]
             (.setSelectionMode selection-model 
               ({:single TreeSelectionModel/SINGLE_TREE_SELECTION
                 :multi TreeSelectionModel/CONTIGUOUS_TREE_SELECTION
                 :free TreeSelectionModel/DISCONTIGUOUS_TREE_SELECTION} 
                 mode))))

         :set-selection-handler
         (fn-swing-threads*-doc
           "Sets the selection-handler for the tree-control which is
   called every time the selection changes to handler.

   Parameters:
     handler    _function that will be called with a list of the selected nodes
                 represented each by a list of labels starting from the root."
           [handler]
           
           (let [current-handlers (.getTreeSelectionListeners treecontrol)
                  listeners (extract-array current-handlers) ]
             (doseq [l listeners] (.removeTreeSelectionListener treecontrol l)))
           (let [listener (proxy [TreeSelectionListener] []
                            (valueChanged [event] 
                              (with-swing-threads*
                                (let [ paths (.getSelectionPaths treecontrol)
                                       treepaths (map (*comp
                                                        (rho .getPath)
                                                        extract-array
                                                        (rho map 
                                                          (rho .getUserObject))
                                                        )
                                                   (extract-array paths))
                                       ]
                                  (handler treepaths) ))))]
             (.addTreeSelectionListener treecontrol listener) ) ) }]
    (do
      (doseq [x setup] (unroll-parameters-fn-map widget x)) 
      widget )))

;;
;;
;;  Table
;;
;;
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
                          (!! obj :set-index-at
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
         sel-values (map (fn [l] (map (fn [p] (str (!! obj :get-value-at 
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

         :get-index-at
         (fn-doc "Gets the value of the cell at specified model index.

  Parameters:
    row        _row of the cell
    column     _column of the cell
  Returns the cell value"
           [row column]
           (let [widget @self]
             (!! widget :get-value-at (!! widget :get-index-row row)
               (!! widget :get-index-column column))))


         :get-value-at
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

         :set-index-at
         (fn-doc "Sets the value of a model-indexed cell in the table.
  Parameters:
    row        _integer identifying a row
    column     _integer identifying a column
    contents   _the new contents"
           [row column contents]
           (let [widget @self]
             (!! widget :set-value-at (!! widget :get-index-row row)
               (!! widget :get-index-column column) contents)))

         :update-index-at
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
                 current (!! widget :get-value-at r c)]
             (if (not= current contents)
               (!! widget :set-value-at r c contents))))


         :set-value-at
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
                           (let [ current-value (!! widget :get-index-at first-row column)
                                   good-value (!!!! widget :set-cell-value-hook 
                                                first-row column current-value)]
                              (if (not= current-value good-value)
                                (!! widget :set-index-at first-row column good-value)))))]

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

