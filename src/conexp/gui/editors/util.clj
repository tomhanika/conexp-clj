;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.editors.util
  (:import [javax.swing JSplitPane JRootPane JTextArea JTable JList JTree
             JScrollPane JOptionPane]
    [javax.swing.tree DefaultTreeModel DefaultMutableTreeNode 
      TreeSelectionModel]
    [java.util Vector]
    [javax.swing.event TreeSelectionListener]
    [javax.swing.table DefaultTableModel])
  (:use clojure.contrib.swing-utils
    conexp.gui.util))

;;;
;;; General purpose & macros
;;;

(defmacro one-by-one
  "Calls a function on each element of a collection.
   Parameters:
   coll    _collection of arguments
   f       _function body that takes one parameter
   " 
  [coll & f]
  `(loop [args# ~coll]
     (if (empty? args#) nil (do (~@f (first args#))
                              (recur (rest args#))))))

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
         (one-by-one childs# (lambda (x#) (~do-add root# (mapper# x#))))
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
     (def object-to-be-documented ~v)
     (doc* object-to-be-documented)))

(defmacro fn-doc
  "Create a documented anonymous function. (Currently does nothing.)"
  [doc & rest]
  `(with-meta (fn ~@rest) {:doc ~doc :alternate-doc 0}))


(defn extract-array
  "Takes a java-array and turns it into a list"
  [array]
  (map (rho aget array) (range (alength array))))

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

;;
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

;;
;;
;;  Split Pane
;;
;;
;;
;;

(defn do-mk-split-pane
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
             (with-swing-threads 
               (.setDividerLocation split-pane location))) 
           }
           ]
      (do
        (one-by-one setup unroll-parameters-fn-map widget) 
        widget )))

;;
;;
;; Tree Control
;;
;;
;;

(defn do-mk-tree-control
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
             (with-swing-threads
               (.removeAllChildren treeroot)
               (do-map-tree* (fn [x] (if (= :root x) treeroot 
                                       (DefaultMutableTreeNode. x)))
                 .add t)
               (.reload treemodel)) ))

         :set-selection-mode
         (fn-doc "Sets the tree control's selection mode.

   Parameters:
     mode       _either :single, :multi (contiguous multiselection) or
                 :free (free multiselection)"
           [mode]
           (let [selection-model (.getSelectionModel treecontrol)]
             (.setSelectionMode selection-model 
               ({:single TreeSelectionModel/SINGLE_TREE_SELECTION
                 :multi TreeSelectionModel/CONTIGUOUS_TREE_SELECTION
                 :free TreeSelectionModel/DISCONTIGUOUS_TREE_SELECTION} mode))))

         :set-selection-handler
         (fn-doc "Sets the selection-handler for the tree-control which is
   called every time the selection changes to handler.

   Parameters:
     handler    _function that will be called with a list of the selected nodes
                 represented each by a list of labels starting from the root."
           [handler]
           (with-swing-threads
             (let [current-handlers (.getTreeSelectionListeners treecontrol)
                    listeners (extract-array current-handlers) ]
               (one-by-one listeners .removeTreeSelectionListener treecontrol))
             (let [listener (proxy [TreeSelectionListener] []
                              (valueChanged [event] 
                                (let [ paths (.getSelectionPaths treecontrol)
                                       treepaths (map (*comp
                                                        (rho .getPath)
                                                        extract-array
                                                        (rho map 
                                                          (rho .getUserObject))
                                                        )
                                                   (extract-array paths))
                                       ]
                                  (handler treepaths) )))]
               (.addTreeSelectionListener treecontrol listener) ) )) }]
    (do
      (one-by-one setup unroll-parameters-fn-map widget) 
      widget )))

;;
;;
;;  Table
;;
;;
;;

(defn do-mk-table-control
  "Creates a table control in Java.

  Parameters:
     & setup     _an optional number of vectors that may contain additional
                  tweaks that are called after widget creation"
  [& setup]
  (let [ model (DefaultTableModel.)
         table (JTable. model)
         pane  (JScrollPane. table 
                 JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED 
                 JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED)
         widget {:managed-by-conexp-gui-editors-util "table-control"
         :widget   pane
         :control  table
         :model    model

         :set-column-count 
         (fn-doc "Sets the number of columns of the table.
   Parameters:
     count     _number of columns in the altered table"
           [count]
           (with-swing-threads
             (.setColumnCount model count)))

         :set-row-count 
         (fn-doc "Sets the number of rows of the table.
   Parameters:
     count     _number of rows in the altered table"
           [count]
           (with-swing-threads
             (.setRowCount model count)))

         :set-resize-mode
         (fn-doc "Sets the behaviour of the table on resize.
   Parameters:
     mode      _either :all, :last, :next, :off, or :subseq"
           [mode]
           (with-swing-threads
             (.setAutoResizeMode table ({:all JTable/AUTO_RESIZE_ALL_COLUMNS
               :last JTable/AUTO_RESIZE_LAST_COLUMN
               :next JTable/AUTO_RESIZE_NEXT_COLUMN
               :off JTable/AUTO_RESIZE_OFF
               :subseq JTable/AUTO_RESIZE_SUBSEQUENT_COLUMNS} mode))))

         :set-cell-selection-mode
         (fn-doc "Sets the cell selection mode.
   Parameters:
     mode      _either :none, :rows, :columns or :cells"
           [mode]
           (with-swing-threads
             (cond (= mode :none) (.setCellSelectionEnabled table false)
               (= mode :rows) (doto table (.setColumnSelectionAllowed false)
                                (.setRowSelectionAllowed true))
               (= mode :columns) (doto table (.setRowSelectionAllowed false)
                                (.setColumnSelectionAllowed true))
               (= mode :cells) (.setCellSelectionEnabled table true))))
         }
         defaults [ [:set-resize-mode :off] 
                    [:set-cell-selection-mode :cells]] ]
    (do
      (one-by-one defaults unroll-parameters-fn-map widget)
      (one-by-one setup unroll-parameters-fn-map widget)
      widget )))

nil

