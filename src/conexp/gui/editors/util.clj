;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.editors.util
  (:import [javax.swing JSplitPane JRootPane JTextArea JTable JList JTree
             JScrollPane]
    [javax.swing.tree DefaultTreeModel DefaultMutableTreeNode]
    [java.util Vector]
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


;;;
;;; java & swing utils
;;;

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

(defn do-mk-split-pane
  "Creates a managed split pane object.

   Parameters:
     direction   _may be eighter :horiz or :vert
     topleft     _top or left widget
     bottomright _bottom or right widget
     &setup      _an optional number of vectors that may contain additional
                  tweaks that are called after widget creation, i.e.
                  [:set-divider-location 150] will call the
                  :set-divider-location map with 150 as single parameter
  "
  ([direction topleft bottomright]
    (do-mk-split-pane direction topleft bottomright [])
    )
  ([direction topleft bottomright & setup]
    (let [split-pane (JSplitPane. (direction {:horiz JSplitPane/HORIZONTAL_SPLIT
                                    :vert JSplitPane/VERTICAL_SPLIT})
                       (get-widget topleft)
                       (get-widget bottomright))
           widget  {:managed-by-conexp-gui-editors-util "split-pane"
           :widget split-pane
           :set-divider-location 
           (fn [location]
             (with-swing-threads 
               (.setDividerLocation split-pane location))) 
           }
           ]
      (do
        (one-by-one setup unroll-parameters-fn-map widget) widget ))))

nil

