;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.editors.contexts
    (:import [javax.swing JSplitPane JRootPane JTextArea JTable JList JTree
              JScrollPane]
             [javax.swing.tree DefaultTreeModel DefaultMutableTreeNode]
             [java.util Vector]
             [javax.swing.table DefaultTableModel]
             )
    (:use conexp.gui.plugins.base
          conexp.gui.util
          clojure.contrib.swing-utils
          conexp.gui.editors.util
          ))

(def context-data (ref {}))
(def context-pane (ref nil))
(def context-workspace (ref nil))
(def context-attributes (ref nil))
(def context-table (ref nil))
(def context-tablemodel (ref nil))
(def context-workspace-tree (ref nil))

(defn update-workspace-tree
  "Updates the data displayed in the current workspace tree
   in order to reflect the current @context-data map-var."
  []
  (let [nodes ((*comp 
                keys                    ; take the keys
                (rho map str)           ; turn them into strings
                sort                    ; sort them
                (rho map list))         ; and make them leaf nodes
               @context-data)
        workspace-tree (conj nodes :root)
        ]
    (with-swing-threads
     (.removeAllChildren @context-workspace-tree)
     (do-map-tree* (fn [x] (if (= :root x) 
                             @context-workspace-tree 
                             (DefaultMutableTreeNode. x)))
                   .add
                   workspace-tree)
     (.reload @context-workspace))))

(defn add-to-workspace
  "Adds a named context to the current workspace or replaces 
    it if it exists
    Parameters:
       name     _the contexts name (will be converted to string)
       context  _the context data structure
   "
  [name context]
  (dosync (commute context-data conj {(str name) context}))
  (update-workspace-tree)
  )


(defn plug-load-hook
  "Loads the context-editor plugin.
   Parameters:
     frame    _frame that shall contain the user interface
  "
  [frame]
  (do
    (dosync (ref-set context-data {}))
    (with-swing-threads
      
     (let [ treeroot (DefaultMutableTreeNode. "context workspace")
            treemodel (DefaultTreeModel. treeroot)
            tree (JTree. treemodel)
            topleft (JScrollPane. tree JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED 
                                  JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED )   
            bottomleft (JTextArea. "bottomleft")
            left (JSplitPane. JSplitPane/VERTICAL_SPLIT topleft bottomleft)
            tablemodel (DefaultTableModel. (Vector. (list "[objects]" "wee")) 20 )
            table (JTable. tablemodel)
            right (JScrollPane. table JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED 
                                JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED )             pane (JSplitPane. JSplitPane/HORIZONTAL_SPLIT left right )]
       (do
         (doto table
           (.setColumnSelectionAllowed true)
           (.setAutoResizeMode JTable/AUTO_RESIZE_OFF)
           (.setMinimumSize (java.awt.Dimension. 35 35))
           )
         (.setDividerLocation left 250)
         (.setDividerLocation pane 200)
         (add-tab-with-name-icon-tooltip frame pane 
                                         "Contexts" nil "View and edit contexts")
         (dosync (ref-set context-pane pane)
                 (ref-set context-workspace treemodel)
                 (ref-set context-attributes bottomleft)
                 (ref-set context-table table)
                 (ref-set context-tablemodel tablemodel)
                 (ref-set context-workspace-tree treeroot)     
                 )
         )))
    )
  )

(defn plug-unload-hook
  "Unloads the context-editor plugin.
   Parameters:
     frame    _frame that contains the user interface
  "
  [frame]
  (with-swing-threads
   (remove-tab frame @context-pane)
   (dosync 
    (ref-set context-workspace nil)
    (ref-set context-attributes nil)
    (ref-set context-table nil)
    (ref-set context-pane nil)
    (ref-set context-data nil)
    )
   ))
   

(define-plugin context-editor
  "Context editor plugin."
  :load-hook plug-load-hook,
  :unload-hook plug-unload-hook)

nil
