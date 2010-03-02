;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.editors.contexts
  (:import [javax.swing JSplitPane JRootPane JTextArea JTable JList JTree
             JScrollPane JPanel JButton]
    [java.awt GridLayout BorderLayout Dimension]
    [javax.swing.tree DefaultTreeModel DefaultMutableTreeNode]
    [java.util Vector]
    [javax.swing.table DefaultTableModel]
    )
  (:use conexp.gui.plugins.base
    conexp.gui.util
    clojure.contrib.swing-utils
    conexp.gui.editors.util
    conexp.fca
    ))

;;
;;
;; Context editor control
;;
;;
;;
;;

(defn-swing do-mk-context-editor
  "Creates a control for editing contexts.

  Parameters:
     & setup     _an optional number of vectors that may contain additional
                  tweaks that are called after widget creation"
  [& setup]
  (let [ self (promise)
         root (JRootPane.)
         table (do-mk-table-control
                 [:set-row-count 10]
                 [:set-column-count 10])
         toolbar (do-mk-toolbar-control :vert
                   [:add-button 
                     (do-mk-button "Copy" 
                       (get-ui-icon "OptionPane.informationIcon")
                       [:set-handler (fn [] (table-to-clipboard! table))])]
                   [:add-button 
                     (do-mk-button "Paste" 
                       (get-ui-icon "OptionPane.informationIcon")
                       [:set-handler (fn [] (clipboard-to-table! table))])]
                   ;[:add-button (JButton. "copy")])
                   )
         widget {:managed-by-conexp-gui-editors-util "context-editor"

         :widget root
         :table table
         :control (get-control table)

         }

         defaults []]
    (do
      (deliver self widget)
      (.. root getContentPane 
        (add (get-widget toolbar) BorderLayout/LINE_START))
      (.. root getContentPane
        (add (get-widget table)))
      (one-by-one defaults unroll-parameters-fn-map widget)
      (one-by-one setup unroll-parameters-fn-map widget)
      widget )))


;;
;;
;; Editable contexts
;;
;;
;;

(defn editable-context?
  "Tests whether the argument is an editable context."
  [ctx?]
  (and (map? ctx?)
    (contains? ctx? :editable-context)))

(defn get-context
  "Tests whether ctx is an editable context and returns
  the appropriate context value, or otherwise ctx itself."
  [ctx]
  (if (editable-context? ctx)
    @(ctx :context)
    ctx))

(defn make-editable-context
  "Takes an optional context as input and returns the appropriate
   editable context structure.

  Parameters:
    ctx     _context or editable context"
  ([ctx]
    (let [editable-ctx 
           {:editable-context 1
           :context (ref (get-context ctx))
           :associated-widgets (ref [])}]
      editable-ctx))
  ([]
    (make-editable-context (make-context [] [] []))))

(def ctx (conexp.fca/make-context ["a" "b" "c"] [1 2 3] [["a" 1] ["c" 3]]))

(def context-pane (ref nil))
(def context-workspace (ref nil))
(def context-workspace-tree (ref nil))
(def +debug+ (ref nil))

(defn update-workspace-tree
  "Updates the data displayed in the current workspace tree
   in order to reflect the current @context-workspace map-var."
  []
  (with-swing-threads*
    (let [nodes ((*comp 
                   keys                 ; take the keys
                   (rho map str)        ; turn them into strings
                   sort                 ; sort them
                   (rho map list))      ; and make them leaf nodes
                  @context-workspace)
           workspace-tree (conj nodes :root)
           ]
      ((@context-workspace-tree :set-tree) workspace-tree))))

(defn add-to-workspace
  "Adds a named context to the current workspace or replaces 
    it if it exists
    Parameters:
       name     _the contexts name (will be converted to string)
       context  _the context data structure
   "
  [name context]
  (do
    (let [done (promise)]
      (dosync 
        (commute context-workspace conj {(str name) context})
        (deliver done nil))
      (deref done)
      (update-workspace-tree)
      (update-workspace-tree)           ; updates do not show when called only
                                        ; once and something in the control
                                        ; was already selected, WHY?
      )))
  


(defn plug-load-hook
  "Loads the context-editor plugin.
   Parameters:
     frame    _frame that shall contain the user interface
  "
  [frame]
  (do
    (dosync (ref-set context-workspace {}))

    (with-swing-threads
      
      (let [ workspace-tree (do-mk-tree-control "context workspace"
                              [:set-selection-mode :single]
                              [:set-selection-handler 
                                (fn [x] (with-swing-threads
                                          (message-box (vec (first x)))))])
             left  workspace-tree
;;             right (do-mk-table-control [:set-column-count 12]
;;                                    [:set-row-count 5])
             right (do-mk-context-editor)
             
             pane (do-mk-split-pane :horiz left right
                    [:set-divider-location 200]) ]
        (do
          (add-tab-with-name-icon-tooltip frame (get-widget pane)
            "Contexts" nil "View and edit contexts")
          (dosync 
            (ref-set context-workspace-tree workspace-tree)
            (ref-set context-pane pane)
            (ref-set +debug+ right)) ))) ) )

(defn plug-unload-hook
  "Unloads the context-editor plugin.
   Parameters:
     frame    _frame that contains the user interface
  "
  [frame]
  (with-swing-threads
    (remove-tab frame @context-pane)
    (dosync 
      (ref-set context-pane nil)
      (ref-set context-workspace nil)
      (ref-set context-workspace-tree nil))))
   

(define-plugin context-editor
  "Context editor plugin."
  :load-hook plug-load-hook,
  :unload-hook plug-unload-hook)

nil
