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
    [clojure.contrib.string :only (join split-lines split)]))

;; (ns conexp.gui.editors.contexts)

;;
;;
;; Helper functions
;;
;;
;;
;;

(defn map-to-unique-strings
  "Takes a sequence of (unique) keys and returns a map
   that maps each unique key to a unique string

  Parameters:
   keys    _sequence of (unique) keys"
  [keys]
  (loop [ k (set keys)        
          m {} 
          taken #{}
          nbr 0]
    (if (empty? k) m
      (let [ k1 (first k)
             k1-as-str (if (< 0 nbr)
                         (join "-" [(str k1) (str nbr)])
                         (str k1))]
        (if (contains? taken k1-as-str)
          (recur k m taken (+ 1 nbr))
          (recur (disj k k1) (conj m {k1 k1-as-str})
            (conj taken k1-as-str) 0))))))

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
         :context (ref nil)

         :associate-context
         (fn-doc "Associates a context with the widget object and implicitly
  updates the control

  Parameters:
    ectx      _editable context"
           [ectx]
           (let [ ctx (if (editable-context? ectx) ectx 
                       (make-editable-context ectx))
                  ctx-ref (@self :context)
                  old-ctx @ctx-ref]
             (do
               (dosync-wait (ref-set ctx-ref nil))
               (if (not (nil? old-ctx)) (!! old-ctx :remove-widget- @self))
               (!! ctx :add-widget- @self))
               (dosync-wait (ref-set ctx-ref ctx))))


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
    context-in     _context or editable context"
  ([context-in]
    (let [ ctx (get-context context-in)
           obj (objects ctx)
           attr (attributes ctx)
           inc (incidence ctx)
           obj-map (map-to-unique-strings obj)
           attr-map (map-to-unique-strings attr)
           compatible-obj (map obj-map obj)
           compatible-attr (map attr-map attr)
           compatible-inc  (map (fn [x] [(obj-map (first x)) 
                                          (attr-map (second x))]) inc)
           compatible-ctx (make-context compatible-obj
                            compatible-attr compatible-inc)
           attr-range (range 1 (+ 1 (count compatible-attr)))
           assoc-widgets (ref #{})
           context (ref compatible-ctx)
           editable-ctx 
           {:editable-context 1
           :context context
           :associated-widgets assoc-widgets
           :attribute-columns (ref (conj 
                                     (zipmap compatible-attr attr-range)
                                     (zipmap attr-range compatible-attr)))

           :remove-widget-
           (fn-doc "Removes a widget from associated widgets.
  Parameters:
    widget     _widget to be removed"
             [widget]
             (dosync-wait (commute assoc-widgets disj widget)))

           :add-widget-
           (fn-doc "Adds a widget to associated widgets and updates the control
  accordingly.

  Parameters:
    widget    _context-widget to be added"
             [widget]
             (do
               (dosync-wait (commute assoc-widgets conj widget))
               (let [ table (widget :table)
                      ctx @context 
                      fca-ctx (get-context ctx)]
                 (do
                   (!! table :set-column-count 
                     (+ 1 (count (attributes fca-ctx))))
                   (!! table :set-row-count
                     (+ 1 (count (objects fca-ctx))))
                   ))))
          }]
        editable-ctx))
 ([]
   (make-editable-context (make-context [] [] []))))

(def ctx (conexp.fca/make-context ["a" "b" "c"] [1 2 3] [["a" 1] ["c" 3]]))
(def ectx (make-editable-context ctx))
(def gui conexp/gui)

(def context-pane (ref nil))
(def context-workspace (ref nil))
(def context-workspace-tree (ref nil))
(def +debug+ (ref nil))
(defn +debug-hook+ []
  (!! @+debug+ :associate-context ectx))

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
          (dosync-wait 
            (ref-set context-workspace-tree workspace-tree)
            (ref-set context-pane pane)
            (ref-set +debug+ right)) 
          (+debug-hook+)))) ) )

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
