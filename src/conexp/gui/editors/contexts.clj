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

(defn string-to-cross
  "Takes a string and decides, whether it should be a cross or not

  Parameters:
   s     _string
  Returns true if there should be a cross"
  [s]
  (let [trimmed (.trim s)]
    (if (re-matches #"0*[,.]?0*" trimmed) false true)))

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

(defn req-unique-string
  "Takes a sequence of keys and a new requested key and
  returns a new key that will not conflict with any of the
  keys in the sequence.

  Parameters:
    old-keys  _sequence of keys
    req-key   _requested new key"
  [old-keys req-key]
  (let [keys (set old-keys)]
    (loop [ nbr 0 ]
      (let [new-key (if (< 0 nbr)
                      (join "-" [(str req-key) (str nbr)])
                      (str req-key))]
        (if (contains? keys new-key)
          (recur (+ 1 nbr))
          new-key)))))

(defn switch-bipartit-auto
  "Takes a bipartit auto map and removes the old-key and old-value
   associations and associates new-key with new-value and new-value
   with new-key, then returns the new map

   Parameters:
     m         _bipartit auto map   
     old-key   _old key
     old-value _old value
     new-key   _new key
     new-value _new value"
  [m old-key old-value new-key new-value]
  (let [m-without-old (dissoc m old-key old-value)
        m-with-new (assoc m-without-old new-key new-value new-value new-key)]
    m-with-new))


;;
;;
;; Context editor control
;;
;;
;;
;;

(declare editable-context? make-editable-context)

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
                     (make-button "Copy" 
                       (get-ui-icon "OptionPane.informationIcon")
                       [set-handler (fn [] (table-to-clipboard! table))] )]
                   [:add-button
                     (make-button "Paste" 
                       (get-ui-icon "OptionPane.informationIcon")
                       [set-handler (fn [] (clipboard-to-table! table))] ) ])

                   
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
      ;(set-handler paste-button (fn [] (clipboard-to-table! table)))
      ;(!! toolbar :add-button 
      ;               (make-button "Copy" 
      ;                 (get-ui-icon "OptionPane.informationIcon")
      ;                 [set-handler (fn [] (table-to-clipboard! table))]))
      (doseq [x defaults] (unroll-parameters-fn-map widget x))
      (doseq [x setup] (unroll-parameters-fn-map widget x))
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
    (let [ self (promise)
           ctx (get-context context-in)
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
           obj-range (range 1 (+ 1 (count compatible-obj)))


           assoc-widgets   (ref #{})
           context         (ref compatible-ctx)
           attr-cols       (ref (conj (zipmap compatible-attr attr-range)
                                  (zipmap attr-range compatible-attr)))
           obj-rows        (ref (conj (zipmap compatible-obj obj-range)
                                  (zipmap obj-range compatible-obj)))


           extend-rows-hook
           (fn-doc "This hook is called when the number of rows is extended by
  pasting content that is too big for the table.

  Parameters:
    widget   _the widget object where the paste operation takes place
    old-rows _the number of rows that were present in the table before
    new-rows _the number of rows that will be needed for the paste"
             [table old-rows new-rows]
             (let [ other-widgets (filter
                                    (fn [w] (not= (w :table) table))
                                    @assoc-widgets)]
               (do
                 (doseq [w other-widgets] (!!!! (w :table) 
                                            :extend-rows-to new-rows))
                 (doseq [x (range old-rows new-rows)] (!! table 
                                                        :set-index-at x 0 
                                                        "new object")))))

           extend-columns-hook
           (fn-doc "This hook is called when the number of columns is extended
  by pasting content that is too big for the table.

  Parameters:
    widget      _the widget object where the paste operation takes place
    old-columns _the number of rows that were present in the table before
    new-columns _the number of rows that will be needed for the paste"
             [table old-columns new-columns]
             (let [ other-widgets (filter
                                    (fn [w] (not= (w :table) table))
                                    @assoc-widgets)]
               (do
                 (doseq [w other-widgets]
                   (!!!! (w :table) :extend-columns-to new-columns))
                 (doseq [x (range old-columns new-columns)]
                   (!! table :set-index-at 0 x "new attribute")))))

           update-incidence
           (fn-doc "This function updates the incidence relation.

  Parameters:
    object     _the object
    attribute  _the attribute
    cross      _true, if object has attribute, false otherwise"
             [object attribute cross]
             (dosync-wait (commute context 
                            (fn [x]
                              (let [ objs (objects x)
                                     attrs (attributes x)
                                     inc (incidence x) 
                                     op (if cross conj disj)]
                                (make-context objs attrs 
                                  (op inc [object attribute])))))))

           change-attribute-name
           (fn-doc "This function will change the name of the attribute in
  the specified column to the new given name, if the new name does not conflict,
  or otherwise to a conflict free new name; and returns the new name of the
  attribute.

  Parameters:
    model-column   _column of the attribute in the model
    req-name       _requested new attribute name"
             [model-column req-name]
             (dosync-wait
               (let [ attribs (attributes (get-context @context))
                      current-name (@attr-cols model-column)
                      other-attribs (disj attribs current-name)
                      new-name (req-unique-string other-attribs
                                 req-name) ]
                 (do
                   (if (not= current-name new-name)
                     (let [ amap (fn [x] (if (= x current-name) new-name x))]
                       (dosync
                         (commute context
                           (fn [x] (let [as (conj (disj (attributes x) 
                                                    current-name)
                                              new-name)
                                         os (objects x)
                                         inc (map 
                                               (fn [x] [ (first x) 
                                                         (amap (second x))])
                                               (incidence x))]
                                     (make-context os as inc))))
                         (commute attr-cols switch-bipartit-auto 
                           model-column current-name
                           model-column new-name))))                         
                   new-name))))

           change-object-name
           (fn-doc "This function will change the name of the object in
  the specified row to the new given name, if the new name does not conflict,
  or otherwise to a conflict free new name; and returns the new name of the
  object.

  Parameters:
    model-row      _row of the object in the model
    req-name       _requested new object name"
             [model-row req-name]
             (dosync-wait
               (let [ objs (objects (get-context @context))
                      current-name (@obj-rows model-row)
                      other-objs (disj objs current-name)
                      new-name (req-unique-string other-objs
                                 req-name) ]
                 (do
                   (if (not= current-name new-name)
                     (let [ omap (fn [x] (if (= x current-name) new-name x))]
                       (dosync
                         (commute context
                           (fn [x] (let [os (conj (disj (objects x) 
                                                    current-name)
                                              new-name)
                                         as (attributes x)
                                         inc (map 
                                               (fn [x] [ (omap (first x))
                                                         (second x)])
                                               (incidence x))]
                                     (make-context os as inc))))
                         (commute obj-rows switch-bipartit-auto 
                           model-row current-name
                           model-row new-name))))                         
                   new-name))))
             
           set-cell-value-hook
           (fn-doc "This hook is called when a cell's value in the table widget
  has been changed, the passed contents have to be translated into allowed con-
  tents that will eventually be set in the widget.

  Parameters:
    widget     _the widget object
    row        _integer identifying a row 
    column     _integer identifying a column 
    contents   _the desired new contents

  Returns the new allowed contents for the cell (as string)."
             [table row column contents]
             (let [ model-column column
                    model-row row
                    good-value
                    (cond (= model-column model-row 0) "⇊objects⇊"
                      (= model-row 0) 
                      (let [attrib-cols @attr-cols
                            current-name (attrib-cols model-column)] 
                        (if (= contents current-name) current-name
                          (change-attribute-name model-column contents)))
                      (= model-column 0)
                      (let [object-rows @obj-rows
                             current-name (object-rows model-row)]
                        (if (= contents current-name) current-name
                          (change-object-name model-row contents)))
                      true ;else
                      (let [ cross (string-to-cross contents)
                             obj-name (@obj-rows model-row)
                             attr-name (@attr-cols model-column)
                             fca-ctx (get-context @context)
                             inc (incidence fca-ctx)
                             current-state (contains? inc [obj-name attr-name])]
                        (do
                          (if (not= current-state cross)
                            (update-incidence obj-name attr-name cross))
                          (if cross "X" " ")))
                      )
                    other-widgets (filter
                                    (fn [w] (not= (w :table) table))
                                    @assoc-widgets)]
               (do
                 (doseq [w other-widgets]
                   (!! (w :table) :update-index-at 
                             row column good-value))
                 good-value)))
            

           set-table-extents
           (fn-doc "This function will set the row and column count of the
  table widget to the appropriate sizes.

  Parameters:
    table  _table widget"
             [table]
             (dosync
               (let [ ctx @context 
                      fca-ctx (get-context ctx)
                      attrs (attributes fca-ctx)
                      attr-count (count attrs)
                      objs (objects fca-ctx)
                      obj-count (count objs)]
                 (do
                   (!! table :set-column-count (+ 1 attr-count))
                   (!! table :set-row-count (+ 1 obj-count))))))
                      
           
           fill-table-widget
           (fn-doc "This function will fill the given table widget with the
  contents of the editable context.

  Parameters:
    table   _table widget"
             [table]
             (dosync
               (let [ ctx @context 
                      fca-ctx (get-context ctx)
                      attrs (attributes fca-ctx)
                      inc (incidence fca-ctx)
                      get-cross (fn [obj att] (if (contains? inc [obj att])
                                                "X" " "))
                      attr-count (count attrs)
                      objs (objects fca-ctx)
                      obj-count (count objs)
                      attrib-cols @attr-cols
                      object-rows @obj-rows]
                 (do
                   (!! table :set-column-count (+ 1 attr-count))
                   (!! table :set-row-count (+ 1 obj-count))
                   (!! table :set-index-at 0 0 "")
                   (doseq [x attrs]
                     (let [column (attrib-cols x)]
                               (!! table :set-index-at 0 column x)))
                   (doseq [x objs]
                     (let [row (object-rows x)]
                               (!! table :set-index-at row 0 x)))
                   (doseq [a attrs]
                    (doseq [o objs]
                                 (let [ row (object-rows o)
                                        column (attrib-cols a)]
                                   (!! table :set-index-at row column 
                                     (get-cross o a)))))))))

           

           editable-ctx 
           {:editable-context 1
           :context context
           :associated-widgets assoc-widgets
           :attribute-columns attr-cols
           :object-rows obj-rows

           :get
           (fn-doc "Returns the fca-context that is associated with the
  current state of the editable context."
             [] (get-context @context))

           :set
           (fn-doc "Sets the current state of the editable context to the
  given fca context.

  Parameters:
    context-in  _(fca) context"
             [context-in]
             (let [ ctx (get-context context-in)
                    obj (objects ctx)
                    attr (attributes ctx)
                    inc (incidence ctx)
                    obj-map (map-to-unique-strings obj)
                    attr-map (map-to-unique-strings attr)
                    compatible-obj (map obj-map obj)
                    compatible-attr (map attr-map attr)
                    compatible-inc  (map (fn [x] [(obj-map (first x)) 
                                                   (attr-map (second x))])
                                      inc)
                    compatible-ctx (make-context compatible-obj
                                     compatible-attr compatible-inc) 
                    attr-range (range 1 (+ 1 (count compatible-attr)))
                    obj-range (range 1 (+ 1 (count compatible-obj)))


                    
                    new-attr-cols  (conj (zipmap compatible-attr attr-range)
                                     (zipmap attr-range compatible-attr))
                    new-obj-rows   (conj (zipmap compatible-obj obj-range)
                                     (zipmap obj-range compatible-obj)) ]
               (do
                 (dosync-wait 
                   (ref-set context compatible-ctx)
                   (ref-set attr-cols new-attr-cols)
                   (ref-set obj-rows new-obj-rows))
                 (!! @self :update-))))

           :update-
           (fn-doc "Updates all widgets that are associated with the editable
  context by forcing a complete table refresh from data."
             []
             (let [ widgets @assoc-widgets
                    tables  (map :table widgets)]
               (dosync
                 (doseq [t tables] (set-table-extents t))
                 (doseq [t tables] (fill-table-widget t)))))

           :remove-widget-
           (fn-doc "Removes a widget from associated widgets.
  Parameters:
    widget     _widget to be removed"
             [widget]
             (do
               (dosync-wait (commute assoc-widgets disj widget))
               (let [ table (widget :table)
                      empty-ext-hook (fn-doc "Empty extend hook" 
                                       [widget old-rows new-rows] nil)
                      empty-val-hook (fn-doc "Empty set cell value hook"
                        [widget row column contents]
                        contents)]
                 (do
                   (!! table :set-handler-and-wait 
                     :set-cell-value-hook empty-val-hook)
                   (!! table :set-handler-and-wait 
                     :extend-columns-hook empty-ext-hook)
                   (!! table :set-handler-and-wait 
                     :extend-rows-hook empty-ext-hook)))))

           :add-widget-
           (fn-doc "Adds a widget to associated widgets and updates the control
  accordingly.

  Parameters:
    widget    _context-widget to be added"
             [widget]
             (do
               (dosync-wait (commute assoc-widgets conj widget))
               (let [ table (widget :table)]
                 (do
                   (!! table :set-handler-and-wait 
                     :set-cell-value-hook set-cell-value-hook)
                   (!! table :set-handler-and-wait 
                     :extend-columns-hook extend-columns-hook)
                   (!! table :set-handler-and-wait 
                     :extend-rows-hook extend-rows-hook)
                   (fill-table-widget table)))))
          }]
               (do
                 (deliver self editable-ctx)
                 editable-ctx)))
 ([]
   (make-editable-context (make-context [] [] []))))

(def ctx (conexp.fca/make-context ["a" "b" "c"] [1 2 3] [["a" 1] ["c" 3]]))
(def ctx2 (make-context #{"Apfel" "Birne" "Möhre" "Gurke"} 
            #{"lecker" "geht so" "iieh"} 
            #{["Apfel" "lecker"] ["Birne" "lecker"] ["Möhre" "iieh"]
            ["Gurke" "geht so"]}))

(def ectx (make-editable-context ctx))


(def context-pane (ref nil))
(def context-workspace (ref nil))
(def context-workspace-tree (ref nil))
(def +debug+ (ref nil))
(defn +debug-hook+ []
  (do
    (doseq [x @+debug+] (!! x :associate-context ectx))
          ))

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
             right2 (do-mk-context-editor)
             rpane (do-mk-split-pane :vert right right2
                     [:set-divider-location 300])
             
             pane (do-mk-split-pane :horiz left rpane
                    [:set-divider-location 200]) 
             ]
        (do
          (add-tab-with-name-icon-tooltip frame (get-widget pane)
            "Contexts" nil "View and edit contexts")
          (dosync-wait 
            (ref-set context-workspace-tree workspace-tree)
            (ref-set context-pane pane)
            (ref-set +debug+ [right right2])) 
          (+debug-hook+)
          ))) ) )

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
