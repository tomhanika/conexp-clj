;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; This file has been written by Immanuel Albrecht, with modifications by DB

(ns conexp.contrib.gui.editors.context-editor.table-control
  (:use [conexp.base :only (zip)]
        conexp.contrib.gui.util
        conexp.contrib.gui.util.hookable
        conexp.contrib.gui.editors.context-editor.widgets)
  (:use [clojure.contrib.string :only (join split-lines split)])
  (:import [javax.swing JComponent AbstractAction JTable
                        JScrollPane KeyStroke]
           [javax.swing.event TableModelListener]
           [java.awt.event ActionListener KeyEvent ActionEvent]
           [java.awt Point]
           [javax.swing.table DefaultTableModel]))
  
;;;  Table

(defrecord table-control [widget control hooks model row-permutator])
;; row-permutator is a ref to an two-element vector of functions, 
;; where the first-row element transforms view-rows to index-rows 
;; and the second element is the inverse function transforming
;; index-rows to view-rows.
(derive ::table-control :conexp.contrib.gui.editors.context-editor.widgets/control)
(derive ::table-control :conexp.contrib.gui.util.hookable/hookable)

(defmulti get-table
  "Returns the table-control that belongs to the first parameter."
  (fn [& x]
    (class-to-keyword (type (first x))))
  :default nil)

(defmethod get-table ::table-control
  [x] x)

(defn-typecheck-swing get-row-count ::table-control
  "Returns the number of rows of the table control."
  [otable-control]
  (.getRowCount (get-control otable-control)))

(defn-typecheck-swing get-column-count ::table-control
  "Returns the number of columns of the table control."
  [otable-control]
  (.getColumnCount (get-control otable-control)))

(declare get-column-index-permutator set-column-index-permutator)

(defn-typecheck-swing-threads* set-column-count ::table-control
  "Sets the number of columns of the table control."
  [otable-control column-count]
  (let [p (get-column-index-permutator otable-control)]
    (.setColumnCount (:model otable-control) column-count)
    (set-column-index-permutator otable-control p)))

(declare get-row-index-permutator set-row-index-permutator)

(defn-typecheck-swing-threads* set-row-count ::table-control
  "Sets the number of rows of the table control."
  [otable-control row-count]
  (let [p (get-row-index-permutator otable-control)]
    (set-row-index-permutator otable-control identity)
    (.setRowCount (:model otable-control) row-count)
    (set-row-index-permutator otable-control p)))

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
  (let [java-condition ({:focus JComponent/WHEN_FOCUSED
                         :widget JComponent/WHEN_IN_FOCUSED_WINDOW
                         :ancestor JComponent/WHEN_ANCESTOR_OF_FOCUSED_COMPONENT}
                        condition),
         widget otable,
         table (get-control otable),
         action (proxy [AbstractAction ActionListener] []
                  (actionPerformed [event]
                    (handler widget))),
         input-map (.getInputMap table java-condition),
         action-map (.getActionMap table),
         cmd-name (str name)]
    (.put input-map keystroke cmd-name)
    (.put action-map cmd-name action)))

(defn-typecheck-swing get-column-index ::table-control
  "Returns the tables model index of the specified column in the
  view."
  [otable column]
  (let [table     (get-control otable),
        col-count (.getColumnCount table)]
    (if (>= column col-count)
      column
      (let [col-model (.getColumnModel table),
            table-col (.getColumn col-model column) ]
        (.getModelIndex table-col)))))

(defn-typecheck-swing get-index-column ::table-control
 "Returns the column in the view that corresponds to the specified
  table model index."
  [otable index]
  (let [table     (get-control otable),
        col-model (.getColumnModel table),
        cols      (range (.getColumnCount col-model)),
        matching  (filter #(= index
                              (.getModelIndex (.getColumn col-model %)))
                          cols),
         found    (first matching)]
    (or found index)))

(defn-typecheck get-row-index ::table-control
  "Returns the tables model index of the specified row in view."
  [otable row]
  (let [p (deref (:row-permutator otable))]
    ((first p) row)))

(defn-typecheck get-index-row ::table-control
  "Returns the row in the view that corresponds
  to the specified table model index."
  [otable index]
  (let [p (deref (:row-permutator otable))]
    ((second p) index)))

(defn-typecheck get-row-index-permutator ::table-control
  "Returns a function that will map the current view rows to
   the according index values."
  [otable]
  (let [p (deref (:row-permutator otable))]
    (first p)))

(defn-typecheck-swing get-column-index-permutator ::table-control
  "Returns a function that will map the current view
    columns to the according index values."
  [otable]
  (let [table     (get-control otable),
        col-count (.getColumnCount table),
        col-range (range col-count),
        col-model (.getColumnModel table),
        col-map   (zipmap col-range
                          (map #(.getModelIndex (.getColumn col-model %))
                               col-range))]
    (fn [i]
      (if (>= i col-count)
        i
        (col-map i)))))

(defn-typecheck-swing get-value-at-view ::table-control
  "Returns the value of the cell at specified position in view."
  [otable row column]
  (let [table (get-control otable)]
    (.getValueAt table row column)))

(defn-typecheck-swing get-value-at-index ::table-control
  "Returns the value of the cell at specified position in the table
  model."
  [otable row column]
  (let [irow (get-index-row otable row),
        icolumn (get-index-column otable column)]
    (get-value-at-view otable irow icolumn)))

(defn-typecheck-swing-threads* set-resize-mode ::table-control
  "Sets the behaviour of the table on resize. mode is either one
  of :all, :last, :next, :off, or :subseq"
  [otable mode]
  (.setAutoResizeMode (get-control otable)
                      ({:all JTable/AUTO_RESIZE_ALL_COLUMNS
                        :last JTable/AUTO_RESIZE_LAST_COLUMN
                        :next JTable/AUTO_RESIZE_NEXT_COLUMN
                        :off JTable/AUTO_RESIZE_OFF
                        :subseq JTable/AUTO_RESIZE_SUBSEQUENT_COLUMNS}
                       mode)))

(defn-typecheck-swing-threads* set-cell-selection-mode ::table-control
  "Sets the cell selection mode. mode is either one
  of :none, :rows, :columns or :cells"
  [otable mode]
  (let [table (get-control otable)]
    (condp = mode
     :none    (.setCellSelectionEnabled table false)
     :rows    (doto table
                (.setColumnSelectionAllowed false)
                (.setRowSelectionAllowed true))
     :columns (doto table
                (.setRowSelectionAllowed false)
                (.setColumnSelectionAllowed true))
     :cells   (.setCellSelectionEnabled table true))))

(defn-typecheck-swing-threads* select-single-cell ::table-control
  "Selects a single cell given as view-coordinates in the given table control"
  [otable row column]
  (.changeSelection (get-control otable) row column false false))

(defn-typecheck-swing set-value-at-view ::table-control
  "Sets the value of a cell in the table according to a view position."
  [otable row column contents]
  (let [columns (get-column-count otable),
        rows  (get-row-count otable)]
    (if (>= column columns)
      (call-hook otable "extend-columns-to" (+ 1 column)))
    (if (>= row rows)
      (call-hook otable "extend-rows-to" (+ 1 row)))
    (.setValueAt (get-control otable) (str contents) row column)))

(defn-typecheck set-value-at-index ::table-control
  "Sets the value of a cell in the table according to the model
   index." [otable row column contents]
  (set-value-at-view otable (get-index-row otable row)
    (get-index-column otable column) contents))

(defn-typecheck update-value-at-index ::table-control
  "Sets the value of a cell in the table according to the model index,
   if it is different from the current cells value."
  [otable row column contents]
  (let [current (get-value-at-index otable row column)]
    (if (not= current contents)
      (set-value-at-index otable row column contents))))

(defn-typecheck-swing-threads* paste-from-clipboard ::table-control
  "Pastes the current system clipboard contents into the table."
  [obj]
  (let [control     (get-control obj),
        sel-columns (-> control .getSelectedColumns seq),
        sel-rows    (-> control .getSelectedRows seq),
        col-index   (get-column-index-permutator obj),
        row-index   (get-row-index-permutator obj)]
    (if (or (empty? sel-columns) (empty? sel-rows))
      nil
      (let [data   (str (get-clipboard-contents)),
            startx (first sel-columns),
            starty (first sel-rows),
            cells  (vec (map #(vec (split #"\t" %))
                             (split-lines data))),
            lns    (range (count cells))]
        (doseq [r lns]
          (doseq [c (range (count (cells r)))]
            (set-value-at-index obj
                                (row-index (+ starty r))
                                (col-index (+ startx c))
                                ((cells r) c))))))))

(defn-typecheck-swing-threads* copy-to-clipboard ::table-control
  "Copies the selected cells from the table widget to the system
   clipboard."
  [obj]
  (let [control     (get-control obj),
        sel-columns (-> control .getSelectedColumns seq),
        sel-rows    (-> control .getSelectedRows seq),
        sel-pairs   (map (fn [y]
                           (map (fn [x]
                                  (list y x)) sel-columns))
                         sel-rows),
        sel-values  (map (fn [l]
                           (map (fn [p]
                                  (str (get-value-at-view obj
                                                          (first p)
                                                          (second p)))) l))
                         sel-pairs),
        sel-lines   (map #(join "\t" %) sel-values)
        selection   (join "\n" sel-lines)]
    (set-clipboard-contents selection)))

(defn-typecheck-swing get-view-coordinates-at-point ::table-control
  "Returns the current view coordinates as [row column] for the given
   point."
  [otable position]
  (let [x (first position),
        y (second position)]
    [(.rowAtPoint (get-control otable) (Point. x y)),
     (.columnAtPoint (get-control otable) (Point. x y))]))

(defn-typecheck-swing move-column ::table-control
  "Moves the column at view index old-view to be viewed at view index
   new-view."
  [otable old-view new-view]
  (let [ control (get-control otable) 
         col-model (.getColumnModel control) ]
    (.moveColumn col-model old-view new-view)))

(defn-typecheck-swing-threads* set-column-index-permutator ::table-control
  "Takes a table object and a column-index permutator and
  rearranges the columns accordingly."
  [otable col-idx]
  (let [ col-count (get-column-count otable) ]
    (doseq [col (range col-count)]
      (let [at-view (get-index-column otable (col-idx col))]
        (move-column otable at-view col)))))

(defn-typecheck-swing-threads* move-row ::table-control
  "Moves the row at view index old-view to be viewed at view index
   new-view."
  [otable old-view new-view]
  (let [ view-to-index (get-row-index-permutator otable)
         row-count (get-row-count otable)
         col-indices (range (get-column-count otable))]
    (if (and (>= new-view 0)
         (< new-view row-count)
          (not= old-view new-view))
      (if (< old-view new-view)
        (let [ new-view-to-index-map 
               (apply conj (for [r (range row-count)]
                             (cond (< r old-view) {r (view-to-index r)}
                               (= r old-view) {new-view (view-to-index old-view)}
                               (<= r new-view) {(- r 1) (view-to-index r)}
                               :otherwise {r (view-to-index r)})))
               new-index-to-view-map 
               (apply conj (for [r (range row-count)]
                             (cond (< r old-view) {(view-to-index r) r}
                               (= r old-view) {(view-to-index old-view) new-view}
                               (<= r new-view) {(view-to-index r) (- r 1)}
                               :otherwise {(view-to-index r) r})))
               new-view-to-index (fn [x] (if (and (>= x 0) (< x row-count)) 
                                           (new-view-to-index-map x) x))
               new-index-to-view (fn [x] (if (and (>= x 0) (< x row-count)) 
                                           (new-index-to-view-map x) x))
               new-row-permutator [new-view-to-index new-index-to-view]
               grab-list (range old-view (+ 1 new-view))
               put-list (apply conj [new-view] (range old-view new-view))
               old-table-change-hook (get-hook-function otable "table-changed")]
          (set-hook otable "table-changed" (fn [_ _ _ _] nil))
          (dorun (for [put-data 
                        (doall (zip put-list 
                           (for [r grab-list] 
                             (doall (for [c col-indices] 
                                      (get-value-at-view otable r c))))))]
            (doseq [put-cell (zip col-indices (second put-data))]
              (set-value-at-view otable (first put-data) 
                (first put-cell) (second put-cell)))))
          (dosync (ref-set (:row-permutator otable) new-row-permutator))
          (set-hook otable "table-changed" old-table-change-hook))

        (let [ new-view-to-index-map 
               (apply conj (for [r (range row-count)]
                             (cond (< r new-view) {r (view-to-index r)}
                               (= r old-view) {new-view (view-to-index old-view)}
                               (< r old-view) {(+ r 1) (view-to-index r)}
                               :otherwise {r (view-to-index r)})))
               new-index-to-view-map 
               (apply conj (for [r (range row-count)]
                             (cond (< r new-view) {(view-to-index r) r}
                               (= r old-view) {(view-to-index old-view) new-view}
                               (< r old-view) {(view-to-index r) (+ r 1)}
                               :otherwise {(view-to-index r) r})))
               new-view-to-index (fn [x] (if (and (>= x 0) (< x row-count)) 
                                           (new-view-to-index-map x) x))
               new-index-to-view (fn [x] (if (and (>= x 0) (< x row-count)) 
                                           (new-index-to-view-map x) x))
               new-row-permutator [new-view-to-index new-index-to-view]
               grab-list (range new-view (+ 1 old-view))
               put-list (conj (vec (range (+ 1 new-view) (+ 1 old-view))) new-view)
               old-table-change-hook (get-hook-function otable "table-changed")]
          (set-hook otable "table-changed" (fn [_ _ _ _] nil))
          (dorun (for [put-data 
                        (doall (zip put-list 
                           (for [r grab-list] 
                             (doall (for [c col-indices] 
                                      (get-value-at-view otable r c))))))]
            (doseq [put-cell (zip col-indices (second put-data))]
              (set-value-at-view otable (first put-data) 
                (first put-cell) (second put-cell)))))
          (dosync (ref-set (:row-permutator otable) new-row-permutator))
          (set-hook otable "table-changed" old-table-change-hook))))))

(defn-typecheck-swing-threads* set-row-index-permutator ::table-control
  "Takes a table object and a row-index permutator and
  rearranges the rows accordingly."
  [otable row-idx]
  (let [ row-count (get-row-count otable) ]
    (doseq [row (range row-count)]
      (let [at-view (get-index-row otable (row-idx row))]
        (move-row otable at-view row)))))

(defn- table-change-hook
  [otable column first-row-in-view last-row-in-view type]
  (if (and (= 0 type)
           (<= 0 (min column first-row-in-view last-row-in-view))
           (= first-row-in-view last-row-in-view))
    (let [ first-row (get-row-index otable first-row-in-view)
           current-value (get-value-at-index otable first-row column),
           good-value (call-hook otable "cell-value" first-row column
                                current-value)]
      (if (not= current-value good-value)
        (set-value-at-index otable first-row column good-value)))))

(defn-swing make-table-control
  "Creates a table control in Java. Here setup is an optional number
  of vectors that may contain additional tweaks that are called after
  widget creation"
  [& setup]
  (let [model (DefaultTableModel.),
        table (JTable. model),
        pane  (JScrollPane. table
                            JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED
                            JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED),
        keystroke-copy  (KeyStroke/getKeyStroke KeyEvent/VK_C
                                                ActionEvent/CTRL_MASK false),
        keystroke-paste (KeyStroke/getKeyStroke KeyEvent/VK_V
                                                ActionEvent/CTRL_MASK false),

        hooks           (:hooks (make-hookable)),
        widget          (table-control. pane table hooks model 
                          (ref [identity identity])),
        change-listener (proxy [TableModelListener] []
                          (tableChanged [event]
                            (with-swing-threads*
                              (let [column (.getColumn event),
                                    first  (.getFirstRow event),
                                    last   (.getLastRow event),
                                    type   (.getType event)]
                                (call-hook widget "table-changed"
                                           column first last type))))),

        defaults [[set-resize-mode :off],
                  [set-cell-selection-mode :cells],
                  [register-keyboard-action
                   copy-to-clipboard "Copy" keystroke-copy :focus],
                  [register-keyboard-action
                   paste-from-clipboard "Paste" keystroke-paste :focus]],

        ignore-event      (fn [x] nil),
        show-data         (fn [x] (message-box (str x))),
        drag-start        (ref nil),
        button-down-event (fn [x]
                            (if (and (= (:button x) 1)
                                     (= (:modifiers x) #{:alt}))
                              (dosync
                               (ref-set drag-start
                                        (get-view-coordinates-at-point 
                                         widget (:position x))))
                              (dosync (ref-set drag-start nil)))),
        button-up-event (fn [x]
                          (if (and (= (:button x) 1)
                                   (= (:modifiers x) #{:alt})
                                   (not= (deref drag-start) nil))
                            (let [start     (deref drag-start),
                                  end       (get-view-coordinates-at-point
                                             widget (:position x)),
                                  start-row (first start),
                                  start-col (second start),
                                  end-row   (first end),
                                  end-col   (second end)]
                              (dosync (ref-set drag-start nil))
                              (if (not= start-col end-col)
                                (move-column widget start-col end-col))
                              (if (not= start-row end-row)
                                (move-row widget start-row end-row))))),
        drag-motion-event (fn [x]
                             (if (not= (deref drag-start) nil)
                               (let [start     (deref drag-start),
                                     end       (get-view-coordinates-at-point
                                                widget (:position x)),
                                     start-row (first start),
                                     start-col (second start),
                                     end-row   (first end),
                                     end-col   (second end)]
                                 (select-single-cell widget end-row end-col)
                                 (dosync (ref-set drag-start end))
                                 (if (not= start-col end-col)
                                   (move-column widget start-col end-col))
                                 (if (not= start-row end-row)
                                   (move-row widget start-row end-row))))),
        cell-permutor (proxy-mouse-listener button-down-event
                                            button-up-event
                                            ignore-event
                                            ignore-event
                                            ignore-event
                                            ignore-event
                                            drag-motion-event)]
    (add-hook widget "table-changed"
      (fn [c f l t] (table-change-hook widget c f l t))
      "This hook is called whenever a table widget is changed,
       Parameters:
             column   _the column index of the changed area
             first    _the first row *view* of the changed area
             last     _the last row *view* of the changed area
             type     _(-1,0, or 1) delete, update, insert")
    (add-hook widget "extend-columns-to" #(set-column-count widget %)
      "This hook is called whenever the table needs to extend its
       columns in order to write to a cell. The hook is supposed
       to call set-column-count on the table widget.
       Parameters:
             columns  _the requested column count")
    (add-hook widget "extend-rows-to" #(set-row-count widget %)
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
    widget))

;;;

nil
