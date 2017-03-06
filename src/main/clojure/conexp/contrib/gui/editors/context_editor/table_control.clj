;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; This file has been written by Immanuel Albrecht, with modifications by DB

(ns conexp.contrib.gui.editors.context-editor.table-control
  (:use [conexp.base :exclude (join)]
        conexp.contrib.gui.util
        conexp.contrib.gui.editors.context-editor.widgets)
  (:use [clojure.string :only (join split-lines split)])
  (:import [javax.swing JComponent AbstractAction JTable
                        JScrollPane KeyStroke DefaultCellEditor JTextField]
           [javax.swing.event TableModelListener TableModelEvent]
           [java.awt.event ActionListener KeyEvent ActionEvent MouseEvent
             InputEvent]
           [javax.swing.event MouseInputAdapter]
           [java.awt Point]
           [javax.swing.table DefaultTableModel TableCellEditor
             DefaultTableCellRenderer]))


;;; mouse-click-interface-helpers

(defmacro- when-mask
  "Takes two bitmasks and checks whether their bitwise-and is not 0,
   if so, calls all other expressions in an implicit do."
  [bit-mask-1 bit-mask-2 & exprs]
  `(when (not= 0 (bit-and ~bit-mask-1 ~bit-mask-2))
     ~@exprs))

(defn- proxy-mouse-listener
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
     clicked   _mouse button clicked
     moved     _mouse has been moved
     dragged   _mouse has been dragged"
  [pressed released entered exited clicked moved dragged]
  (let [translate-button {MouseEvent/NOBUTTON 0
                          MouseEvent/BUTTON1  1
                          MouseEvent/BUTTON2  2
                          MouseEvent/BUTTON3  3},
        translate-modifier (fn [m]
                             (union (when-mask m InputEvent/ALT_DOWN_MASK #{:alt})
                                    (when-mask m InputEvent/ALT_GRAPH_DOWN_MASK #{:alt-gr})
                                    (when-mask m InputEvent/CTRL_DOWN_MASK #{:control})
                                    (when-mask m InputEvent/SHIFT_DOWN_MASK #{:shift})
                                    (when-mask m InputEvent/META_DOWN_MASK #{:meta}))),
        translate-pressed-btns (fn [m]
                                 (union (when-mask m InputEvent/BUTTON1_DOWN_MASK #{1})
                                        (when-mask m InputEvent/BUTTON2_DOWN_MASK #{2})
                                        (when-mask m InputEvent/BUTTON3_DOWN_MASK #{3})))
        translate-event (fn [^MouseEvent event]
                          {:button (translate-button (.getButton event))
                           :modifiers (translate-modifier (.getModifiersEx event))
                           :position [(.getX event) (.getY event)]
                           :click-count (.getClickCount event)
                           :buttons-down (translate-pressed-btns (.getModifiersEx event))})]
    (proxy [MouseInputAdapter] []
      (mousePressed [event]
        (do-swing (pressed (translate-event event))))
      (mouseReleased [event]
        (do-swing (released (translate-event event))))
      (mouseEntered [event]
        (do-swing (entered (translate-event event))))
      (mouseExited [event]
        (do-swing (exited (translate-event event))))
      (mouseClicked [event]
        (do-swing (clicked (translate-event event))))
      (mouseMoved [event]
        (do-swing (moved (translate-event event))))
      (mouseDragged [event]
        (do-swing (dragged (translate-event event)))))))


;;;  Table

(defwidget table-control [conexp.contrib.gui.editors.context_editor.widgets.control,
                          conexp.contrib.gui.editors.context_editor.widgets.hookable]
  [widget control hooks model row-permutator])
;; row-permutator is a ref to an two-element vector of functions,
;; where the first-row element transforms view-rows to index-rows
;; and the second element is the inverse function transforming
;; index-rows to view-rows.

(defmulti get-table
  "Returns the table-control that belongs to the first parameter."
  (fn [& x] (keyword-class (first x))))

(defmethod get-table (class-to-keyword table-control)
  [x] x)

(defmethod get-table :default
  [x]
  (illegal-argument "Don't know how to get table from " x "."))

(defn-swing get-row-count
  "Returns the number of rows of the table control."
  [otable-control]
  (assert (keyword-isa? otable-control table-control))
  (.getRowCount ^JTable (get-control otable-control)))

(defn-swing get-column-count
  "Returns the number of columns of the table control."
  [otable-control]
  (assert (keyword-isa? otable-control table-control))
  (.getColumnCount ^JTable (get-control otable-control)))

(declare get-column-index-permutator set-column-index-permutator)

(defn-swing set-column-count
  "Sets the number of columns of the table control."
  [otable-control column-count]
  (assert (keyword-isa? otable-control table-control))
  (let [p (get-column-index-permutator otable-control)]
    (.setColumnCount ^DefaultTableModel (:model otable-control) column-count)
    (set-column-index-permutator otable-control p)))

(declare get-row-index-permutator set-row-index-permutator)

(defn-swing set-row-count
  "Sets the number of rows of the table control."
  [otable-control row-count]
  (assert (keyword-isa? otable-control table-control))
  (let [p (get-row-index-permutator otable-control)]
    (set-row-index-permutator otable-control identity)
    (.setRowCount ^DefaultTableModel (:model otable-control) row-count)
    (set-row-index-permutator otable-control p)))

(defn-swing register-keyboard-action
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
  (assert (keyword-isa? otable table-control))
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

(defn-swing get-column-index
  "Returns the tables model index of the specified column in the
  view."
  [otable column]
  (assert (keyword-isa? otable table-control))
  (let [^JTable table (get-control otable),
        col-count (.getColumnCount table)]
    (if (>= column col-count)
      column
      (let [col-model (.getColumnModel table),
            table-col (.getColumn col-model column) ]
        (.getModelIndex table-col)))))

(defn-swing get-index-column
  "Returns the column in the view that corresponds to the specified
  table model index."
  [otable index]
  (assert (keyword-isa? otable table-control))
  (let [^JTable table (get-control otable),
        col-model (.getColumnModel table),
        cols      (range (.getColumnCount col-model)),
        matching  (filter #(= index
                              (.getModelIndex (.getColumn col-model %)))
                          cols),
         found    (first matching)]
    (or found index)))

(defn get-row-index
  "Returns the tables model index of the specified row in view."
  [otable row]
  (assert (keyword-isa? otable table-control))
  (let [p (deref (:row-permutator otable))]
    ((first p) row)))

(defn get-index-row
  "Returns the row in the view that corresponds
  to the specified table model index."
  [otable index]
  (assert (keyword-isa? otable table-control))
  (let [p (deref (:row-permutator otable))]
    ((second p) index)))

(defn get-row-index-permutator
  "Returns a function that will map the current view rows to
   the according index values."
  [otable]
  (assert (keyword-isa? otable table-control))
  (let [p (deref (:row-permutator otable))]
    (first p)))

(defn-swing get-column-index-permutator
  "Returns a function that will map the current view
    columns to the according index values."
  [otable]
  (assert (keyword-isa? otable table-control))
  (let [^JTable table (get-control otable),
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

(defn-swing get-value-at-view
  "Returns the value of the cell at specified position in view."
  [otable row column]
  (assert (keyword-isa? otable table-control))
  (.getValueAt ^JTable (get-control otable) row column))

(defn-swing get-value-at-index
  "Returns the value of the cell at specified position in the table
  model."
  [otable row column]
  (assert (keyword-isa? otable table-control))
  (let [irow    (get-index-row otable row),
        icolumn (get-index-column otable column)]
    (get-value-at-view otable irow icolumn)))

(defn-swing set-resize-mode
  "Sets the behaviour of the table on resize. mode is either one
  of :all, :last, :next, :off, or :subseq"
  [otable mode]
  (assert (keyword-isa? otable table-control))
  (.setAutoResizeMode ^JTable (get-control otable)
                      ({:all JTable/AUTO_RESIZE_ALL_COLUMNS
                        :last JTable/AUTO_RESIZE_LAST_COLUMN
                        :next JTable/AUTO_RESIZE_NEXT_COLUMN
                        :off JTable/AUTO_RESIZE_OFF
                        :subseq JTable/AUTO_RESIZE_SUBSEQUENT_COLUMNS}
                       mode)))

(defn-swing set-cell-selection-mode
  "Sets the cell selection mode. mode is either one
  of :none, :rows, :columns or :cells"
  [otable mode]
  (assert (keyword-isa? otable table-control))
  (let [^JTable table (get-control otable)]
    (condp = mode
     :none    (.setCellSelectionEnabled table false)
     :rows    (doto table
                (.setColumnSelectionAllowed false)
                (.setRowSelectionAllowed true))
     :columns (doto table
                (.setRowSelectionAllowed false)
                (.setColumnSelectionAllowed true))
     :cells   (.setCellSelectionEnabled table true))))

(defn-swing select-single-cell
  "Selects a single cell given as view-coordinates in the given table control"
  [otable row column]
  (assert (keyword-isa? otable table-control))
  (.changeSelection ^JTable (get-control otable) row column false false))

(defn-swing set-value-at-view
  "Sets the value of a cell in the table according to a view position."
  [otable row column contents]
  (assert (keyword-isa? otable table-control))
  (let [columns (get-column-count otable),
        rows    (get-row-count otable)]
    (if (>= column columns)
      (call-hook otable "extend-columns-to" (+ 1 column)))
    (if (>= row rows)
      (call-hook otable "extend-rows-to" (+ 1 row)))
    (.setValueAt ^JTable (get-control otable)
                 (str contents)
                 row
                 column)))

(defn set-value-at-index
  "Sets the value of a cell in the table according to the model
   index."
  [otable row column contents]
  (assert (keyword-isa? otable table-control))
  (set-value-at-view otable
                     (get-index-row otable row)
                     (get-index-column otable column)
                     contents))

(defn update-value-at-index
  "Sets the value of a cell in the table according to the model index,
   if it is different from the current cells value."
  [otable row column contents]
  (assert (keyword-isa? otable table-control))
  (let [current (get-value-at-index otable row column)]
    (if (not= current contents)
      (set-value-at-index otable row column contents))))

(defn-swing paste-from-clipboard
  "Pastes the current system clipboard contents into the table."
  [obj]
  (assert (keyword-isa? obj table-control))
  (let [^JTable control (get-control obj),
        sel-columns (-> control .getSelectedColumns seq),
        sel-rows    (-> control .getSelectedRows seq),
        col-index   (get-column-index-permutator obj),
        row-index   (get-row-index-permutator obj)]
    (if (or (empty? sel-columns) (empty? sel-rows))
      nil
      (let [data   (str (get-clipboard-contents)),
            startx (first sel-columns),
            starty (first sel-rows),
            cells  (vec (map #(vec (split % #"\t"))
                             (split-lines data))),
            lns    (range (count cells))]
        (doseq [r lns]
          (doseq [c (range (count (cells r)))]
            (set-value-at-index obj
                                (row-index (+ starty r))
                                (col-index (+ startx c))
                                ((cells r) c))))))))

(defn-swing copy-to-clipboard
  "Copies the selected cells from the table widget to the system
   clipboard."
  [obj]
  (assert (keyword-isa? obj table-control))
  (let [^JTable control (get-control obj),
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

(defn-swing cut-to-clipboard
  "Copies the selected cells from the table widget to the system
   clipboard and afterwards empties the contents"
  [obj]
  (assert (keyword-isa? obj table-control))
  (let [^JTable control (get-control obj),
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
    (set-clipboard-contents selection)
    (doseq [lines sel-pairs]
      (doseq [p lines]
        (set-value-at-view obj (first p) (second p) "")))))

(defn-swing get-view-coordinates-at-point
  "Returns the current view coordinates as [row column] for the given
   point."
  [otable [x y]]
  (assert (keyword-isa? otable table-control))
  [(.rowAtPoint    ^JTable (get-control otable) (Point. x y)),
   (.columnAtPoint ^JTable (get-control otable) (Point. x y))])

(defn-swing move-column
  "Moves the column at view index old-view to be viewed at view index
   new-view."
  [otable old-view new-view]
  (assert (keyword-isa? otable table-control))
  (let [^JTable control (get-control otable),
        col-count (get-column-count otable)
        col-model (.getColumnModel control)]
    (when (and (<= 0 old-view)
               (<= 0 new-view)
               (< old-view col-count)
               (< new-view col-count))
      (.moveColumn col-model old-view new-view))))

(defn-swing set-column-index-permutator
  "Takes a table object and a column-index permutator and
  rearranges the columns accordingly."
  [otable col-idx]
  (assert (keyword-isa? otable table-control))
  (let [col-count (get-column-count otable)]
    (doseq [col (range col-count)]
      (let [at-view (get-index-column otable (col-idx col))]
        (move-column otable at-view col)))))

(defn-swing view-cell-selected?
  "Returns true if the given cell of the table control in view
   coordinates is selected"
  [obj view-row view-col]
  (assert (keyword-isa? obj table-control))
  (let [^JTable control (get-control obj),
        sel-columns (-> control .getSelectedColumns seq),
        sel-rows    (-> control .getSelectedRows seq),
        sel-pairs   (map (fn [y]
                           (map (fn [x]
                                  (list y x))
                                sel-columns))
                         sel-rows)]
    (boolean (some #(= % (list view-row view-col)) sel-pairs))))

(defn-swing move-row
  "Moves the row at view index old-view to be viewed at view index
   new-view."
  [otable old-view new-view]
  (assert (keyword-isa? otable table-control))
  (let [view-to-index (get-row-index-permutator otable),
        row-count     (get-row-count otable),
        col-indices   (range (get-column-count otable))]
    (when (and (>= new-view 0)
               (< new-view row-count)
               (not= old-view new-view))
      (let [grab-list             (vec (if (< old-view new-view)
                                         (range old-view (+ 1 new-view))
                                         (range new-view (+ 1 old-view)))),

            put-list              (if (< old-view new-view)
                                    (into [new-view] (range old-view new-view))
                                    (conj (vec (range (+ 1 new-view) (+ 1 old-view))) new-view)),

            new-view-to-index-map (reduce! (fn [map i]
                                             (assoc! map (put-list i) (view-to-index (grab-list i))))
                                           (map-by-fn view-to-index (range row-count))
                                           (range (count grab-list))),

            new-index-to-view-map (map-invert new-view-to-index-map)

            new-view-to-index     (fn [x]
                                    (if (and (>= x 0) (< x row-count))
                                      (new-view-to-index-map x)
                                      x)),
            new-index-to-view     (fn [x]
                                    (if (and (>= x 0) (< x row-count))
                                      (new-index-to-view-map x)
                                      x)),
            new-row-permutator    [new-view-to-index new-index-to-view],
            old-table-change-hook (get-hook-function otable "table-changed")]
        (try
          (do
            (set-hook otable "table-changed" (constantly nil))
            (doseq [[r row] (zip put-list
                                 (for [r grab-list]
                                   (doall (for [c col-indices]
                                            (get-value-at-view otable r c))))),
                    [c x]   (zip col-indices row)]
              (set-value-at-view otable r c x))
            (dosync (ref-set (:row-permutator otable) new-row-permutator)))
          (finally
           (set-hook otable "table-changed" old-table-change-hook)))))))

(defn-swing set-row-index-permutator
  "Takes a table object and a row-index permutator and
  rearranges the rows accordingly."
  [otable row-idx]
  (assert (keyword-isa? otable table-control))
  (let [row-count (get-row-count otable)]
    (doseq [row (range row-count)]
      (let [at-view (get-index-row otable (row-idx row))]
        (move-row otable at-view row)))))

(defn- table-change-hook
  "This hook is called whenever a table widget is changed,

   Parameters:
    column   _the column index of the changed area
    first    _the first row *view* of the changed area
    last     _the last row *view* of the changed area
    type     _TableModelEvent/{DELETE,INSERT,UPDATE}"
  [otable column first-row-in-view last-row-in-view type]
  (if (and (= TableModelEvent/UPDATE type)
           (<= 0 (min column first-row-in-view last-row-in-view))
           (= first-row-in-view last-row-in-view))
    (let [first-row  (get-row-index otable first-row-in-view),
          new-value  (get-value-at-index otable first-row column),
          old-value  (call-hook otable "get-cell-value" first-row column)]
      (when (not= old-value new-value)
        (call-hook otable "set-cell-value" first-row column new-value)))))

(defn- make-table-mouse-listener
  [widget]
  (let [ignore-event      (fn [x] nil),
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
                                   (move-row widget start-row end-row)))))]
    (proxy-mouse-listener button-down-event
                          button-up-event
                          ignore-event
                          ignore-event
                          (fn [x]
                            (let [[r c] (get-view-coordinates-at-point widget (:position x)),
                                  r     (get-row-index widget r),
                                  c     (get-column-index widget c),
                                  val   (get-value-at-index widget r c)]
                              (when (and (< 0 r) (< 0 c))
                                (set-value-at-index widget r c (cond (= val "X") ""
                                                                     (= val "") "X"
                                                                     :else val)))))
                          ignore-event
                          drag-motion-event)))

(defn-swing make-table-control
  "Creates a table control in Java."
  []
  (let [model           (DefaultTableModel.),
        table           (JTable. model),
        pane            (JScrollPane. table
                                      JScrollPane/VERTICAL_SCROLLBAR_AS_NEEDED
                                      JScrollPane/HORIZONTAL_SCROLLBAR_AS_NEEDED),

        keystroke-copy  (KeyStroke/getKeyStroke KeyEvent/VK_C
                                                ActionEvent/CTRL_MASK false),
        keystroke-cut   (KeyStroke/getKeyStroke KeyEvent/VK_X
                                                ActionEvent/CTRL_MASK false),
        keystroke-paste (KeyStroke/getKeyStroke KeyEvent/VK_V
                                                ActionEvent/CTRL_MASK false),

        hooks           (:hooks (make-hookable)),
        widget          (table-control. pane table hooks model
                                        (ref [identity identity])),

        cell-editor     (proxy [DefaultCellEditor] [(JTextField.)]
                          (isCellEditable [event]
                            (if (isa? (type event) MouseEvent)
                              (do-swing-return
                               (let [pt     (.getPoint ^MouseEvent event)
                                     col    (.columnAtPoint table pt)
                                     row    (.rowAtPoint table pt)
                                     result (call-hook widget "mouse-click-cell-editable-hook"
                                              row col)]
                                 result))
                              true))),

        cell-renderer   (proxy [DefaultTableCellRenderer] []
                          (getTableCellRendererComponent
                           [jtable value is-selected has-focus row column]
                           (do-swing-return
                            (let [component (proxy-super getTableCellRendererComponent
                                                         jtable value is-selected
                                                         has-focus row column)]
                              (call-hook widget "cell-renderer-hook"
                                component row column is-selected
                                has-focus value))))),

        change-listener (proxy [TableModelListener] []
                          (tableChanged [^TableModelEvent event]
                            (do-swing
                              (let [column (.getColumn event),
                                    first  (.getFirstRow event),
                                    last   (.getLastRow event),
                                    type   (.getType event)]
                                (call-hook widget "table-changed"
                                  column first last type)))))]
    (.addTableModelListener model change-listener)
    (doto table
      (.setTableHeader nil)
      (.setCellEditor cell-editor)
      (.setDefaultEditor java.lang.Object cell-editor)
      (.setDefaultRenderer java.lang.Object cell-renderer))
    (doto widget
      (add-hook "table-changed"        (fn [c f l t]
                                         (table-change-hook widget c f l t)))
      (add-hook "extend-columns-to"    #(set-column-count widget %))
      (add-hook "extend-rows-to"       #(set-row-count widget %))
      (add-hook "get-cell-value"       (fn [x y] ","))
      (add-hook "set-cell-value"       (fn [x y z] "."))
      (add-hook "mouse-click-cell-editable-hook"
                                       (constantly false))
      (add-hook "cell-renderer-hook"   (fn [component view-row view-col is-selected has-focus value]
                                         component))
      (set-resize-mode :off)
      (set-cell-selection-mode :cells)
      (register-keyboard-action copy-to-clipboard    "Copy"  keystroke-copy  :focus)
      (register-keyboard-action cut-to-clipboard     "Cut"   keystroke-cut   :focus)
      (register-keyboard-action paste-from-clipboard "Paste" keystroke-paste :focus)
      (add-control-mouse-listener (make-table-mouse-listener widget)))
    widget))

;;;

nil
