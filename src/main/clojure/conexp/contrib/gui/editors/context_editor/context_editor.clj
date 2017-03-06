;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; This file has been written by Immanuel Albrecht, with modifications by DB

(ns conexp.contrib.gui.editors.context-editor.context-editor
  (:use [conexp.base :exclude (select)]
        conexp.fca.contexts
        conexp.contrib.gui.util
        conexp.contrib.gui.editors.context-editor.widgets
        conexp.contrib.gui.editors.context-editor.table-control
        conexp.contrib.gui.editors.context-editor.editable-contexts
        conexp.contrib.gui.editors.context-editor.context-editor-control)
  (:use seesaw.core)
  (:import [javax.swing KeyStroke Box JTable]
           [java.awt.event KeyEvent ActionEvent])
  (:import conexp.contrib.gui.editors.context_editor.context_editor_control.context-editor-widget))


;;; Functions implementing button actions

(declare get-current-second-operand-context)

(defmacro- fn-context-changer
  "Constructs an anonymous function of a widget, defining the names
  widget, ectx, ctx, selected-objs, selected-atts and second-operand."
  [& body]
  `(fn [~'widget]
     (let [~'ectx (get-ectx ~'widget),
           ~'ctx  (get-context ~'ectx),
           ~'selected-objs (get-selected-objects ~'widget),
           ~'selected-atts (get-selected-attributes ~'widget),
           ~'second-operand (get-current-second-operand-context)]
       (with-swing-error-msg nil "Error"
         ~@body))))

(defmacro- defn-context-changer
  "Defines a function with given name, getting a widget and returning
  a new context. Defines the same names as fn-context-changer."
  [name doc & body]
  `(defn- ~name ~doc [x#]
     ((fn-context-changer ~@body) x#)))

(defmacro- cc-1 [fn]
  `(fn-context-changer (~fn ~'ctx)))

(defmacro- cc-2 [fn]
  `(fn-context-changer (when-not ~'second-operand
                         (illegal-argument "Operation " '~fn " needs a second operand!"))
                       (~fn ~'ctx ~'second-operand)))

(defn-context-changer add-new-attribute
  "Adds a new attribute."
  (make-context (objects ctx)
                (conj (attributes ctx)
                      (req-unique-string (attributes ctx) "new attribute"))
                (incidence-relation ctx)))

(defn-context-changer add-new-object
  "Adds a new object."
  (make-context (conj (objects ctx)
                      (req-unique-string (objects ctx) "new object"))
                (attributes ctx)
                (incidence-relation ctx)))

(defn-context-changer keep-attributes
  "Cut out all but the given attributes from the context."
  (make-context (objects ctx)
                (intersection (set selected-atts) (attributes ctx))
                (incidence-relation ctx)))

(defn-context-changer keep-objects
  "Cut out all but the given objects from the context."
  (make-context (intersection (set selected-objs) (objects ctx))
                (attributes ctx)
                (incidence-relation ctx)))

(defn-context-changer keep-objects-attributes
  "Cut out the given objects and attributes from the context."
  (make-context (intersection (set selected-objs) (objects ctx))
                (intersection (set selected-atts) (attributes ctx))
                (incidence-relation ctx)))

(defn-context-changer cut-attributes
  "Cut out the given attributes from the context."
  (make-context (objects ctx)
                (remove (set selected-atts)
                        (attributes ctx))
                (incidence-relation ctx)))

(defn-context-changer cut-objects
  "Cut out the given objects from the context."
  (make-context (remove (set selected-objs)
                        (objects ctx))
                (attributes ctx)
                (incidence-relation ctx)))

(defn-context-changer cut-objects-attributes
  "Cut out the given objects and attributes from the context"
  (make-context (remove (set selected-objs) (objects ctx))
                (remove (set selected-atts) (attributes ctx))
                (incidence-relation ctx)))

;;; Helper for filling with X's

(defn-swing fill-selection-with-X
  "Fills the selected cells from the table widget with 'X's"
  [obj]
  (assert (keyword-isa? obj conexp.contrib.gui.editors.context_editor.table_control.table-control))
  (let [^JTable control (get-control obj),
        sel-columns (-> control .getSelectedColumns seq),
        sel-rows    (-> control .getSelectedRows seq),
        sel-pairs   (map (fn [y]
                           (map (fn [x]
                                  (list y x)) sel-columns))
                         sel-rows)]
    (doseq [lines sel-pairs]
      (doseq [p lines]
        (set-value-at-view obj (first p) (second p) "X")))))


;;; Creating context editor widgets

(let [second-operand (atom nil)]

  (def- toolbar-icons
    [["copy.png" "C"
      "Copy the selected cells to clipboard",
      #(copy-to-clipboard (get-table %)) :no-return]
     ["cut.png" "X",
      "Cut the selected cells to clipboard",
      #(cut-to-clipboard (get-table %)) :no-return]
     ["paste.png" "P",
      "Paste the clipboard to the selected cell and its down-right neighbors",
      #(paste-from-clipboard (get-table %)) :no-return]
     ["second-op.png" "M+",
      "Use a copy of this context as second operand",
      #(reset! second-operand (get-context (get-ectx %))) :no-return]
     ["add-attribute.png" "+A",
      "Adds a new attribute column to the context",
      add-new-attribute]
     ["add-object.png" "+O",
      "Adds a new object row to the context",
      add-new-object]
     :separator
     ["keep-attribute.png" "8<A",
      "Remove all non-selected attribute columns from the context",
      keep-attributes]
     ["keep-object.png" "8<O",
      "Remove all non-selected object rows from the context",
      keep-objects]
     ["keep-both.png" "8<OA",
      "Remove all non-selected rows and columns from the context",
      keep-objects-attributes]
     :separator
     ["cut-attribute.png" "-A",
      "Remove all selected attribute columns from the context",
      cut-attributes]
     ["cut-object.png" "-O",
      "Remove all selected object rows from the context",
      cut-objects]
     ["cut-both.png" "-OA",
      "Remove all selected rows and columns from the context",
      cut-objects-attributes]
     ["clarify-attribute.png" "cA",
      "Clarify the attribute columns of the context",
      (cc-1 clarify-attributes)]
     ["clarify-object.png" "cO",
      "Clarify the object rows of the context",
      (cc-1 clarify-objects)]
     ["clarify-both.png" "cOA",
      "Clarify both objects and attributes of the context",
      (cc-1 clarify-context)]
     :separator
     ["reduce-attribute.png" "rA",
      "Reduce the attribute columns of the context",
      (cc-1 reduce-attributes)]
     ["reduce-object.png" "rO",
      "Reduce the object rows of the context",
      (cc-1 reduce-objects)]
     ["reduce-both.png" "rOA",
      "Reduce both objects and attributes of the context",
      (cc-1 reduce-context)]
     ["transitive-closure.png" "trans",
      "Apply transitive closure to the context",
      (cc-1 context-transitive-closure)]
     :separator
     ["dual-context.png" "dual",
      "Flip objects and attributes",
      (cc-1 dual-context)]
     ["inverse-context.png" "inv",
      "Flip all crosses",
      (cc-1 invert-context)]
     ["sum.png" "sum",
      "Calculate the context sum of this context with the second operand context"
      (cc-2 context-sum)]
     ["product.png" "prod",
      "Calculate the context product of this context with the second operand context"
      (cc-2 context-product)]
     ["semi-product.png" "semi",
      "Calculate the context semiproduct of this context with the second operand context"
      (cc-2 context-semiproduct)]
     ["xia-product.png" "Xia",
      "Calculate the context Xia product of this context with the second operand context"
      (cc-2 context-xia-product)]
     :separator
     ["union.png" "union",
      "Calculate the context union of this context with the second operand context"
      (cc-2 context-union)]
     ["intersection.png" "inter",
      "Calculate the context intersection of this context with the second operand context"
      (cc-2 context-intersection)]
     :separator
     ["composition.png" "comp",
      "Calculate the context composition of this context with the second operand context"
      (cc-2 context-composition)]
     ["apposition.png" "ap",
      "Calculate the context apposition of this context with the second operand context"
      (cc-2 context-apposition)]
     ["subposition.png" "sub",
      "Calculate the context subposition of this context with the second operand context"
      (cc-2 context-subposition)]])

  (defn get-current-second-operand-context
    "Returns the current second operand."
    []
    @second-operand)

  (defn-swing make-context-editor-widget
    "Creates a control for editing contexts, starting with the initial
    context ctx."
    [ctx]
    (let [table          (doto (make-table-control)
                           (set-row-count 1)
                           (set-column-count 1)),
          ectx           (ref (make-editable-context ctx)),
          bar            (toolbar :orientation :horizontal
                                  :floatable? false)
          root           (top-bottom-split bar
                                           (get-widget table)
                                           :divider-location 40)
          e-ctx          @ectx,
          widget         (context-editor-widget. root table ectx),
          keystroke-fill (KeyStroke/getKeyStroke KeyEvent/VK_SPACE
                                                 ActionEvent/CTRL_MASK false)]
      (register-keyboard-action table fill-selection-with-X "Fill-X" keystroke-fill :focus)
      (add-widget e-ctx widget)
      (doseq [icon toolbar-icons]
        (if (= icon :separator)
          (.add bar (Box/createHorizontalStrut 3))
          (let [[path alt tip fun no-return] icon]
            (.add bar (button :icon (get-image-icon-or-string (str "context-editor/" path)
                                                              alt)
                              :tip tip
                              :listen [:action (if no-return
                                                 (fn [_] (fun widget))
                                                 (fn [_] (set-context @ectx (fun widget))))])))))
      widget))

  nil)

;;;

(let [panels (atom {})]

  (defn make-context-editor
    "Creates a context editor object for a given context ctx and
    returns its root panel."
    [ctx]
    (let [widget (make-context-editor-widget ctx),
          panel  (get-widget widget)]
      (swap! panels assoc panel widget)
      panel))

  (defn clone-context-view-from-panel
    "Creates another view from the view represented by the given panel,
     and returns the root panel of the new view."
    [panel]
    (let [old-widget (@panels panel),
          new-widget (make-context-editor-widget (make-context #{} #{} [])),
          new-panel  (get-widget new-widget),
          old-ectx   (get-ectx old-widget),
          old-table  (get-table old-widget),
          old-col-permutator (get-column-index-permutator old-table),
          old-row-permutator (get-row-index-permutator old-table),
          new-table  (get-table new-widget)]
      (swap! panels assoc new-panel new-widget)
      (add-widget old-ectx new-widget)
      (set-column-index-permutator new-table old-col-permutator)
      (set-row-index-permutator new-table old-row-permutator)
      new-panel))

  (defn get-context-from-panel
    "Returns the context that is currently associated with the context
    editor widget represented by the given panel."
    [panel]
    (let [widget (@panels panel)]
      (if-not widget
        (unsupported-operation "There is no formal context in this panel")
        (get-context (get-ectx widget)))))

  (defn set-context-in-panel
    "Sets the context of the given panel to the given context."
    [panel context]
    (if-let [widget (@panels panel)]
      (set-context (get-ectx widget) context)
      (illegal-argument "Given panel does not contain a context.")))

  nil)

;;;

nil
