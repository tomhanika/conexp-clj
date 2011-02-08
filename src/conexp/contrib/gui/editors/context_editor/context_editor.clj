;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; This file has been written by Immanuel Albrecht, with modifications by DB

(ns conexp.contrib.gui.editors.context-editor.context-editor
  (:use conexp.base
        conexp.fca
        conexp.contrib.gui.util
        conexp.contrib.gui.editors.context-editor.widgets
        conexp.contrib.gui.editors.context-editor.table-control
        conexp.contrib.gui.editors.context-editor.editable-contexts
        conexp.contrib.gui.editors.context-editor.context-editor-control)
  (:import [javax.swing JRootPane KeyStroke Box JFrame]
           [java.awt BorderLayout]
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
                (incidence ctx)))

(defn-context-changer add-new-object
  "Adds a new object."
  (make-context (conj (objects ctx)
                      (req-unique-string (objects ctx) "new object"))
                (attributes ctx)
                (incidence ctx)))

(defn-context-changer keep-attributes
  "Cut out all but the given attributes from the context."
  (make-context (objects ctx)
                (intersection (set selected-atts) (attributes ctx))
                (incidence ctx)))

(defn-context-changer keep-objects
  "Cut out all but the given objects from the context."
  (make-context (intersection (set selected-objs) (objects ctx))
                (attributes ctx)
                (incidence ctx)))

(defn-context-changer keep-objects-attributes
  "Cut out the given objects and attributes from the context."
  (make-context (intersection (set selected-objs) (objects ctx))
                (intersection (set selected-atts) (attributes ctx))
                (incidence ctx)))

(defn-context-changer cut-attributes
  "Cut out the given attributes from the context."
  (make-context (objects ctx)
                (remove (set selected-atts)
                        (attributes ctx))
                (incidence ctx)))

(defn-context-changer cut-objects
  "Cut out the given objects from the context."
  (make-context (remove (set selected-objs)
                        (objects ctx))
                (attributes ctx)
                (incidence ctx)))

(defn-context-changer cut-objects-attributes
  "Cut out the given objects and attributes from the context"
  (make-context (remove (set selected-objs) (objects ctx))
                (remove (set selected-atts) (attributes ctx))
                (incidence ctx)))

;;; Helper for filling with X's

(defn-swing fill-selection-with-X
  "Fills the selected cells from the table widget with 'X's"
  [obj]
  (assert (keyword-isa? obj conexp.contrib.gui.editors.context_editor.table_control.table-control))
  (let [control     (get-control obj),
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

(let [ second-operand (atom nil)
       copy-img (get-image-icon-or-string "context-editor/copy.png" "C")
       paste-img (get-image-icon-or-string "context-editor/paste.png" "P")
       cut-img (get-image-icon-or-string "context-editor/cut.png" "X")
       second-op-img (get-image-icon-or-string "context-editor/second-op.png" "M+")
       add-attribute-img (get-image-icon-or-string "context-editor/add-attribute.png" "+A")
       add-object-img (get-image-icon-or-string "context-editor/add-object.png" "+O")
       keep-attribute-img (get-image-icon-or-string "context-editor/keep-attribute.png" "8<A")
       keep-object-img (get-image-icon-or-string "context-editor/keep-object.png" "8<O")
       keep-both-img (get-image-icon-or-string "context-editor/keep-both.png" "8<OA")
       cut-attribute-img (get-image-icon-or-string "context-editor/cut-attribute.png" "-A")
       cut-object-img (get-image-icon-or-string "context-editor/cut-object.png" "-O")
       cut-both-img (get-image-icon-or-string "context-editor/cut-both.png" "-OA")
       clarify-attribute-img (get-image-icon-or-string "context-editor/clarify-attribute.png"
                                "cA")
       clarify-object-img (get-image-icon-or-string "context-editor/clarify-object.png" "cO")
       clarify-both-img (get-image-icon-or-string "context-editor/clarify-both.png" "cOA")
       reduce-attribute-img (get-image-icon-or-string "context-editor/reduce-attribute.png"
                                "rA")
       reduce-object-img (get-image-icon-or-string "context-editor/reduce-object.png" "rO")
       reduce-both-img (get-image-icon-or-string "context-editor/reduce-both.png" "rOA")
       transitiy-closure-img (get-image-icon-or-string "context-editor/transitive-closure.png"
                               "trans")
       dual-context-img (get-image-icon-or-string "context-editor/dual-context.png" "dual")
       inverse-context-img (get-image-icon-or-string "context-editor/inverse-context.png" "inv")
       sum-img (get-image-icon-or-string "context-editor/sum.png" "sum")
       product-img (get-image-icon-or-string "context-editor/product.png" "prod")
       semi-product-img (get-image-icon-or-string "context-editor/semi-product.png" "semi")
       xia-product-img (get-image-icon-or-string "context-editor/xia-product.png" "Xia")
       union-img (get-image-icon-or-string "context-editor/union.png" "union")
       intersection-img (get-image-icon-or-string "context-editor/intersection.png" "inter")

       apposition-img (get-image-icon-or-string "context-editor/apposition.png" "ap")
       composition-img (get-image-icon-or-string "context-editor/composition.png" "comp")
       subposition-img (get-image-icon-or-string "context-editor/subposition.png" "sub")]


  (defn get-current-second-operand-context
    "Returns the current second operand."
    []
    @second-operand)

  (defn-swing make-context-editor-widget
    "Creates a control for editing contexts, starting with the initial
    context ctx."
    [ctx]
    (let [table   (doto (make-table-control)
                    (set-row-count 1)
                    (set-column-count 1)),
          ectx    (ref (make-editable-context ctx)),
          toolbar (make-toolbar-control :horiz)
          root    (make-split-pane :vert toolbar table)
          e-ctx   @ectx,
          widget  (context-editor-widget. (get-widget root) table toolbar ectx),
          keystroke-fill  (KeyStroke/getKeyStroke KeyEvent/VK_SPACE
                                                ActionEvent/CTRL_MASK false),
          add-button- (fn [toolbar text f & args]
                        (add-button toolbar
                                    (doto (make-tooltip-button "no-tip" text)
                                      (set-handler #(set-context @ectx (f widget))))))

          add-separator (fn [toolbar]
                          (add-button toolbar (Box/createVerticalStrut 3)))
          
          add-button-box (fn [toolbar & buttonlist]
                           (let [box (Box/createHorizontalBox)]
                             (doseq [button buttonlist]
                               (if (contains? button :spacer)
                                 (.add box (Box/createHorizontalStrut (:spacer button)))
                                 (let [ bctrl (make-tooltip-button (:tip button) (:name button))
                                        handler #(do
                                                   (if (contains? button :no-return)
                                                     ((:no-return button) widget))
                                                   (if (contains? button :f)
                                                     (set-context @ectx ((:f button) widget))))]
                                   (set-handler bctrl handler)
                                   ;;(add-button toolbar bctrl)))
                                   (.add box (get-widget bctrl)))))
                             (.add (get-control toolbar) box)))]
      (register-keyboard-action table fill-selection-with-X "Fill-X" keystroke-fill :focus)
      (add-widget e-ctx widget)
      (doto toolbar
        (set-floatable false)
        (add-button-box  { :name copy-img,
                           :tip  "Copy the selected cells to clipboard",
                           :no-return #(copy-to-clipboard (get-table %)) }
                         { :name cut-img,
                           :tip  "Cut the selected cells to clipboard",
                           :no-return #(cut-to-clipboard (get-table %)) }
                         { :name paste-img,
                           :tip  "Paste the clipboard to the selected cell and its down-right neighbors",
                           :no-return #(paste-from-clipboard (get-table %)) }
                         { :name second-op-img,
                           :tip  "Use a copy of this context as second operand",
                           :no-return #(reset! second-operand (get-context (get-ectx %))) }
                        )
        (add-button-box {:name add-attribute-img,
                         :tip "Adds a new attribute column to the context",
                         :f add-new-attribute}
                        {:name add-object-img,
                         :tip "Adds a new object row to the context",
                         :f add-new-object}
                        {:spacer 3}
                        {:name keep-attribute-img,
                         :tip "Remove all non-selected attribute columns from the context",
                         :f keep-attributes}
                        {:name keep-object-img,
                         :tip "Remove all non-selected object rows from the context",
                         :f keep-objects}
                        {:name keep-both-img,
                         :tip "Remove all non-selected rows and columns from the context",
                         :f keep-objects-attributes}
                        {:spacer 3}
                        {:name cut-attribute-img,
                         :tip "Remove all selected attribute columns from the context",
                         :f cut-attributes}
                        {:name cut-object-img,
                         :tip "Remove all selected object rows from the context",
                         :f cut-objects}
                        {:name cut-both-img,
                         :tip "Remove all selected rows and columns from the context",
                         :f cut-objects-attributes})
        (add-button-box {:name clarify-attribute-img,
                         :tip "Clarify the attribute columns of the context",
                         :f (cc-1 clarify-attributes)}
                        {:name clarify-object-img,
                         :tip "Clarify the object rows of the context",
                         :f (cc-1 clarify-objects)}
                        {:name clarify-both-img,
                         :tip "Clarify both objects and attributes of the context",
                         :f (cc-1 clarify-context)}
                        {:spacer 3}
                        {:name reduce-attribute-img,
                         :tip "Reduce the attribute columns of the context",
                         :f (cc-1 reduce-attributes)}
                        {:name reduce-object-img,
                         :tip "Reduce the object rows of the context",
                         :f (cc-1 reduce-objects)}
                        {:name reduce-both-img,
                         :tip "Reduce both objects and attributes of the context",
                         :f (cc-1 reduce-context)})
        (add-button-box {:name transitiy-closure-img,
                         :tip "Apply transitive closure to the context",
                         :f (cc-1 context-transitive-closure)}
                        {:spacer 3}
                        {:name dual-context-img,
                         :tip "Flip objects and attributes",
                         :f (cc-1 dual-context)}
                        {:name inverse-context-img,
                         :tip "Flip all crosses",
                         :f (cc-1 invert-context)})
        (add-button-box {:name sum-img,
                         :tip "Calculate the context sum of this context with the second operand context"
                         :f (cc-2 context-sum)}
                        {:name product-img,
                         :tip "Calculate the context product of this context with the second operand context"
                         :f (cc-2 context-product)}
                        {:name semi-product-img,
                         :tip "Calculate the context semiproduct of this context with the second operand context"
                         :f (cc-2 context-semiproduct)}
                        {:name xia-product-img,
                         :tip "Calculate the context Xia product of this context with the second operand context"
                         :f (cc-2 context-xia-product)}
                        {:spacer 3}
                        {:name union-img,
                         :tip "Calculate the context union of this context with the second operand context"
                         :f (cc-2 context-union)}
                        {:name intersection-img,
                         :tip "Calculate the context intersection of this context with the second operand context"
                         :f (cc-2 context-intersection)}
                        {:spacer 3}
                        {:name composition-img,
                         :tip "Calculate the context composition of this context with the second operand context"
                         :f (cc-2 context-composition)}
                        {:name apposition-img,
                         :tip "Calculate the context apposition of this context with the second operand context"
                         :f (cc-2 context-apposition)}
                        {:name subposition-img,
                         :tip "Calculate the context subposition of this context with the second operand context"
                         :f (cc-2 context-subposition)}))
      (set-divider-location root 86)
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
      (when widget
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
