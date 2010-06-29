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
  (:import [javax.swing JRootPane]
           [java.awt BorderLayout])
  (:import conexp.contrib.gui.editors.context-editor.context-editor-control.context-editor-widget))


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


;;; Creating context editor widgets

(let [second-operand (atom nil)]

  (defn get-current-second-operand-context
    "Returns the current second operand."
    []
    @second-operand)

  (defn-swing make-context-editor-widget
    "Creates a control for editing contexts, starting with the initial
    context ctx."
    [ctx]
    (let [root    (JRootPane.),
          table   (doto (make-table-control)
                    (set-row-count 1)
                    (set-column-count 1)),
          ectx    (ref (make-editable-context ctx)),
          toolbar (make-toolbar-control :vert)
          e-ctx   @ectx,
          widget  (context-editor-widget. root table toolbar ectx),

          add-button- (fn [toolbar text f & args]
                        (add-button toolbar
                                    (doto (make-button text)
                                      (set-handler #(set-context @ectx (f widget))))))]
      (.. root getContentPane (add (get-widget toolbar) BorderLayout/LINE_START))
      (.. root getContentPane (add (get-widget table)))
      (add-widget e-ctx widget)
      (doto toolbar
        (set-floatable false)
        (add-button- "Copy"  #(do (copy-to-clipboard (get-table %))
                                  (get-context (get-ectx %))))
        (add-button- "Paste" #(do (paste-from-clipboard (get-table %))
                                  (get-context (get-ectx %))))
        (add-button- "Second Operand"
                     #(do (reset! second-operand (get-context (get-ectx %)))
                          (get-context (get-ectx %))))
        (add-separator)
        (add-button- "New attribute" add-new-attribute)
        (add-button- "New object"    add-new-object)
        (add-separator)
        (add-button- "Keep attributes" keep-attributes)
        (add-button- "Keep objects"    keep-objects)
        (add-button- "Keep both"       keep-objects-attributes)
        (add-separator)
        (add-button- "Cut attributes" cut-attributes)
        (add-button- "Cut objects"    cut-objects)
        (add-button- "Cut both"       cut-objects-attributes)
        (add-separator)
        (add-button- "Clarify attributes" (cc-1 clarify-attributes))
        (add-button- "Clarify objects"    (cc-1 clarify-objects))
        (add-button- "Clarify context"    (cc-1 clarify-context))
        (add-separator)
        (add-button- "Reduce attributes"  (cc-1 reduce-context-attributes))
        (add-button- "Reduce objects"     (cc-1 reduce-context-objects))
        (add-button- "Reduce context"     (cc-1 reduce-context))
        (add-separator)
        (add-button- "Transitive closure" (cc-1 context-transitive-closure))
        (add-separator)
        (add-button- "Dual context"    (cc-1 dual-context)) ;todo: keep order!
        (add-button- "Inverse context" (cc-1 invert-context))
        (add-separator)
        (add-button- "Context sum"          (cc-2 context-sum))
        (add-button- "Context product"      (cc-2 context-product))
        (add-button- "Context semiproduct"  (cc-2 context-semiproduct))
        (add-button- "Context Xia product"  (cc-2 context-xia-product))
        (add-button- "Context union"        (cc-2 context-union))
        (add-button- "Context intersection" (cc-2 context-intersection))
        (add-button- "Context composition"  (cc-2 context-composition))
        (add-button- "Context apposition"   (cc-2 context-apposition))
        (add-button- "Context-subposition"  (cc-2 context-subposition)))
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

  (defn get-context-from-panel
    "Returns the context that is currently associated with the context
    editor widget represented by the given panel."
    [panel]
    (let [widget (@panels panel)]
      (when widget
        (get-context (get-ectx widget)))))

  nil)

;;;

nil
