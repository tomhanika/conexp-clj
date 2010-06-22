;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; This file has been written by Immanuel Albrecht, with modifications by DB

(ns conexp.contrib.gui.editors.context-editor.context-editor-control
  (:use conexp.fca
        conexp.contrib.gui.util
        conexp.contrib.gui.util.one-to-many
        conexp.contrib.gui.util.hookable
        conexp.contrib.gui.editors.context-editor.util
        conexp.contrib.gui.editors.context-editor.widgets
        conexp.contrib.gui.editors.context-editor.table-control
        conexp.contrib.gui.editors.context-editor.editable-contexts))


;;; Context editor control

(defrecord context-editor-widget [widget table toolbar e-ctx])
(derive ::context-editor-widget :conexp.contrib.gui.editors.context-editor.widgets/widget)

(defmethod get-table ::context-editor-widget
  [widget]
  (:table widget))

(defn get-ectx
  "Returns the editable-context that is currently associated with the
   context-editor-widget."
  [widget]
  (assert (instance? context-editor-widget widget))
  @(:e-ctx widget))

(defmethod get-context ::context-editor-widget
  [widget]
  (get-context @(:e-ctx widget)))

(defn set-ectx
  "Sets the editable-context that is currently associated with the
   context-editor-widget to the second parameter."
  [widget e-ctx]
  (assert (instance? context-editor-widget widget))
  (dosync (ref-set (:e-ctx widget) e-ctx)))

(defn get-selected-objects
  "Returns the set of selected objects in the context-editor-widget."
  [widget]
  (assert (instance? context-editor-widget widget))
  (let [table (get-table widget),
        view  (get-row-index-permutator table),
        sel   (seq (.getSelectedRows (get-control table))),
        ectx  (get-ectx widget),
        names (deref (:obj-rows ectx)),
        fltr  #(filter (fn [x]
                         (when-not (nil? x)
                           x))
                       %)]
    (set (fltr (map #(names (view %)) sel)))))

(defn get-selected-attributes
  "Returns the set of selected attributes in the context-editor-widget."
  [widget]
  (assert (instance? context-editor-widget widget))
  (let [table (get-table widget),
        view  (get-column-index-permutator table),
        sel   (seq (.getSelectedColumns (get-control table))),
        ectx  (get-ectx widget),
        names (deref (:attr-cols ectx)),
        fltr  #(filter (fn [x]
                         (when-not (nil? x)
                           x))
                       %)]
    (set (fltr (map #(names (view %)) sel)))))

(defn add-widget
  "Adds a context-editor-widget to an editable context, and sets the
   editor windows table to represent the new context."
  [e-ctx editor]
  (let [ctx (get-context e-ctx),
        att-cols (:attr-cols e-ctx),
        obj-rows (:obj-rows e-ctx),
        att (attributes ctx),
        obj (objects ctx),
        inc (incidence ctx),
        get-cross (fn [obj att]
                    (if (contains? inc [obj att]) "X" " "))
        table (get-table editor),
        current-ectx (get-ectx editor)]
    (dosync
     (alter (:widgets current-ectx) del editor)
     (set-ectx editor e-ctx)
     (alter (:widgets e-ctx) add editor))
    (set-hook table "cell-value" (fn [_ _ x] x))
    (set-column-count table (+ 1 (count att)))
    (set-row-count table (+ 1 (count obj)))
    (doseq [a att]
      (set-value-at-index table 0 (att-cols a) a))
    (doseq [o obj]
      (set-value-at-index table (obj-rows o) 0 o))
    (doseq [a att o obj]
      (set-value-at-index table (obj-rows o) (att-cols a)
                          (get-cross o a)))
    (set-value-at-index table 0 0 "⇊objects⇊")
    (set-hook table "extend-rows-to"
              #(ectx-extend-rows-hook e-ctx %))
    (set-hook table "extend-columns-to"
              #(ectx-extend-columns-hook e-ctx %))
    (set-hook table "cell-value"
              (fn [r c s]
                (ectx-cell-value-hook e-ctx r c s)))))

(defn set-context-keep-order
  "Sets the bound fca-context of an editable context object to ctx."
  [ectx ctx keep-order]
  (let [new     (make-editable-context ctx keep-order),
        widgets @(:widgets ectx)]
    (dosync
     (ref-set (:widgets ectx) @(:widgets new))
     (ref-set (:context ectx) @(:context new))
     (ref-set (:attr-cols ectx) @(:attr-cols new))
     (ref-set (:obj-rows ectx) @(:obj-rows new)))
    (call-many widgets #(add-widget ectx %))))

(defn set-context
  "Sets the bound fca-context of an editable context object to ctx."
  [ectx ctx]
  (set-context-keep-order ectx ctx (get-order ectx)))

;;;

nil
