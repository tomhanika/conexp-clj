;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.gui.editors.context-editor
  (:use conexp.base
        conexp.contrib.gui.editors.context-editor.util
        conexp.contrib.gui.editors.context-editor.widgets))

(ns-doc "Provides basic operations to contstruct context editors.")

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
    (let [widget (panels panel)]
      (when widget
        (get-context (get-ectx widget)))))

  nil)

;;;

nil



