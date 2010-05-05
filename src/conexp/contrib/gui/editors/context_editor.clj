;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.gui.editors.context-editor
  (:use 
    conexp.contrib.gui.editors.context-editor.util
    conexp.contrib.gui.editors.util
    conexp.util))

(let [ panels (ref {}) ]

  (defn make-context-editor
    "Creates a context editor object for a given context and returns its root
   panel.

  Parameters:
    ctx  _ context"
    [ctx]
    (let [ ectx (make-editable-context ctx)
           widget (make-context-editor-widget) 
           panel (get-widget widget)]
      (add-widget ectx widget)
      (dosync-wait (commute panels conj {panel [widget ectx]}))
      panel))

  (defn get-context-from-panel
    "Returns the context that is currently associated with the context editor
   widget represented by the given panel.

  Parameters:
    panel  _ context editor panel"
    [panel]
    (get-context (second (panels panel))))

  nil)


