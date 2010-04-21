;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;;; This file is for specifically fixing a new bug with java event callbacks
;;; and undefined variables.

(ns conexp.ise-bug
  (:use conexp.base
    conexp.contrib.gui.util
    conexp.contrib.gui.editors.context-editor.util
    conexp.contrib.gui.editors.util
    conexp.util.multimethods
    conexp.util.hookable
    conexp.fca.contexts))

(def ctx (rand-context #{"a" "b" "c" "d" "e" "f" "g"} #{1 2 3 4 5 6} 0.4))

(def ectx (make-editable-context ctx))

(let [ widget (make-context-editor-widget) ]
  (add-widget ectx widget)
  (def w widget)
  (def c (get-control widget))
  (defn fire-test [] (show-in-frame (get-widget widget))))

(let [table (make-table-control)]
  (set-row-count table 10)
  (set-column-count table 10)
  (def tbl table))

(defn tbl-test [] (show-in-frame (get-widget tbl)))

(defn bing [] (message-box "BING!"))

(set-hook tbl "table-changed" (fn [_ _ _ _] (bing)))

nil
