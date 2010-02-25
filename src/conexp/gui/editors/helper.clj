;
; This file contains expressions to be run in clojure via C-x C-e from emacs:)
; & for copy & paste into the interactive REPL
;

(require 'conexp)

(conexp/gui)

(ns conexp.gui.editors.contexts)

(ns conexp.gui.editors.util)

(conexp.gui.editors.contexts/add-to-workspace "test" 0)
(conexp.gui.editors.contexts/add-to-workspace "test-1" 0)
(conexp.gui.editors.contexts/add-to-workspace "test-3" 0)
(conexp.gui.editors.contexts/add-to-workspace "test31" 0)
(conexp.gui.editors.contexts/add-to-workspace "test-21" 0)
(conexp.gui.editors.contexts/add-to-workspace "test-31" 0)
(conexp.gui.editors.contexts/update-workspace-tree)
