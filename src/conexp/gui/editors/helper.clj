;
; This file contains expressions to be run in clojure via C-x C-e from emacs:)
; & for copy & paste into the interactive REPL
;

(require 'conexp)

(conexp/gui)

(ns conexp.gui.editors.contexts)

(ns conexp.gui.editors.util)

(add-to-workspace "test" 0)
(add-to-workspace "test-1" 0)
