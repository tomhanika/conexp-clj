;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.gui.editors.code
  (:use [conexp.base :exclude (select)]
        conexp.contrib.gui.plugins.base
        conexp.contrib.gui.util
        conexp.contrib.gui.repl)
  (:use seesaw.core))

;;;

(declare load-code-editor unload-code-editor)

(define-plugin code-editor
  "Code editor plugin."
  :load-hook #(load-code-editor %)
  :unload-hook #(unload-code-editor %))

;;; Menu

(defn- get-file-and-go
  "Asks the user for a file and loads it into the repl of the given
  frame."
  [frame]
  (let [repl-process (get-repl-thread frame)]
    (when-not (and repl-process (repl-alive? repl-process))
      (illegal-state "There is no REPL running, cannot load file."))
    (when-let [file (choose-open-file frame ["clojure files" "clj"])]
      (repl-in repl-process
               (str "(do (load-file \"" (.getAbsolutePath ^java.io.File file) "\") ")))))

(defn- code-menu
  "Returns the menu for the code editor"
  [frame]
  (menu :text "Code",
        :items [(menu-item :text "Load into REPL",
                           :listen [:action (fn [_]
                                              (with-swing-error-msg frame "Error"
                                                (get-file-and-go frame)))])]))

;;;

(defn- load-code-editor
  "Loads the code editor plugin."
  [frame]
  (add-menus frame [(code-menu frame)]))

(defn- unload-code-editor
  "Unloads the code editor plugin."
  [frame]
  nil)                                  ; remove menu?

;;;

nil
