(ns conexp.gui.editors.contexts
  (:use conexp.gui.plugins.base
        conexp.gui.util
        clojure.contrib.swing-utils
    ))

(def context-data (ref nil))
(def context-pane (ref nil))

(defn plug-load-hook
  "Loads the context-editor plugin.
   Parameters:
     frame    _frame that shall contain the user interface
  "
   [frame]
   (with-swing-threads
      
     (let [pane (javax.swing.JRootPane.)]
      (do
       (add-tab-with-name-icon-tooltip frame pane 
         "Contexts" nil "View and edit contexts")
       (dosync (ref-set context-pane pane))
     )))
    
    )

(defn plug-unload-hook
  "Unloads the context-editor plugin.
   Parameters:
     frame    _frame that contains the user interface
  "
     [frame]
     nil
   )

(define-plugin context-editor
  "Context editor plugin."
  :load-hook plug-load-hook,
  :unload-hook plug-unload-hook)

nil
