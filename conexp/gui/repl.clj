(ns conexp.gui.repl
  (:import [javax.swing.text PlainDocument])
  (:use conexp.gui.util)
  (:gen-class
   :name "conexp.gui.repl.ClojureREPL"
   :extends javax.swing.text.PlainDocument
   :init init
   :post-init post-init
   :prefix "conexp-repl-"
   :constructors { [] [] }
   :state state
   :methods [ [ insertResult [ String ] Integer ] ]
   :exposes-methods { remove removeSuper, insertString insertStringSuper }))

(defn own-repl [in out]
  (binding [*in* in
	    *out* out]
    (while true
      (print (str *ns* "=> "))
      (flush)
      (println (eval (read))))))

(defn conexp-repl-init []
  [ [] (ref {:last-pos 0}) ])

(defn conexp-repl-post-init [this]
  (.insertStringSuper this (.getLength this) (str *ns* "=> ") nil)
  (dosync
   (alter (.state this) assoc :last-pos (.getLength this))))

(defn conexp-repl-remove [this off len]
  (print (@(.state this) :last-pos))
  (if (>= (- off len -1) (@(.state this) :last-pos))
    (. this removeSuper off len)))

(defn conexp-repl-insertResult [this result]
  (.insertStringSuper this (.getLength this) result nil)
  (.insertStringSuper this (.getLength this) "\n" nil)
  (.insertStringSuper this (.getLength this) (str *ns* "=> ") nil)
  (dosync 
   (alter (.state this) assoc :last-pos (. this getLength)))
  (@(.state this) :last-pos))