(ns conexp.gui.repl
  (:import [javax.swing.text PlainDocument]
	   [java.io PushbackReader StringReader]
	   [javax.swing KeyStroke AbstractAction JTextArea])
  (:use conexp.gui.util
	[clojure.main :only (skip-whitespace)]
	[clojure.contrib.pprint :only (write)]
	[conexp.gui.enclojure-repl :only (create-clojure-repl)])
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

(def *conexp-clojure-repl* (create-clojure-repl))
(defn repl-in [string]
  ((:repl-fn *conexp-clojure-repl*) (str string "\n")))
(defn repl-out []
  (let [result ((:result-fn *conexp-clojure-repl*))]
    result))
(defn interrupt-repl []
  (.interrupt (:repl-thread *conexp-clojure-repl*)))

(defn conexp-repl-init []
  [ [] (ref {:last-pos 0}) ])

(defn conexp-repl-post-init [this]
  (.start (Thread. 
	   (fn []
	     (while true
	       (let [result (repl-out)]
		 (invoke-later #(. this insertResult result))))))))

(defn conexp-repl-remove [this off len]
  (if (>= (- off len -1) (@(.state this) :last-pos))
    (. this removeSuper off len)))

(defn conexp-repl-insertResult [this result]
  (println "Inserting: " result)
  (.insertStringSuper this (.getLength this) result nil)
  (dosync 
   (alter (.state this) assoc :last-pos (. this getLength)))
  (@(.state this) :last-pos))

(defn balanced? 
  ([string]
     (balanced? string 0))
  ([string paran-count]
     (cond
       (> 0 paran-count)
       false
       (empty? string)
       (= 0 paran-count)
       :else
       (recur (rest string)
	      (cond
		(= \( (first string)) (inc paran-count)
		(= \) (first string)) (dec paran-count)
		:else paran-count)))))

(defn conexp-repl-insertString [this off string attr-set]
  (let [last-pos (@(.state this) :last-pos)]
    (when (>= off last-pos)
      (.insertStringSuper this off string attr-set)
      (if (and (= string "\n") (= off (- (.getLength this) 1)))
	(let [input (.getText this (- last-pos 1) (- (.getLength this) last-pos))]
	  (print (str ":::" input ":::"))
	  (println input)
	  (if (balanced? input)
	    (repl-in input)))))))

(defn make-clojure-repl []
  (let [rpl (JTextArea. (conexp.gui.repl.ClojureREPL.))]
    (.. rpl getInputMap (put (KeyStroke/getKeyStroke "control C") "interrupt"))
    (.. rpl getActionMap (put "interrupt" (proxy [AbstractAction] []
					    (actionPerformed [_]
					      (interrupt-repl)))))
    rpl))