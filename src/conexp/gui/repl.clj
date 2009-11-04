(ns conexp.gui.repl
  (:import [javax.swing.text PlainDocument]
	   [java.io PushbackReader StringReader PipedWriter PipedReader 
	            PrintWriter CharArrayWriter]
	   [javax.swing KeyStroke AbstractAction JTextArea])
  (:use conexp.gui.util
	[clojure.main :only (with-bindings)]
	[clojure.contrib.pprint :only (write)])
  (:gen-class
   :name "conexp.gui.repl.ClojureREPL"
   :extends javax.swing.text.PlainDocument
   :init init
   :post-init post-init
   :prefix "conexp-repl-"
   :constructors { [] [] }
   :state state
   :methods [ [ insertResult [ String ] Integer ]
	      [ lastPosition [ ]        Integer ]
	      [ outputThread [ ]        Object  ]
	      [ replThread   [ ]        Object  ] ]
   :exposes-methods { remove       removeSuper,
		      insertString insertStringSuper }))

;;; REPL

(def *print-stack-trace-on-error* false)

(defn eof-ex?
  "Returns true iff given throwable is an \"EOF while reading\" or \"Write 
  end dead\" exception not thrown from the repl." ; hopefully
  [throwable]
  (and (not (instance? clojure.lang.Compiler$CompilerException throwable))
       (.getMessage throwable)
       (or (re-matches #".*EOF while reading.*" (.getMessage throwable))
	   (re-matches #".*Write end dead.*" (.getMessage throwable)))))

(defn create-clojure-repl []
  "This function creates an instance of clojure repl using piped in and out.
   It returns a map of two functions repl-fn and result-fn - first function
   can be called with a valid clojure expression and the results are read using
   the result-fn.

   Based on org.enclojure.repl.main/create-clojure-repl
 
   Copyright (c) ThorTech, L.L.C.. All rights reserved.
   The use and distribution terms for this software are covered by the
   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
   which can be found in the file epl-v10.html at the root of this distribution.
   By using this software in any fashion, you are agreeing to be bound by
   the terms of this license.
   You must not remove this notice, or any other, from this software.

   Author: Eric Thorsen, Narayan Singhal"
  (let [cmd-wtr (PipedWriter.)
        result-rdr (PipedReader.)
        piped-in (clojure.lang.LineNumberingPushbackReader. (PipedReader. cmd-wtr))
        piped-out (PrintWriter. (PipedWriter. result-rdr))
        repl-thread-fn #(with-bindings
			  (binding [*print-stack-trace-on-error* *print-stack-trace-on-error*
				    *in* piped-in
				    *out* piped-out
				    *err* (PrintWriter. *out*)]
			    (try
			     (clojure.main/repl
                              :init (fn [] 
				      (use 'conexp)
				      (in-ns 'conexp))
                              :caught (fn [e]
					(if *print-stack-trace-on-error*
                                          (.printStackTrace e *out*)
                                          (prn (clojure.main/repl-exception e)))
					(when (eof-ex? e)
					  (throw e))
					(flush))
                              :need-prompt (constantly true))
                            (catch Exception ex
                              (prn "REPL closing")))))
	repl-thread (Thread. repl-thread-fn)]
    (.start repl-thread)
    {:repl-thread repl-thread
     :repl-fn (fn [cmd]
		(.write cmd-wtr cmd)
		(.flush cmd-wtr))
     :result-fn (fn []
		  (loop [wtr (CharArrayWriter.)]
		    (.write wtr (.read result-rdr))
		    (if (.ready result-rdr)
		      (recur wtr)
		      (.toString wtr))))}))

(defn repl-in [rpl string]
  ((:repl-fn rpl) (str string "\n")))

(defn repl-out [rpl]
  (let [result ((:result-fn rpl))]
    result))

(defn repl-interrupt [rpl]
  (.stop (:repl-thread rpl)))

(defn repl-alive? [rpl]
  (.isAlive (:repl-thread rpl)))


;;; Display

(defn conexp-repl-init []
  [ [] (ref {:last-pos 0
	     :output-thread nil
	     :repl-thread nil}) ])

(defn conexp-repl-post-init [this]
  (dosync
   (alter (.state this) 
	  assoc :output-thread (Thread. 
				(fn []
				  (while (repl-alive? (.replThread this))
				    (let [result (repl-out (.replThread this))]
				      (invoke-later #(. this insertResult result)))))))
   (alter (.state this)
	  assoc :repl-thread (create-clojure-repl)))
  (.start (@(.state this) :output-thread)))

(defn conexp-repl-lastPosition [this]
  (@(.state this) :last-pos))

(defn conexp-repl-outputThread [this]
  (@(.state this) :output-thread))

(defn conexp-repl-replThread [this]
  (@(.state this) :repl-thread))

(defn conexp-repl-remove [this off len]
  (if (>= (- off len -1) (@(.state this) :last-pos))
    (. this removeSuper off len)))

(defn conexp-repl-insertResult [this result]
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
      (if (and (= string "\n")
	       (= off (- (-> this .getEndPosition .getOffset)
			 2)))
	(let [input (.getText this
			      (- last-pos 1)
			      (- (-> this .getEndPosition .getOffset) last-pos 1))]
	  (if (balanced? input)
	    (repl-in (.replThread this) input )))))))

;;;

(defn make-clojure-repl []
  (let [rpl (conexp.gui.repl.ClojureREPL.)
	win-rpl (JTextArea. rpl)]
    (.. win-rpl getInputMap (put (KeyStroke/getKeyStroke "control C") "interrupt"))
    (.. win-rpl getActionMap (put "interrupt" (proxy [AbstractAction] []
						(actionPerformed [_]
					          (println "Interrupt called!")
					      (repl-interrupt (.replThread rpl))))))
    win-rpl))
