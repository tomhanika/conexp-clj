(comment
 ;* Copied from
 ;*
 ;* Copyright (c) ThorTech, L.L.C.. All rights reserved.
 ;* The use and distribution terms for this software are covered by the
 ;* Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 ;* which can be found in the file epl-v10.html at the root of this distribution.
 ;* By using this software in any fashion, you are agreeing to be bound by
 ;* the terms of this license.
 ;* You must not remove this notice, or any other, from this software.
 ;*
 ;* Author: Eric Thorsen, Narayan Singhal
 )

(ns conexp.gui.enclojure-repl
  (:import [java.io PipedWriter PipedReader PrintWriter CharArrayWriter]))

(def *printStackTrace-on-error* false)

(defn is-eof-ex? [throwable]
  (and (instance? clojure.lang.LispReader$ReaderException throwable)
       (or (.startsWith (.getMessage throwable) "java.lang.Exception: EOF while reading")
	   (.startsWith (.getMessage throwable) "java.io.IOException: Write end dead"))))

(defn create-clojure-repl []
  "This function creates an instance of clojure repl using piped in and out.
   It returns a map of two functions repl-fn and result-fn - first function
   can be called with a valid clojure expression and the results are read using
   the result-fn."
  (let [cmd-wtr (PipedWriter.)
        result-rdr (PipedReader.)
        piped-in (clojure.lang.LineNumberingPushbackReader. (PipedReader. cmd-wtr))
        piped-out (PrintWriter. (PipedWriter. result-rdr))
        repl-thread-fn #(binding [;*print-base* *print-base* Not in 1.0
                                  ;*print-circle* *print-circle*
                                  *print-length* *print-length*
                                  *print-level* *print-level*
                                  ;*print-lines* *print-lines*  
                                  ;*print-miser-width* *print-miser-width*
                                  ;*print-radix* *print-radix* Not in 1.0
                                  ;*print-right-margin* *print-right-margin*
                                  ;*print-shared* *print-shared*  Not in 1.0
                                  ;*print-suppress-namespaces* *print-suppress-namespaces*
                                  ;*print-pretty* *print-pretty*
                                  *warn-on-reflection* *warn-on-reflection*
				  *in* piped-in
                                  *out* piped-out
                                  *err* (PrintWriter. *out*)]
                          (try
                            (clojure.main/repl
                              :init (fn [] (in-ns 'user))
                              :caught (fn [e]
                                        (when (is-eof-ex? e)
                                          (throw e))
                                        (if *printStackTrace-on-error*
                                          (.printStackTrace e *out*)
                                          (prn (clojure.main/repl-exception e)))
                                        (flush))
                              :need-prompt (constantly true))
                            (catch clojure.lang.LispReader$ReaderException ex
                              (prn "REPL closing"))
                            (catch java.lang.InterruptedException ex)
                            (catch java.nio.channels.ClosedByInterruptException ex)))]
    (.start (Thread. repl-thread-fn))
    {:repl-fn (fn [cmd]
                (if (= cmd ":CLOSE-REPL")
                  (do
                    (.close cmd-wtr)
                    (.close result-rdr))
                  (do
                    (.write cmd-wtr cmd)
                    (.flush cmd-wtr))))
     :result-fn (fn []
		  (loop [wtr (CharArrayWriter.)]
		    (.write wtr (.read result-rdr))
		    (if (.ready result-rdr)
		      (recur wtr)
		      (.toString wtr))))}))