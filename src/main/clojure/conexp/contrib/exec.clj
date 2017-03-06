;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.exec
  "Executing external programs with a common interface."
  (:use [conexp.base :only (illegal-state)]
        [conexp.io.contexts :only (write-context)]
        [conexp.io.util :only (tmpfile with-out-writer)])
  (:use [clojure.java.shell :only (sh)]
        [clojure.string :only (split)])
  (:import [java.io File]
           [java.util Map]))

;;;

(defn program-exists?
  "Tests whether the given program-name is an executable program in
  the current path."
  [program-name]
  (let [path     (split (.get ^Map (System/getenv) "PATH")
                        (re-pattern File/pathSeparator)),
        program  (first (for [dir path,
                              program (.list ^File (File. ^String dir)),
                              :when (= program program-name)]
                          (str dir "/" program)))]
    (boolean (and program
                  (let [^File program-file (File. ^String program)]
                    (and (.canExecute program-file)
                         (.isFile program-file)))))))

;;;

(defn run-in-shell
  "Runs given commandline in an external shell process. Returns the
  output on stdout as result."
  [& cmdln]
  (let [result (apply sh cmdln)]
    (when-not (zero? (:exit result))
      (illegal-state "External program " (first cmdln) " returned with non-zero status: " (:err result)))
    (:out result)))

(defn context-to-file
  "Writes context to a file using format and returns the name of the
  file."
  [context format]
  (let [^java.io.File file (tmpfile)]
    (write-context format context file)
    (.getAbsolutePath file)))

(defmacro define-external-program
  "Defines program-name to be a function calling an external
  program. Keywords are treated as arguments to the function and for
  every keyword there will be a corresponding function argument with
  the same name (without the colon). Special treatment is given
  to :input-file, which must be followed by a valid format to
  write-context. Its argument will be called \"context\" (thus you
  shall not name any other argument this way) and will get a context
  as input. This context will then be written to a file using the give
  format and this file will be given as an argument to the external
  process.

  Returns a map of :exit, :out and :err pointing to corresponding exit
  value, value at stdout and value at stderr, respectively."
  [program-name & command-line]
  (let [cmdln                      (map (fn [x]
                                          (if (keyword? x)
                                            x,
                                            (str x)))
                                        command-line),
        [before-input after-input] (split-with #(not= :input-file %) cmdln),
        input-file?                (first after-input)
        input-format               (second after-input)
        cmdln                      (if input-file?
                                     (concat before-input [:context] (drop 2 after-input))
                                     before-input),
        arguments                  (filter keyword? cmdln),
        arg-names                  (vec (map (comp symbol name) arguments))]
    `(defn ~program-name ~arg-names
       (assert (program-exists? ~(first cmdln)))
       (run-in-shell ~@(replace (merge {:context `(context-to-file ~'context ~input-format)}
                                       (dissoc (zipmap arguments arg-names) :context))
                                cmdln)))))

;;;

(defmacro with-context-from-output
  "Runs body, expecting to get a string containing a formal
  context. Writes this string to a file and reads it in
  with read-context, returning the result."
  [& body]
  `(let [result# (do ~@body),
         file#   (tmpfile)]
     (with-out-writer file#
       (print result#))
     (read-context file#)))

(defmacro with-context-from-tmpfile
  "Creates a new temporary file, executes body and returns the result
  from calling read-context on file."
  [file & body]
  `(let [~file (tmpfile),
         result# (do ~@body)]
     (read-context ~file)))

;;;

true
