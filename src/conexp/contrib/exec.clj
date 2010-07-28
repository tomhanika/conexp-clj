;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.exec
  (:use conexp.main
        [conexp.io.util :only (tmpfile)])
  (:use clojure.contrib.shell-out))

(ns-doc "Executing external programs with a common interface.")

;;;

(defn run-in-shell
  "Runs given commandline in an external shell process."
  [& cmdln]
  (apply sh
         :return-map true
         cmdln))

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
       (run-in-shell ~@(replace (merge {:context `(context-to-file ~'context ~input-format)}
                                       (dissoc (zipmap arguments arg-names) :context))
                                cmdln)))))

;;;

(defn- context-to-file
  "Writes context to a file using format and returns the name of the
  file."
  [context format]
  (let [^java.io.File file (tmpfile)]
    (write-context format context file)
    (.getAbsolutePath file)))

(defn- context-from-file
  "Returns the context from file."
  [file]
  (read-context file))

;;;

nil
