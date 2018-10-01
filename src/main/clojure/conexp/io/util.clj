;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.util
  (:use conexp.base)
  (:require [clojure.java.io :as io]))

;;;

(defalias reader io/reader)
(defalias writer io/writer)

(defn ^String get-line
  "Reads one line from *in*.  Throws an EOFException if line does not exist."
  []
  (let [line (read-line)]
    (if-not line
      (throw (java.io.EOFException. "Premature end of input"))
      line)))

(defn get-lines
  "Reads n line from *in*.  Throws an EOFException if less than n lines have been read."
  [n]
  (let [lines (doall (take n (repeatedly #(get-line))))]
    (if-not (every? (comp not nil?) lines)
      (throw (java.io.EOFException. (format "Expected %d lines of input, but got less" n)))
      lines)))

(defmacro with-in-reader
  "Opens file with reader and binds it to *in*."
  [file & body]
  `(with-open [input# (reader ~file)]
     (binding [*in* input#,
               *read-eval* false]
       ~@body)))

(defmacro with-out-writer
  "Opens file with writer and binds it to *out*."
  [file & body]
  `(with-open [output# (writer ~file)]
     (binding [*out* output#]
       ~@body)))

(defn tmpfile
  "Returns a temporary and unique File object."
  []
  (let [^java.io.File file (java.io.File/createTempFile "conexp-clj-" ".tmp")]
    (.deleteOnExit file)
    file))


;;; Format dispatch framework macro

(defmacro define-format-dispatch
  "Defines for name the functions write-name, read-name,
  add-name-input-format, get-known-name-input-formats and
  find-name-input-format. You can then add new formats with
  add-name-input-format and read-name will automatically dispatch in
  the format determined from its only argument."
  [name]
  (let [add   (symbol (str "add-" name "-input-format")),
        get   (symbol (str "get-known-" name "-input-formats")),
        find  (symbol (str "find-" name "-input-format")),
        write (symbol (str "write-" name)),
        read  (symbol (str "read-" name)),
        get-default-write (symbol (str "get-default-" name "-format")),
        set-default-write (symbol (str "set-default-" name "-format!")),
        list-formats (symbol (str "list-" name "-formats")),
        list-in-formats (symbol (str "list-" name "-input-formats")),
        list-out-formats (symbol (str "list-" name "-output-formats"))]
  `(do
     (let [known-input-formats# (ref {})]
       (defn- ~add [name# predicate#]
         "Adds a new file format predicate under the given name."
         (dosync
          (alter known-input-formats# assoc name# predicate#)))

       (defn- ~get []
         (keys @known-input-formats#))

       (defn- ~find
         "Tries to determine the format used in file."
         ([file#]
            (first (for [[name# predicate#] @known-input-formats#
                         :when (with-in-reader file#
                                 (predicate# *in*))]
                     name#)))
         ([file# format#]
            (keyword format#)))

       nil)

     (let [default-write-format# (atom nil)]
       (defn ~get-default-write
         ~(str "Returns default write format for " name "s.")
         []
         (when (nil? @default-write-format#)
           (illegal-state "No default write format specified for " ~name "."))
         @default-write-format#)

       (defn ~set-default-write
         ~(str "Sets default write format for " name "s to format.")
         [format#]
         (reset! default-write-format# format#))

       nil)

     (defmulti ~write
       ~(str "Writes " name " to file using format.")
       {:arglists (list [(symbol "format") (symbol ~name) (symbol "file")]
                        [(symbol ~name) (symbol "file")])}
       (fn [& args#]
         (cond
          (= 2 (count args#)) ::default-write
          (>= 3 (count args#)) (keyword (first args#))
          :else (illegal-argument "Invalid number of arguments in call to " ~write "."))))
     (defmethod ~write :default [format# & _#]
       (illegal-argument "Format " format# " for " ~name " output is not known."))
     (defmethod ~write ::default-write [ctx# file#]
       (~write (~get-default-write) ctx# file#))

     (defmulti ~read
       ~(str "Reads " name " from file, automatically determining the format used.")
       {:arglists (list [(symbol "file")] [(symbol "file") (symbol "explicit-format")])}
       (fn [& args#] (apply ~find args#)))
     (defmethod ~read :default
       [file#]
       (illegal-argument "Cannot determine format of " ~name " in " file#))

     (defn ~list-in-formats
       ~(str "Returns a list of known " name " input formats")
       []
       (keys (dissoc (methods ~read)
                     :default)))

     (defn ~list-out-formats
       ~(str "Returns a list of known " name " input formats")
       []
       (keys (dissoc (methods ~write)
                     :default
                     ::default-write)))

     (defn ~list-formats
       ~(str "Returns a list of known " name " IO formats, i.e. formats "
             "for which reading and writing is defined.")
       []
       (seq (intersection (set (~list-in-formats)) (set (~list-out-formats)))))

     (defmacro ~(symbol (str "define-" name "-input-format"))
       ~(str "Defines input format for " name "s.")
       [~'input-format [~'file] & ~'body]
       `(defmethod ~'~read ~~'input-format
          ([~~'file]
             ~@~'body)
          ([~~'file ~'~'_]
             ~@~'body)))

     (defmacro ~(symbol (str "define-" name "-output-format"))
       ~(str "Defines output format for " name "s.")
       [~'input-format [~'thing ~'file] & ~'body]
       `(defmethod ~'~write ~~'input-format
          [~'~'_ ~~'thing ~~'file]
          ~@~'body))

     nil)))

;;;

nil
