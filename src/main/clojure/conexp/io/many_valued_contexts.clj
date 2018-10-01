;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.many-valued-contexts
  "Implements IO for Many-Valued Contexts."
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.many-valued-contexts
        conexp.io.util)
  (:import [java.io PushbackReader]))

;;; Input format dispatch

(define-format-dispatch "mv-context")
(set-default-mv-context-format! :simple)

(defalias read-many-valued-context read-mv-context)
(defalias write-many-valued-context write-mv-context)

;;; Formats

;; Simple conexp-clj Format for Many-Valued Contexts

(add-mv-context-input-format :simple
                             (fn [rdr]
                               (= "conexp-clj simple" (read-line))))

(define-mv-context-output-format :simple
  [mv-context file]
  (with-out-writer file
    (binding [*print-length* nil]
      (println "conexp-clj simple")
      (prn {:many-valued-context [(objects mv-context)
                                  (attributes mv-context)
                                  (incidence mv-context)]}))))

(define-mv-context-input-format :simple
  [file]
  (with-in-reader file
    (let [_        (get-line),
          hash-map (binding [*in* (PushbackReader. *in*)]
                     (read)),
          mv-ctx   (:many-valued-context hash-map)]
      (when-not mv-ctx
        (illegal-argument "File " file " does not contain a many-valued context."))
      (apply make-mv-context mv-ctx))))


;; Data Table Format

;; Note the following restrictions:
;;   - we need at least one object and at least two attributes (to
;;     reliably determine the file type)
;;   - the first line must contain of the attributes in the correct order
;;   - if the subsequent lines have the same number of entries as the
;;     first, the resulting mv-context will have the line number as
;;     objects,
;;   - if the subsequent lines have one more element as the first, the first
;;     entry will be the object for that line

(add-mv-context-input-format :data-table
                             (fn [rdr]
                               (try
                                (re-matches #"^[^,]+,[^,]+.*$" (read-line))
                                (catch Exception _))))

(define-mv-context-output-format :data-table
  [mv-context file]
  (with-out-writer file
    (when (> 2 (count (attributes mv-context)))
      (unsupported-operation
       "Cannot store many-valued contexts with less then 2 attributes in format :data-table."))
    (when (= 0 (count (objects mv-context)))
      (unsupported-operation
       "Cannot store many-valued context without objects in format :data-table."))
    (let [write-comma-line (fn [things]
                             (cond
                              (empty? things) nil,
                              (= 1 (count things)) (prn (first things)),
                              :else (do (pr (first things))
                                        (print ",")
                                        (recur (rest things)))))]
      (write-comma-line (attributes mv-context))
      (doseq [g (objects mv-context)]
        (write-comma-line (cons g (map #((incidence mv-context) [g %])
                                       (attributes mv-context))))))))

(define-mv-context-input-format :data-table
  [file]
  (with-in-reader file
    (let [read-comma-line (fn []
                            (try
                              (let [line (get-line)]
                                (read-string (str "(" line ")")))
                              (catch java.io.EOFException _
                                nil))),
          attributes      (read-comma-line),
          lines           (doall
                           (take-while #(not (nil? %))
                                       (repeatedly read-comma-line))),
          line-lengths    (set-of (count line) [line lines])]
      (when (< 1 (count line-lengths))
        (illegal-argument "Many-Valued Context in file " file " has lines of different length."))
      (when (and (not= (count attributes) (first line-lengths))
                 (not= (inc (count attributes)) (first line-lengths)))
        (illegal-argument
         "Number of values in lines in file " file " does not match given attributes.\n"
         "Number of values given should be equal or once more to the number of attributes."))
      (let [lines      (if (not= (first line-lengths) (count attributes))
                         lines
                         (map #(cons %1 %2) (iterate inc 0) lines)),
            objects    (map first lines),
            object-set (set objects)]
        (when (not= (count objects) (count object-set))
          (illegal-argument "Given file " file " contains double entries for objects."))
        (let [interpretation  (into {}
                                    (for [line lines
                                          :let [g (first line),
                                                values (rest line)],
                                          [m w] (map vector attributes values)]
                                      [[g m] w]))]
          (make-mv-context object-set attributes interpretation))))))

;;;

nil
