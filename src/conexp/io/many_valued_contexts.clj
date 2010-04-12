;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.many-valued-contexts
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.many-valued-contexts
        conexp.io.util)
  (:use [clojure.contrib.io :exclude (with-in-reader)])
  (:import [java.io PushbackReader]))

(update-ns-meta! conexp.io.many-valued-contexts
  :doc "Implements IO for Many-Valued Contexts.")

;;; Input format dispatch

(define-format-dispatch "mv-context")
(set-default-mv-context-format! :simple)

;;; Formats

;; Simple conexp-clj Format for Many-Valued Contexts

(add-mv-context-input-format :simple
                             (fn [rdr]
                               (= "conexp-clj simple" (.readLine rdr))))

(defmethod write-mv-context :simple [_ mv-context file]
  (with-out-writer file
    (println "conexp-clj simple")
    (prn {:many-valued-context [(objects mv-context)
                                (attributes mv-context)
                                (incidence mv-context)]})))

(defmethod read-mv-context :simple [file]
  (with-in-reader file
    (let [_        (get-line),
          hash-map (binding [*in* (PushbackReader. *in*)]
                     (read)),
          mv-ctx   (:many-valued-context hash-map)]
      (when-not mv-ctx
        (illegal-argument "File " file " does not contain a many-valued context."))
      (apply make-mv-context mv-ctx))))

;; Format Data Table

;; Note that all entries must be separated by ,. The MVC must have at
;; least one object and at least two attributes. The first line is a
;; comma separated line of all attributes. All subsequent lines comma
;; separated line starting with the object name followed by the valued
;; for the corresponding attributes.

(add-mv-context-input-format :data-table
                             (fn [rdr]
                               (try
                                (re-matches #"^[^,]+,[^,]+.*$" (.readLine rdr))
                                (catch Exception _))))

(defmethod write-mv-context :data-table [_ mv-context file]
  (with-out-writer file
    (when (> 2 (count (attributes mv-context)))
      (illegal-argument "Cannot store many-valued contexts with less then 2 attributes in format :data-table."))
    (when (= 0 (count (objects mv-context)))
      (illegal-argument "Cannot store many-valued context without objects in format :data-table."))
    (let [write-comma-line (fn [things]
                             (cond
                              (empty? things) nil,
                              (= 1 (count things)) (prn (first things)),
                              :else (do (pr (first things))
                                        (print ",")
                                        (recur (rest things)))))]
      (write-comma-line (attributes mv-context))
      (doseq [g (objects mv-context)]
        (write-comma-line (cons g (map #((incidence mv-context) [g %]) (attributes mv-context))))))))

(defmethod read-mv-context :data-table [file]
  (with-in-reader file
    (let [read-comma-line (fn []
                            (let [line (get-line)]
                              (read-string (str "(" line ")")))),
          attributes      (read-comma-line),
          lines           (doall
                           (take-while #(not (empty? %))
                                       (repeatedly read-comma-line))),
          objects         (map first lines)
          interpretation  (into {}
                                (for [line lines
                                      :let [g (first line),
                                            values (rest line)],
                                      [m w] (map vector attributes values)]
                                  [[g m] w]))]
      (make-mv-context objects attributes interpretation))))

;;;

nil
