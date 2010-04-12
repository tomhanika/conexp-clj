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

;;;

nil
