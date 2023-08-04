;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.implications
  (:require [conexp.fca.implications :refer :all]
            [conexp.io.util          :refer :all]
            [conexp.io.json          :refer :all]
            [clojure.data.json :as json]
            [json-schema.core  :as json-schema]))

;;; Input format dispatch

(define-format-dispatch "implication")
(set-default-implication-format! :json)

;;; Formats

;; Json helpers

(defn implication->json
  "Returns one implication in json format."
  [implication]
  {:premise (premise implication)
   :conclusion (conclusion implication)})

(defn implications->json
  "Returns a vector of implications in json format."
  [implication-list]
  {:implications
     (mapv implication->json implication-list)})

(defn json->implication
  "Returns an Implication object for the given json implication."
  [json-impl]
  (make-implication (into #{} (:premise json-impl))
                    (into #{} (:conclusion json-impl))))

(defn json->implications
  "Returns a sequence of Implication objects for the given json implications."
  [json]
  (let [impl (:implications json)]
    (map json->implication impl)))

;; Json Format (src/main/resources/schemas/implications_schema_v1.0.json)

(add-implication-input-format :json
                               (fn [rdr]
                                 (try (json-object? rdr)
                                      (catch Exception _))))

(define-implication-output-format :json
  [impl file]
  (with-out-writer file
    (print (json/write-str (implications->json impl)))))

(define-implication-input-format :json
  [file]
  (with-in-reader file
    (let [impl (json/read *in* :key-fn keyword)
          schema-file "schemas/implications_schema_v1.0.json"]
      (assert (matches-schema? impl schema-file)
              (str "The input file does not match the schema given at " schema-file "."))
      (json->implications impl))))
