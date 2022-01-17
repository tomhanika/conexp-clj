;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.fca
  (:require [conexp.io.contexts     :refer :all]
            [conexp.io.implications :refer :all]
            [conexp.io.lattices     :refer :all]
            [conexp.io.util         :refer :all]
            [clojure.data.json      :as json]
            [json-schema.core       :as json-schema]))

;;; Input format dispatch

(define-format-dispatch "fca")
(set-default-fca-format! :json)

;;; Formats

;; Json Format

(add-fca-input-format :json
                      (fn [rdr]
                        (= :success
                           (let [schema (json-schema/prepare-schema (-> "src/main/resources/schemas/fca_schema_v1.0.json" slurp (cheshire.core/parse-string true)))
                                 json (json/read rdr)]
                             (json-schema/validate schema json)
                             :success))))

(define-fca-output-format :json
  [[ctx concepts implications] file]
  (with-out-writer file
    (println "json")
    (print
     (json/write-str {:context (ctx->json ctx)
                      :concepts (concepts->json concepts)
                      :implication_sets [(implications->json implications)]}))))

(define-fca-input-format :json
  [file]
  (with-in-reader file
    (let [json-fca (json/read *in* :key-fn keyword)
          json-ctx (:context json-fca)
          json-concepts (:concepts json-fca)
          json-implications (:implication_sets json-fca)]
      {:context (json->ctx (:formal_context json-ctx))
       :concepts (map json->concept (:formal_concepts json-concepts))
       :implication-sets (map json->implications json-implications)})))
