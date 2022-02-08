;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.fcas
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
                           (let [schema (json-schema/prepare-schema 
                                         (-> "src/main/resources/schemas/fca_schema_v1.0.json" slurp 
                                             (cheshire.core/parse-string true))
                                         ;; referencing inside of schemas with relative references
                                         {:classpath-aware? true
                                          :default-resolution-scope "classpath://schemas/"})
                                 json (json/read rdr)]
                             (json-schema/validate schema json)
                             :success))))

(defn create-fca-output-map
  [fca]
  (let [ctx (:context fca)
        lattice (:lattice fca)
        implication-sets (:implication-sets fca)
        fca-map {:context (ctx->json ctx)}
        fca-map (if-not (nil? lattice) 
                  (into fca-map {:lattice (lattice->json lattice)})
                  fca-map)
        fca-map (if-not (nil? implication-sets)
                  (into fca-map {:implication_sets (mapv implications->json implication-sets)})
                  fca-map)] 
    fca-map))

(defn create-fca-input-map
  [json-fca]
  (let [json-ctx (:context json-fca)
        json-lattice (:lattice json-fca)
        json-implication-sets (:implication_sets json-fca)
        fca-map {:context (json->ctx (:formal_context json-ctx))}
        fca-map (if-not (nil? json-lattice)
                  (into fca-map {:lattice (json->lattice json-lattice)})
                  fca-map)
        fca-map (if-not (nil? json-implication-sets)
                  (into fca-map {:implication-sets (map json->implications json-implication-sets)})
                  fca-map)]
    fca-map))

(define-fca-output-format :json
  [fca file]
  (let [fca-map (create-fca-output-map fca)]
    (with-out-writer file
      (print
       (json/write-str fca-map)))))

(define-fca-input-format :json
  [file]
  (with-in-reader file
    (let [json-fca (json/read *in* :key-fn keyword)
          fca-map (create-fca-input-map json-fca)]
      fca-map)))
