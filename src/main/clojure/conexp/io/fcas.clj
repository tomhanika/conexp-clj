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
            [conexp.io.json         :refer :all]
            [conexp.io.lattices     :refer :all]
            [conexp.io.util         :refer :all]
            [clojure.data.json      :as json]
            [json-schema.core       :as json-schema]))

;;; Input format dispatch

(define-format-dispatch "fca")
(set-default-fca-format! :json)

;;; Formats

;; Json Format (src/main/resources/schemas/fca_schema_v1.0.json)

(defn- create-fca-output-map
  "Returns a map containing the elements of the fca in json format each.
  
  The map contains :lattice and :implication-sets only if they are included in the fca."
  [fca]
  (let [ctx (:context fca)
        lattice (:lattice fca)
        implication-sets (:implication-sets fca)] 
    (cond-> {:context (ctx->json ctx)}
      (some? lattice) (assoc :lattice (lattice->json lattice))
      (some? implication-sets) (assoc :implication_sets (mapv implications->json implication-sets)))))

(defn- create-fca-input-map
  "Takes a map {:context context, :lattice lattice, :implication_sets [implication-sets]} as input in which the context, lattice and implication-sets are in json format. Returns a map containing the elements of the fca from the json-fca.

  :lattice and :implication-sets are optional. The output map contains them only if they are included in the json-fca."
  [json-fca]
  (let [json-ctx (:context json-fca)
        json-lattice (:lattice json-fca)
        json-implication-sets (:implication_sets json-fca)]
    (cond-> {:context (json->ctx json-ctx)}
      (some? json-lattice) (assoc :lattice (json->lattice json-lattice))
      (some? json-implication-sets) (assoc :implication-sets (map json->implications json-implication-sets)))))


(add-fca-input-format :json
                      (fn [rdr]
                        (try (json-object? rdr)
                             (catch Exception _))))

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
          schema-file "schemas/fca_schema_v1.0.json"]
      (assert (matches-schema? json-fca schema-file)
              (str "The input file does not match the schema given at " schema-file "."))
      (create-fca-input-map json-fca))))
