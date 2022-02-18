;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.json
  "Provides functionality to read and process json files"
  (:require [json-schema.core  :as json-schema]
            [clojure.data.json :as json]))

(defn read-schema
  [file]
  (json-schema/prepare-schema 
   (-> file slurp 
       (cheshire.core/parse-string true))
   ;; referencing inside of schemas with relative references
   {:classpath-aware? true
    :default-resolution-scope "classpath://schemas/"}))

(defn- json-format?
  "Validate if string is in json format"
  [string]
  (try (json/read-str string) true
       (catch Exception _ false)))

(defn json-object?
  "Validate if file content is a json object (json format beginning with {)"
  [rdr]
  (let [content (slurp rdr)]
    (and (json-format? content) (= \{ (first content)))))
