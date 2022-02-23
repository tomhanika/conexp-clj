;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.lattices
  (:require [clojure.data.json :as json]
            [json-schema.core  :as json-schema])
  (:use conexp.base
        conexp.math.algebra
        conexp.fca.lattices
        conexp.io.util
        conexp.io.json)
  (:import [java.io PushbackReader]))

;;; Input format dispatch

(define-format-dispatch "lattice")
(set-default-lattice-format! :simple)

;;; Formats

;; Simple conexp-clj Format

(add-lattice-input-format :simple
                          (fn [rdr]
                            (= "conexp-clj simple" (read-line))))

(define-lattice-output-format :simple
  [lat file]
  (with-out-writer file
    (binding [*print-length* nil]
      (println "conexp-clj simple")
      (prn {:lattice [(base-set lat)
                      (set-of [x y]
                              [x (base-set lat)
                               y (base-set lat)
                               :when ((order lat) [x y])])]}))))

(define-lattice-input-format :simple
  [file]
  (with-in-reader file
    (let [_        (get-line),
          hash-map (binding [*in* (PushbackReader. *in*)]
                     (read)),
          lattice  (:lattice hash-map)]
      (when-not lattice
        (illegal-argument "File " file " does not contain a lattice."))
      (apply make-lattice lattice))))

;; Json helpers

(defn- concept->json
  "Returns the concept in json format."
  [concept]
  {:extent (first concept)
   :intent (second concept)})

(defn- concept-pair->json
  "Returns the pair of concepts in json format."
  [pair]
  {:start_concept (concept->json (first pair)) 
   :end_concept (concept->json (second pair))})

(defn- lattice-order->json
  "Returns the order of the concept lattice in json format."
  [lattice]
  (let [pairs (set-of [x y]
                      [x (base-set lattice)
                       y (base-set lattice)
                       :when ((order lattice) [x y])])]
    (map concept-pair->json pairs)))

(defn lattice->json
  "Returns a concept lattice, consisting of the base set and order, in json format."
  [lat]
  {:formal_concepts (mapv concept->json (base-set lat))
   :lattice_structure (lattice-order->json lat)})

(defn- json->concept
  "Returns a vector containing one map each for extent and intent."
  [json-concept]
  [(into #{} (:extent json-concept)) 
   (into #{} (:intent json-concept))])

(defn- json->concept-pair
  "Returns a vector containing the concept pair."
  [json-concept-pair]
  [(into [] (json->concept (:start_concept json-concept-pair)))
   (into [] (json->concept (:end_concept json-concept-pair)))])

(defn json->lattice
  "Returns a Lattice object for the given json lattice."
  [json-lattice]
  (let [json-concepts (:formal_concepts json-lattice)
        json-lattice-order (:lattice_structure json-lattice)
        lattice-base-set (map json->concept json-concepts)
        lattice-order (map json->concept-pair json-lattice-order)]
    (make-lattice lattice-base-set lattice-order)))

;; Json Format

(add-lattice-input-format :json
                          (fn [rdr]
                            (try (json-object? rdr)
                                 (catch Exception _))))

(define-lattice-output-format :json
  [lattice file]
  (with-out-writer file
    (print (json/write-str (lattice->json lattice)))))

(define-lattice-input-format :json
  [file]
  (with-in-reader file
    (let [json-lattice (json/read *in* :key-fn keyword)
          schema-file "src/main/resources/schemas/lattice_schema_v1.0.json"]
      (assert (matches-schema? json-lattice schema-file)
              (str "The input file does not match the schema given at " schema-file "."))
      (json->lattice json-lattice))))

;;; ConExp lattice format

'TODO

;;;

nil
