;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.api.handler
  (:use conexp.main
        conexp.api.namespace)
  (:require [ring.util.response :refer [response, status]]
            [clojure.edn :as edn]
            [clojure.string :refer [split, lower-case]]
            [clojure.java.io :as io])
  (:import conexp.fca.contexts.Formal-Context
           conexp.fca.many_valued_contexts.Many-Valued-Context
           conexp.fca.lattices.Lattice
           conexp.fca.implications.Implication
           conexp.layouts.base.Layout))

(apply use extended-conexp-clj-namespaces)

;;; Process Data

(defn read-data 
  "Converts data into formats used by Clojure."
  [data]
  (let [raw (:data data)]
    (condp = (:type data)
      ;; remove colons from map
      "map" (if (some? raw)
              (into {} (for [[k v] raw] [(edn/read-string (name k)) 
                                         (if (= (type v) clojure.lang.PersistentArrayMap) 
                                           (read-data v)
                                           v)])))
      "context" (json->ctx raw)
      ;; casting its content to char-array is the same as using the filename
      "context_file" (read-context (char-array raw))
      "mv_context" (make-mv-context 
                     (:objects raw)
                     (:attributes raw)
                     (read-data {:type "map" :data (:incidence raw)}))
      "mv_context_file" (read-mv-context (char-array raw))
      "lattice" (json->lattice raw)
      "implication" (json->implication raw) 
      "implication_set" (json->implications raw) ;(map #(apply make-implication % ) raw)
      "layout" (json->layout raw)
      "method" (resolve (symbol raw))
      raw)))

(defn write-data 
  "Takes formats used in Clojure and converts them into formats useable by 
  JSON."
  [data]
  ;; some functions return sets of implications e.g.
  ;; but since JSON knows maps (objs) you should not convert those to vectors
  (if (and (coll? data)(not (map? data)))
    (mapv write-data data)
    (condp instance? data
      Formal-Context (ctx->json data)
      Many-Valued-Context {:objects (objects data)
                           :attributes (attributes data)
                           :incidence (incidence data)}
      Lattice (lattice->json data)
      Implication (implication->json data)
      Layout (layout->json data)
      clojure.lang.PersistentArrayMap (into {} (mapv #(vector (key %) 
                                                              (write-data (val %))) data))
      data)))

;;; Process functions

(defn run-function
  "Given a function object and a collection of data tries to run the 
  function."
  [function data]
  (let [namestring (:name function)
        args (:args function)]
   (if (whitelist-names namestring)
     ;; use namestring as function and args as keys in datamap
     (apply 
      ;; the used namespace consists mainly of only conexp-clj functions
      (ns-resolve (symbol "conexp.api.handler") (symbol namestring)) 
      (map data (map keyword args)))
     (throw (Exception. "Function name not allowed.")))))

(defn process-functions 
  "Loops over each function object and tries to run it. Returned is a map with
  the old keys and the return value or error as value."
  [functions data]
  ;; each iteration uses the prior ones as possible arguments
  ;; one could sort functions in the very next line based on dependencies
  (loop [unprocessed functions
         processed {}]
    (if (empty? unprocessed)
      processed
      (let [next-function (first unprocessed)
            result {(first next-function)
                    (try (run-function (last next-function) 
                                       (merge data processed))
                      (catch Exception e e))}]
        (recur (drop 1 unprocessed) (merge processed result))))))

;;; Build response

(defn get-type
  "Gets the type for the result and makes it more readable."
  [result]
  ;; split string before capital letters and use the last word
  ;; eg. "class clojure.lang.PersistentHashSet" -> "set"
  (let [typename (lower-case (last (split (str (type result)) #"(?=[A-Z])")))]
    ;; collections other than maps don't mix elements
    ;; therefore add the element type
    (cond 
      (symbol? result) "string"
      (and (coll? result) (not (map? result)))
        (str (get-type (first result)) "_set")
      :else (str typename))))

(defn build-map
  "Transforms any result value into a map with an added type an status."
  [result]
  (cond 
    (instance? Exception result) {:status 400
                                  :type (get-type result) 
                                  :msg (.getMessage result)    
                                  :result nil}
    (nil? result) {:status 204                    
                   :type (get-type result) 
                   :msg "Return value is nil."  
                   :result nil}
    :else {:status 200                    
           :type (get-type result) 
           :msg nil                     
           :result (write-data result)}))

;;; Handler

(defn get-http-status
  "Based on the whole body updates the overall http status."
  [body]
  (if (empty? body)
    204
    (apply max (for [[k v] body] (:status v)))))

(defn handler
  "Handles the JSON request and constructs the JSON response."
  [request]
  (let [body (:body request)
        id (:id body) ;an id is just copied if provided
        ;; types defines for JSON objects
        fn-types (list "function" "silent-function")
        ;; each obejct that not a function type is sent through "read-data"
        data (into {} (for [[k v] body 
                            :when (not (some #{(:type v)} fn-types))] 
                           [k (read-data v)]))
        ;; each name from function types is run as an acutal function
        results (process-functions 
                   (filter (fn [a](some #{(:type (val a))} fn-types)) body)
                   data)
        ;; the body map is build with all executed funtion types, their result
        ;; and status
        result-map (into {} (for [[k v] results
                                   :when (= "function" (:type (k body)))] 
                                 [k (build-map v)]))]
    (status 
      (response (merge (if id {:id id}) result-map))
      (get-http-status result-map))))

;;;

nil
