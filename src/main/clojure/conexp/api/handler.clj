;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.api.handler
  (:use conexp.main)
  (:require [ring.util.response :refer [response]]
            [clojure.java.io :as io])
  (:import conexp.fca.lattices.Lattice))

(apply use conexp-clj-namespaces)

;;; Process Data

(defn read-data 
  "Reads in strings and converts data into formats used by Clojure."
  [data]
  (let [raw (:data data)]
    (condp = (:type data)
      "lattice" (make-lattice (first raw) (last raw))
      "context" (read-context (char-array raw))
      raw)))

(defn write-data 
  "Takes formats used in Clojure and converts them in more general formats."
  [data]
  (condp instance? data
    Lattice [(base-set data)
             (set-of [x y]
                     [x (base-set data)
                      y (base-set data)
                      :when ((order data) [x y])])]
    data))

;;; Shorthand functions


;;; Process functions

(defn run-function
  "Given a function object and a collection of data tries to run the 
  function."
  [function data]
  (let [namestring (:name function)
        args (:args function)]
   ;; use namestring as function and args as keys in datamap
   (write-data
     (apply 
      (ns-resolve 'conexp.api.handler (symbol namestring)) 
      (map data (map keyword args))))))

(defn process-functions 
  "Loops over each function object and tries to run it. Returned is a map with
  the old keys and the return value or error as value."
  [functions data]
  (loop [unprocessed functions
         processed {}]
    (if (empty? unprocessed)
      processed
      (let [next-function (first unprocessed)
            result {(first next-function)
                    (try (run-function (last next-function) 
                                       (merge data processed))
                      (catch Exception e (.getMessage e)))}]
        (recur (drop 1 unprocessed) (merge processed result))))))

;;; Handler

(defn build-map
  "Transforms any result value into a map with an added type an status."
  [result]
  (cond 
    (string? result) {:status 1 :msg result}
    (nil? result)    {:status 2 :msg "Return value is nil." :result nil}
    :else            {:status 0 :result result}))

(defn handler
  "Handles the JSON request and constructs the JSON response."
  [request]
  (let [body (:body request)
        id (:id body) ;an id is just copied if provided
        ;; each obejct that not a function type is sent through "read-data"
        data (into {} (for [[k v] body :when (not (= "function" (:type v)))] 
                           [k (read-data v)]))
        ;; each name from function types is run as an acutal function
        results (process-functions 
                   (filter #(= "function" (:type (val %))) body)
                   data)]
    (response 
      (merge
        (if id {:id id})
        (into {} (for [[k v] results] 
                      [k (build-map (write-data v))]))))))

;;;

nil
