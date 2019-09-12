;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.api.handler
  (:require [ring.util.response :refer [response]]))

;;; Macros

;;TODO read data macro

(defn read-data [data] data)

;;; Shorthand functions



;;; Handler

(defn run-function
  "Given a function object and a collection of data tries to run the 
  function."
  [function data]
  (let [namestring (:name function)
        id (:id function)
        args (:args function)]
   ;; use namestring as function and args as keys in datamap
   (apply (resolve (symbol namestring)) (map data (map keyword args)))))

(defn process-function 
  "Tries to run the function and return a map with the result or error.
  The status is either 0 on succes or 1 on error."
  [function data]
  ;; run function an get either a result or error message
  (let [result (try {:result (run-function function data)}
                (catch Exception e {:msg (.getMessage e)}))]
   ;; add an corresponding status to the map
   (merge (if (contains? result :result) {:status 0} {:status 1})
          result))) 

(defn handler
  "Handles the JSON request and constructs the JSON response."
  [request]
  (response
    (apply hash-map
      (flatten
        (list
         ;; an basic id is just copied if provided
         (let [id (:id (:body request))] (if id (list :id id) (list)))
         ;; tries to parse each object in body besides id and function as data
         ;; afterwards tries to run each function with the data
         (let [body (:body request)
               data (read-data body)
               id (:id body)] 
          (map #(list (keyword (str id (if id "/") (:name %))) 
                      (process-function % data)) 
               (:functions body))))))))

;;;

nil
