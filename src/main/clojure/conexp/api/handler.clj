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

(defn read-data [data] nil)

;;; Handler

;;TODO check if valid request by my definition?
;;TODO read data macro
;;TODO process-function function
;;TODO qutomatic sort parameters?
;;TODO add error messages

(defn process-function 
  "Tries to either apply the given parameters to the function as is or search
  for it in a list of pre-defined shorthands. If both fail one of the following
  error messages is applied:
  "
  [function data]
  nil)

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
         (let [data (read-data (:body request))] 
          (map #(process-function % data) (:functions (:body request)))))))))

;;;

nil
