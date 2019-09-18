;; Copyright ⓒ the conexp-clj developers; all rights reserved.                  
;; The use and distribution terms for this software are covered by the          
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)  
;; which can be found in the file LICENSE at the root of this distribution.     
;; By using this software in any fashion, you are agreeing to be bound by       
;; the terms of this license.                                                   
;; You must not remove this notice, or any other, from this software.           

(ns conexp.api.util-test
  (:use conexp.base
        conexp.api)
  (:use ring.mock.request)
  (:require [clojure.data.json :refer [read-str]]))

;;;

(defn mock-request
  "Builds a JSON request with any map."
  [body]
  (read-str
    (:body
      ((set-middleware false)
       (json-body
       (request :post "/")
       body)))
     :key-fn keyword))

;;;

nil
