;; Copyright ⓒ the conexp-clj developers; all rights reserved.                  
;; The use and distribution terms for this software are covered by the          
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)  
;; which can be found in the file LICENSE at the root of this distribution.     
;; By using this software in any fashion, you are agreeing to be bound by       
;; the terms of this license.                                                   
;; You must not remove this notice, or any other, from this software.           

(ns conexp.api.handler-test
	(:use conexp.base
        conexp.api.util-test)
	(:use clojure.test)
  (:require [clojure.data.json :refer [write-str]]))

;;;

(deftest test-empty-request 
  (is (= (:body (mock-request {})) (write-str {}))))

(deftest test-generic-request 
  (is (= (:body (mock-request {:functions [{:name "+" :args ["eins", "zwei"]}] 
                               :eins {:type "int" :data 1}
                               :zwei {:type "int" :data 2}})) 
         (write-str {:+ {:status 0
                         :result 3}}))))
;;;

nil
