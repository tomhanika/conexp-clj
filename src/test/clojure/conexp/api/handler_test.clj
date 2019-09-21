;; Copyright ⓒ the conexp-clj developers; all rights reserved.                  
;; The use and distribution terms for this software are covered by the          
;; Eclipse Public Lic:ense 1.0 (http://opensource.org/licenses/eclipse-1.0.php)  
;; which can be found in the file LICENSE at the root of this distribution.     
;; By using this software in any fashion, you are agreeing to be bound by       
;; the terms of this license.                                                   
;; You must not remove this notice, or any other, from this software.           

(ns conexp.api.handler-test
	(:use conexp.base
        conexp.fca.contexts
        conexp.io.contexts
        conexp.api.util-test)
	(:use clojure.test))

;;; Generic tests

(deftest test-empty-request 
  (is (= (mock-request {}) {})))

(deftest test-generic-request 
  (is (= (mock-request {:function {:type "function"
                                   :name "+" 
                                   :args ["eins", "zwei"]} 
                        :eins {:type "int" :data 1}
                        :zwei {:type "int" :data 2}}) 
         {:function {:status 0
                     :result 3}})))

(deftest test-generic-request-multiple-independent-functions 
  (is (= (mock-request {:function1 {:type "function"
                                    :name "+" 
                                    :args ["eins", "zwei"]}
                        :function2 {:type "function"
                                    :name "-" 
                                    :args ["eins", "zwei"]} 
                        :eins {:type "int" :data 1}
                        :zwei {:type "int" :data 2}}) 
         {:function1 {:status 0
              :result 3}
          :function2 {:status 0
              :result -1}})))

(deftest test-generic-request-multiple-dependent-functions 
  (is (= (mock-request {:function1 {:type "function"
                                    :name "+" 
                                    :args ["eins", "zwei"]}
                        :function2 {:type "function"
                                    :name "-" 
                                    :args ["function1", "zwei"]} 
                        :eins {:type "int" :data 1}
                        :zwei {:type "int" :data 2}}) 
         {:function1 {:status 0
              :result 3}
          :function2 {:status 0
              :result 1}})))

(deftest test-generic-request-id
  (is (= (mock-request {:function {:type "function"
                                   :name "+" 
                                   :args ["eins", "zwei"]} 
                        :id 42
                        :eins {:type "int" :data 1}
                        :zwei {:type "int" :data 2}}) 
         {:function {:status 0
                     :result 3}
          :id 42})))

(deftest test-generic-request-error
  (is (= (mock-request {:function {:type "function"
                                   :name "+" 
                                   :args ["eins"]} 
                        :eins {:type "int" :data "a"}}) 
         {:function {:status 1
                     :msg (try (+ "a") 
                           (catch Exception e (.getMessage e)))}})))

;;; Conexp functions
;; one test per accepted data type

(deftest test-single-context-request
  (is (= (mock-request {:function {:type "function"
                                   :name "concepts"
                                   :args ["ctx1"]}
                        :ctx1 {:type "ctx"
                               :data (slurp "testing-data/Animals.ctx")}})
         {:function {:status 0
                     :result (mapv 
                              #(mapv vec %) 
                              (concepts (read-context 
                                         "testing-data/Animals.ctx")))}})))

;;; Conexp Shorthands

;;;

nil
