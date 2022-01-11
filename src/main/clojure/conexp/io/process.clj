(ns conexp.io.process
  (:require [conexp.io.contexts     :refer :all]
            [conexp.io.implications :refer :all]
            [conexp.io.lattices     :refer :all]
            [conexp.io.util         :refer :all]
            [clojure.data.json      :as json]))

;;; Input format dispatch

(define-format-dispatch "process")
(set-default-process-format! :json)

;;; Formats

;; Json Format

(add-process-input-format :json
                          (fn [rdr]
                            (= "json" (read-line))))

(define-process-output-format :json
  [[ctx concepts implications] file]
  (with-out-writer file
    (println "json")
    (print
     (json/write-str {:context (ctx->json ctx)
                      :concepts (concepts->json concepts)
                      :implication_sets [(implications->json implications)]}))))

(define-process-input-format :json
  [file]
  (with-in-reader file
    (let [_ (get-line)
          json-process (json/read *in* :key-fn keyword)
          json-ctx (:context json-process)
          json-concepts (:concepts json-process)
          json-implications (:implication_sets json-process)]
      [(json->ctx (:formal_context json-ctx)) 
       (map json->concept (:formal_concepts json-concepts)) 
       (map json->implications json-implications)]))
  )
