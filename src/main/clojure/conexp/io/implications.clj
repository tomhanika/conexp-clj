(ns conexp.io.implications
  (:require [clojure.data.json :as json]
            [conexp.fca.implications :refer :all]
            [conexp.io.util          :refer :all]))

;;; Input format dispatch

(define-format-dispatch "implication")
(set-default-implication-format! :json)

;;; Formats

;; Json helpers

(defn- implication->json
  [impl]
  {:premise (premise impl)
   :conclusion (conclusion impl)})

(defn implications->json
  [impl]
  {:implications
     (mapv implication->json impl)})

(defn- json->implication
  [json-impl]
  (make-implication (into #{} (:premise json-impl))
                    (into #{} (:conclusion json-impl))))

(defn json->implications
  [json]
  (let [impl (:implications json)]
    (map json->implication impl)))

;; Json Format

(add-implication-input-format :json
                               (fn [rdr]
                                 (= "json impl" (read-line))))

(define-implication-output-format :json
  [impl file]
  (with-out-writer file
    (println "json impl")
    (print (json/write-str (implications->json impl)))))

(define-implication-input-format :json
  [file]
  (with-in-reader file
    (let [_ (get-line)
          impl (json/read *in* :key-fn keyword)]
      (json->implications impl))))
