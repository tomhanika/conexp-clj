(ns conexp.io.implications
  (:require [clojure.data.json :as json]
            [conexp.fca.implications :refer :all]
            [conexp.io.util          :refer :all]))

;;; Input format dispatch

(define-format-dispatch "implication")
(set-default-implication-format! :json)

;;; Formats

;; Json helpers

(defn implication->json
  [impl]
  (json/write-str {:premise (premise impl)
                   :conclusion (conclusion impl)}))

;;; TODO: adjustments needed
(defn implications->json
  [impl]
  (map implication->json impl))

(defn json->implication
  [json]
  (make-implication (into #{} (:premise json))
                    (into #{} (:conclusion json))))

(defn json->implications
  [json]
  (let [impl (:implications json)]
    (map json->implication impl)))

;; Json Format

(add-implication-input-format :json
                               (fn [rdr]
                                 (= "json" (read-line))))

;;; TODO: adjustments needed
(define-implication-output-format :json
  [impl file]
  (with-out-writer file
    (println "json")
    (prn (implications->json impl))))

(define-implication-input-format :json
  [file]
  (with-in-reader file
    (let [_ (get-line)
          impl (json/read *in* :key-fn keyword)]
      (json->implications impl))))
