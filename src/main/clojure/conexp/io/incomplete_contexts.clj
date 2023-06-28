;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.incomplete-contexts
  "Implements IO for Incomplete Contexts."
  (:require 
   [conexp.base :refer [defalias illegal-argument unsupported-operation set-of]]
   [conexp.fca.incomplete-contexts.incomplete-contexts :refer :all]
   [conexp.fca.incomplete-contexts.conexp-interop :refer :all]
   [conexp.io.util :refer [define-format-dispatch with-out-writer with-in-reader get-line get-lines]])
  (:import [java.io PushbackReader]))

;;; Input format dispatch

(define-format-dispatch "incomplete-context")
(set-default-incomplete-context-format! :json)

(defalias read-?-context read-incomplete-context)
(defalias write-?-context write-incomplete-context)


;; Data Table Format

;; Note the following restrictions:
;;   - we need at least one object and at least two attributes (to
;;     reliably determine the file type)
;;   - the first line must contain of the attributes in the correct order
;;   - if the subsequent lines have the same number of entries as the
;;     first, the resulting mv-context will have the line number as
;;     objects,
;;   - if the subsequent lines have one more element as the first, the first
;;     entry will be the object for that line

(add-incomplete-context-input-format :data-table
                             (fn [rdr]
                               (try
                                (re-matches #"^[^,]+,[^,]+.*$" (read-line))
                                (catch Exception _))))

(define-incomplete-context-output-format :data-table
  [mv-context file]
  (with-out-writer file
    (when (> 2 (count (attributes mv-context)))
      (unsupported-operation
       "Cannot store many-valued contexts with less then 2 attributes in format :data-table."))
    (when (= 0 (count (objects mv-context)))
      (unsupported-operation
       "Cannot store many-valued context without objects in format :data-table."))
    (let [write-comma-line (fn [things]
                             (cond
                              (empty? things) nil,
                              (= 1 (count things)) (prn (first things)),
                              :else (do (pr (first things))
                                        (print ",")
                                        (recur (rest things)))))]
      (write-comma-line (attributes mv-context))
      (doseq [g (objects mv-context)]
        (write-comma-line (cons g (map #((incidence mv-context) [g %])
                                       (attributes mv-context))))))))

(define-incomplete-context-input-format :data-table
  [file]
  (with-in-reader file
    (let [read-comma-line (fn []
                            (try
                              (let [line (get-line)]
                                (read-string (str "(" line ")")))
                              (catch java.io.EOFException _
                                nil))),
          attributes      (read-comma-line),
          lines           (doall
                           (take-while #(not (nil? %))
                                       (repeatedly read-comma-line))),
          line-lengths    (set-of (count line) [line lines])]
      (when (< 1 (count line-lengths))
        (illegal-argument "Many-Valued Context in file " file " has lines of different length."))
      (when (and (not= (count attributes) (first line-lengths))
                 (not= (inc (count attributes)) (first line-lengths)))
        (illegal-argument
         "Number of values in lines in file " file " does not match given attributes.\n"
         "Number of values given should be equal or once more to the number of attributes."))
      (let [lines      (if (not= (first line-lengths) (count attributes))
                         lines
                         (map #(cons %1 %2) (iterate inc 0) lines)),
            objects    (map first lines),
            object-set (set objects)]
        (when (not= (count objects) (count object-set))
          (illegal-argument "Given file " file " contains double entries for objects."))
        (let [interpretation  (into {}
                                    (for [line lines
                                          :let [g (first line),
                                                values (rest line),
                                                mapped_values (map map-true-false-unknown-to-x-o-? values)],
                                          [m w] (map vector attributes mapped_values)]
                                      [[g m] w]))]
          (make-incomplete-context object-set attributes interpretation))))))



;; Burmeister Format

(add-incomplete-context-input-format :burmeister
                          (fn [rdr]
                            (= "B" (read-line))))


(defn convert-incomplete-context-incidences-to-burmeister-output
  "converts x to X; o to . and ? to ? for burmeister output"
  [in]
  (if (= in  known-true) "X" (if (= in known-false) "." (if (= in unknown) "?" (throw "input not x,o,?")))))


(define-incomplete-context-output-format :burmeister
  [ctx file]
  (with-out-writer file
    (println \B)
    (println)
    (println (count (objects ctx)))
    (println (count (attributes ctx)))
    (println)
    (doseq [g (objects ctx)] (println g))
    (doseq [m (attributes ctx)] (println m))
    (let [inz (incidence ctx)]
      (doseq [g (objects ctx)]
        (doseq [m (attributes ctx)]
          (print (convert-incomplete-context-incidences-to-burmeister-output (inz [g m]))))
        (println)))))


(define-incomplete-context-input-format :burmeister
  [file]
  (with-in-reader file
    (let [_                    (get-lines 2)    ; "B\n\n", we don't support names

          number-of-objects    (Integer/parseInt (.trim (get-line)))
          number-of-attributes (Integer/parseInt (.trim (get-line)))

          _                    (get-line)         ; "\n"

          seq-of-objects       (get-lines number-of-objects)
          seq-of-attributes    (get-lines number-of-attributes)]
      (loop [objs seq-of-objects
             incidence {}]
        (if (empty? objs)
          (make-incomplete-context (set seq-of-objects)
                           (set seq-of-attributes)
                           incidence)
          (let [line (get-line)]
            (recur (rest objs)
                   (into incidence
                         (for [idx-m (range number-of-attributes)]
                           [[(first objs) (nth seq-of-attributes idx-m)] (map-true-false-unknown-to-x-o-? (nth line idx-m))])))))))))




;;; Json
(defn icxt->json
  "Returns a formal context as a map that can easily be converted into json format."
  [cxt]
  (let [icxt (to-incomplete-context cxt)]
    {:attributes (attributes icxt) 
     :objects (objects icxt)
     :certain-incidences (mapv first (true-incidence icxt))
     :possible-incidences (mapv first (true-or-unknown-incidences icxt))}
    ))



(defn json->icxt
  "Returns a Context object for the given json context."
  [json-icxt]
  (let [attributes (:attributes json-icxt)
        objects (:objects json-icxt)
        certain-incidences (into {} (map #(vector % known-true) (:certain-incidences json-icxt)))
        possible-incidences (into {} (map #(vector % unknown) (:possible-incidences json-icxt)))
        hm (into {} (map #(vector % known-false) (clojure.math.combinatorics/cartesian-product objects attributes)))
        incidence (reduce into [hm possible-incidences certain-incidences])
        ]
    (make-incomplete-context objects attributes incidence)))


(add-incomplete-context-input-format :json (fn [rdr]
                                             (try (conexp.io.json/json-object? rdr)
                                                  (catch Exception _))))

(define-incomplete-context-output-format :json
  [cxt file]
  (with-out-writer file
    (print (clojure.data.json/write-str (icxt->json cxt)))))

(define-incomplete-context-input-format :json
  [file]
  (with-in-reader file 
    (let [file-content (clojure.data.json/read *in* :key-fn keyword)
          json-cxt file-content]
      (json->icxt json-cxt))))
nil
