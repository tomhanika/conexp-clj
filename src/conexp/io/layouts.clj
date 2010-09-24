;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.layouts
  (:use conexp.base
	conexp.io.util
	conexp.layouts.base)
  (:require clojure.string)
  (:import [java.io PushbackReader]))

(ns-doc "Implements IO for layouts.")

;;; Input format dispatch

(define-format-dispatch "layout")
(set-default-layout-format! :simple)

;;; Formats

;; Simple conexp-clj Format for Layout

(add-layout-input-format :simple
			 (fn [rdr]
			   (= "conexp-clj simple" (.readLine rdr))))

(define-layout-output-format :simple
  [layout file]
  (with-out-writer file
    (println "conexp-clj simple")
    (prn {:layout [(positions layout)
		   (connections layout)]})))

(define-layout-input-format :simple
  [file]
  (with-in-reader file
    (let [_        (get-line),
	  hash-map (binding [*in* (PushbackReader. *in*)]
		     (read)),
	  layout   (:layout hash-map)]
      (when-not layout
	(illegal-argument "File " file " does not contain a layout."))
      (apply make-layout layout))))

;;; ConExp Layout format

'TODO

;;; Text

(defn- seq-positions
  "Returns a map from elements of seq to their positions."
  [seq]
  (loop [seq   seq,
         index 0,
         map   {}]
    (if (empty? seq)
      map
      (recur (rest seq)
             (inc index)
             (assoc map (first seq) index)))))

(define-layout-output-format :text
  [layout file]
  (when-not (concept-lattice-layout? layout)
    (illegal-argument "Cannot store layout in :text format which does "
                      "not come from a concept lattice."))
  (let [nodes       (vec (keys (positions layout))),
        node-number (comp inc (seq-positions nodes))]
    (with-out-writer file
      ;; Node
      (doseq [n nodes]
        (let [[x y] ((positions layout) n)]
          (println (str "Node: " (node-number n) ", " x ", " y))))
      ;; Edge
      (doseq [[x y] (connections layout)]
        (println (str "Edge: " (node-number x) ", " (node-number y))))
      ;; Object
      (doseq [n nodes,
              g (first n)]
        (println (str "Object: " (node-number n) ", " g)))
      ;; Attribute
      (doseq [n nodes,
              m (second n)]
        (println (str "Attribute: " (node-number n) ", " m)))
      (println "EOF"))))

(add-layout-input-format :text
                         (fn [_] (re-find #"^Node: " (read-line))))

(defn- get-arguments
  [line start]
  (let [[first args] (rest (re-matches #"^([^:]*): (.*)$" line))]
    (when-not (= first start)
      (illegal-argument "Expected " start ", got " first))
    (clojure.string/split args #", ")))

(define-layout-input-format :text
  [file]
  (let [lines (partition-by #(second (re-matches #"^([^:]*): .*$" %))
                            (line-seq (reader file)))]
    (when-not (= 5 (count lines))
      (illegal-argument "File " file " does not contain a valid :text layout."))
    (let [;; Node
          pos  (reduce (fn [map line]
                         (let [[node x y] (get-arguments line "Node")]
                           (assoc map node [(Double/parseDouble x), (Double/parseDouble y)])))
                       {}
                       (nth lines 0)),
          ;; Edge
          conn (reduce (fn [set line]
                         (conj set (get-arguments line "Edge")))
                       #{}
                       (nth lines 1)),
          ;; Object
          objs (reduce (fn [map line]
                         (let [[node obj] (get-arguments line "Object")]
                           (update-in map [node] conj obj)))
                       {}
                       (nth lines 2)),
          ;; Attribute
          atts (reduce (fn [map line]
                         (let [[node att] (get-arguments line "Attribute")]
                           (update-in map [node] conj att)))
                       {}
                       (nth lines 3)),
          ;; Layout construction
          nodes (map-by-fn (fn [node]
                             [(set (objs node)), (set (atts node))])
                           (keys pos))]
      (make-layout (into {} (for [[number, coord] pos]
                              [(nodes number), coord]))
                   (set-of [(nodes x) (nodes y)]
                           [[x y] conn])))))

;;; FCA-style

(define-layout-output-format :fca-style
  [layout file]
  (unsupported-operation "Output in :fca-style is not yet supported."))

;;;

nil
