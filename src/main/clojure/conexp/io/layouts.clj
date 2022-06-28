;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.layouts
  "Implements IO for layouts."
  (:use conexp.base
        conexp.io.util
        conexp.io.latex
        conexp.layouts.util
        conexp.layouts.base)
  (:require clojure.string
            [clojure.data.json :as json])
  (:import [java.io PushbackReader]))

;;; Input format dispatch

(define-format-dispatch "layout")
(set-default-layout-format! :simple)

;;; Formats

;; Simple conexp-clj Format for Layout

(add-layout-input-format :simple
                         (fn [rdr]
                           (= "conexp-clj simple" (read-line))))

(define-layout-output-format :simple
  [layout file]
  (with-out-writer file
    (binding [*print-length* nil]
      (println "conexp-clj simple")
      (prn {:layout [(positions layout)
                     (connections layout)]}))))

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
        node-number (comp inc (seq-positions nodes)),
        annotation  (annotation layout)]
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
              g (clojure.string/split (second (annotation n)) #", ")
              :when (not-empty g)]
        (println (str "Object: " (node-number n) ", " g)))
      ;; Attribute
      (doseq [n nodes,
              m (clojure.string/split (first (annotation n)) #", ")
              :when (not-empty m)]
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
    (when-not (<= (count lines) 5)
      (illegal-argument "File " file " does not contain a valid :text layout."))
    (when-not (= (first (last lines)) "EOF")
      (illegal-state "Layout :text format must end with EOF."))
    (let [;; Entries
          entries (apply hash-map (mapcat #(list (second (re-matches #"^([^:]*): .*$" (first %)))
                                                 %)
                                          lines)),
          ;; Node
          pos  (reduce (fn [map line]
                         (let [[node x y] (get-arguments line "Node")]
                           (assoc map node [(Double/parseDouble x), (Double/parseDouble y)])))
                       {}
                       (entries "Node")),
          ;; Edge
          conn (reduce (fn [set line]
                         (conj set (get-arguments line "Edge")))
                       #{}
                       (entries "Edge")),
          ;; Compute lower and upper elements
          [below,above] (compute-below-above conn),
          ;; Object
          objs (reduce (fn [map line]
                         (let [[node obj] (get-arguments line "Object")]
                           (update-in map [node] conj obj)))
                       {}
                       (entries "Object")),
          ;; Attribute
          atts (reduce (fn [map line]
                         (let [[node att] (get-arguments line "Attribute")]
                           (update-in map [node] conj att)))
                       {}
                       (entries "Attribute")),
          ;; Layout construction
          nodes (map-by-fn (fn [node]
                             [(set (mapcat objs (below node))),
                              (set (mapcat atts (above node)))])
                           (keys pos))]
      (make-layout (into {} (for [[number, coord] pos]
                              [(nodes number), coord]))
                   (set-of [(nodes x) (nodes y)]
                           [[x y] conn])))))

;;; TikZ

(define-layout-output-format :tikz
  [layout file]
  (with-out-writer file
    (println (latex layout :tikz))))

;;; FCA-style

(define-layout-output-format :fca-style
  [layout file]
  (unsupported-operation "Output in :fca-style is not yet supported."))

;;; Json
(define-layout-output-format :json
  [layout file]
  (let [vertex-pos (positions  layout)
        sorted-vertices (sort #(let [[x_1 y_1] (vertex-pos %1),
                                     [x_2 y_2] (vertex-pos %2)]
                                 (or (< y_1 y_2)
                                     (and (= y_1 y_2)
                                          (< x_1 x_2))))
                              (nodes layout)),
        vertex-idx      (into {}
                              (map-indexed (fn [i v] [v i])
                                           sorted-vertices))]
    (let [nodes (map-invert vertex-idx)
          pos (into {}
                    (for [n sorted-vertices]
                      [(vertex-idx n), (vertex-pos n)]))
          edges (reduce 
                 (fn [ncoll [k v]] 
                   (assoc ncoll k (conj (get ncoll k) v)))
                 {}
                 (for [[A B] (connections layout)]
                   [(vertex-idx A),(str(vertex-idx B))]))
          v (into {}
                  (for [n sorted-vertices]
                    [(vertex-idx n), ((valuations layout) n)]))
          ann (into {}
                    (for [n sorted-vertices]
                      [(vertex-idx n), ((annotation layout) n)]))]
      (->> (hash-map :nodes nodes
                     :pos pos
                     :edges edges
                     :valuations v
                     :shorthand-annotation ann)
           json/write-str
           (spit file)))))

(define-layout-input-format :json
  [file]
  (with-in-reader file
    (let [file-content (json/read *in* :key-fn keyword)
          positions (into {} 
                          (map #(vector (mapv set (get (:nodes file-content) (key %))) 
                                        (val %)) 
                               (:pos file-content)))
          connections (map #(vector 
                             (mapv set (get (:nodes file-content) (first %))) 
                             (mapv set (get (:nodes file-content) (keyword (second %)))))
                           (for [A (keys (:edges file-content)) 
                                 B (get (:edges file-content) A)] 
                             [A B]))]
      (make-layout positions connections))))

;;;

nil
