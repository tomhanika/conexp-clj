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
        conexp.io.json
        conexp.layouts.util
        conexp.layouts.base)
  (:require clojure.string
            [clojure.data.json :as json]
            [clojure.data.xml :as xml]
            [clojure.set :refer [difference union subset? intersection map-invert]])
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
  (with-out-writer file
    (println (latex layout :fca-style))))


(defn- xml-layout-groups [layout]
  (for [[name pos] (positions layout)] (xml/element :mxCell {:id (str "Group" name)
                                                             :value ""
                                                             :style "group"
                                                             :vertex "1"
                                                             :connectable "0"
                                                             :parent "1"}
                                                    (xml/element :mxGeometry {:x (str (* (first pos) 100))
                                                                              :y (str (* (second pos) 100))
                                                                              :width "170"
                                                                              :height "90"
                                                                              :as "geometry"})))
)

(defn- xml-layout-nodes [layout]
  (for [[name pos] (positions layout)] (xml/element :mxCell {:id (str name)
                                                             :value ""
                                                             :style "ellipse;whiteSpace=wrap;html=1;aspect=fixed;"
                                                             :parent (str "Group" name)
                                                             :vertex "1"}
                                                    (xml/element :mxGeometry {:width "80"
                                                                              :height "80"
                                                                              :as "geometry"})))
)

(defn- xml-layout-edges [layout]
  (for [[source target] (connections layout)] (xml/element :mxCell {:id (str "Edge" source target)
                                                                    :value ""
                                                                    :style "endArrow=none;html=1;rounded=0;"
                                                                    :edge "1"
                                                                    :parent "1"
                                                                    :source source
                                                                    :target target}
                                                           (xml/element :mxGeometry {:width "50"
                                                                                     :height "50"
                                                                                     :relative "1"
                                                                                     :as "geometry"})))
)

(defn- xml-layout-intent-labels [layout]
  (for [[name labels] (concept-lattice-annotation layout)] (xml/element :mxCell {:id (str "Attributes" name)
                                                                                 :value (str (first labels))
                                                                                 :style "text;html=1;align=left;verticalAlign=middle;whiteSpace=wrap;rounded=0;fontSize=20;"
                                                                                 :vertex "1"
                                                                                 :parent (str "Group" name)}
                                                                        (xml/element :mxGeometry {:x "80"
                                                                                                  :y "0"
                                                                                                  :width "200"
                                                                                                  :height "30"
                                                                                                  :as "geometry"})))
)

(defn- xml-layout-extent-labels [layout]
  (for [[name labels] (concept-lattice-annotation layout)] (xml/element :mxCell {:id (str "Objectss" name)
                                                                                 :value (str (second labels))
                                                                                 :style "text;html=1;align=left;verticalAlign=middle;whiteSpace=wrap;rounded=0;fontSize=20;"
                                                                                 :vertex "1"
                                                                                 :parent (str "Group" name)}
                                                                        (xml/element :mxGeometry {:x "80"
                                                                                                  :y "50"
                                                                                                  :width "200"
                                                                                                  :height "30"
                                                                                                  :as "geometry"})))
)



(define-layout-output-format :xml
  [layout file]
  (let [xml-layout (xml/element :mxGraphModel {:dx "2068" 
                                               :dy "1119" 
                                               :grid "1" 
                                               :gridSize "10" 
                                               :guides "1" 
                                               :tooltips "1"
                                               :connect "1"
                                               :arrows "1"
                                               :fold "1"
                                               :page "1"
                                               :pageScale "1"}
                                (xml/element :root {}
                                             (xml/element :mxCell {:id "0"})
                                             (xml/element :mxCell {:id "1"
                                                                   :parent "0"})
                                             (xml-layout-groups layout)
                                             (xml-layout-nodes layout)
                                             (xml-layout-edges layout)
                                             (xml-layout-intent-labels layout)
                                             (xml-layout-extent-labels layout)))]

  (with-out-writer file (println (xml/indent-str xml-layout))))

)


;; Json helpers

(defn json->nodes
  [json-layout]
  (try
    (reduce
     (fn [ncoll [k v]]
       (assoc ncoll k
              (mapv set v)))
     {}
     (apply conj (:nodes json-layout)))
    (catch java.lang.IllegalArgumentException _
      (apply conj (:nodes json-layout)))))

(defn json->positions
  "Transforms the positions from json format to a map, using the id of the nodes."
  [json-positions nodes]
  (into {} 
        (map #(vector (get nodes (key %)) 
                      (val %)) 
             json-positions)))

(defn json->connections
  "Transforms the connections from json format to a map, using the id of the nodes."
  [json-connections nodes]
  (map #(vector 
         (get nodes (first %)) 
         (get nodes (keyword (second %))))
       (for [A (keys json-connections) 
             B (get json-connections A)] 
         [A B])))

(defn- valuation-function
  "Maps the json-valuations to the correct node."
  [json-valuations nodes]
  (fn [concept]
    (get (into {}
               (map #(vector (get nodes (key %))
                             (val %))
                    json-valuations))
         concept)))

(defn json->layout
  "Returns a Layout object for the given json layout."
  [json-layout]
  (let [nodes (json->nodes json-layout)
        positions (apply conj (:positions json-layout))
        edges (apply conj (:edges json-layout))
        valuations (apply conj (:valuations json-layout))
        positions (json->positions positions nodes)
        connections (json->connections edges nodes)
        layout (make-layout positions connections)]
    (if (every? #(nil? (val %)) valuations)
      layout
      (update-valuations layout (valuation-function valuations nodes)))))

(defn layout->json
  ""
  [layout]
  (let [vertex-pos (positions layout)
        sorted-vertices (sort #(let [[x_1 y_1] (vertex-pos %1),
                                     [x_2 y_2] (vertex-pos %2)]
                                 (or (< y_1 y_2)
                                     (and (= y_1 y_2)
                                          (< x_1 x_2))))
                              (nodes layout)),
        vertex-idx (into {}
                         (map-indexed (fn [i v] [v i])
                                      sorted-vertices))]
    (let [nodes (map #(hash-map (key %) (val %)) (map-invert vertex-idx))
          pos (into []
                    (for [n sorted-vertices]
                      {(vertex-idx n), (vertex-pos n)}))
          edges (map #(hash-map (key %) (val %))
                     (reduce 
                      (fn [ncoll [k v]] 
                        (assoc ncoll k (conj (get ncoll k) v)))
                      {}
                      (for [[A B] (connections layout)]
                        [(vertex-idx A),(str(vertex-idx B))])))
          v (into []
                  (for [n sorted-vertices]
                    {(vertex-idx n), ((valuations layout) n)}))
          
          ann (into []
                    (for [n sorted-vertices]
                      {(vertex-idx n), ((annotation layout) n)}))]
      (hash-map :nodes nodes
                :positions pos
                :edges edges
                :valuations v
                :shorthand-annotation ann))))

;;; Json Format (src/main/resources/schemas/layout_schema_v1.0.json)

(add-layout-input-format :json
                         (fn [rdr]
                           (try (json-object? rdr)
                                (catch Exception _))))

(define-layout-output-format :json
  [layout file]
  (with-out-writer file 
    (print (json/write-str (layout->json layout)))))

(define-layout-input-format :json
  [file]
  (with-in-reader file
    (let [json-layout (json/read *in* :key-fn keyword)
          schema-file "schemas/layout_schema_v1.0.json"]
      (assert (matches-schema? json-layout schema-file)
              (str "The input file does not match the schema given at " schema-file "."))
      (json->layout json-layout))))

;;;

nil
