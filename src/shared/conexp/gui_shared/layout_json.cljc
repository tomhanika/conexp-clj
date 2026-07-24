;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui-shared.layout-json
  "Shared (Clojure + ClojureScript) translation of the `layout->json` payload
  produced by conexp.io.layouts into a flat render model the SVG diagram view
  consumes.  Kept as .cljc so the browser and the JVM agree on the contract.

  The `layout->json` shape is, per field, a vector of single-entry maps keyed by
  node index (integer on the JVM, string once round-tripped through JSON):

    {:nodes                [{idx [extent intent]} ...]
     :positions            [{idx [x y]} ...]
     :edges                [{idx [target-idx ...]} ...]      ; cover edges only
     :valuations           [{idx value} ...]
     :shorthand-annotation [{idx [attribute-label object-label]} ...]}"
  (:require [clojure.string :as str]))

(defn- id-str
  "Normalises a node index (int, keyword, or string) to a string id, so node
  keys and edge targets compare equal regardless of platform/JSON round-trip."
  [k]
  (cond (keyword? k) (name k)
        :else        (str k)))

(defn- index-map
  "Merges a layout->json field ([{idx val} ...]) into a {id-string val} map."
  [entries]
  (reduce (fn [m e]
            (reduce-kv (fn [m k v] (assoc m (id-str k) v)) m e))
          {}
          entries))

(defn render-model
  "Turns a `layout->json` payload into a flat render model:

    {:nodes [{:id, :x, :y, :attr-label, :obj-label, :valuation,
              :attribute-concept?, :object-concept?} ...]
     :edges [[from-id to-id] ...]                 ; cover edges, id strings
     :bounds {:min-x :min-y :max-x :max-y}}"
  [layout]
  (let [positions (index-map (:positions layout))
        anns      (index-map (:shorthand-annotation layout))
        vals      (index-map (:valuations layout))
        edge-map  (index-map (:edges layout))
        node-of   (fn [id]
                    (let [[x y]          (get positions id)
                          [attr-l obj-l] (get anns id)
                          attr-l         (or attr-l "")
                          obj-l          (or obj-l "")]
                      {:id                 id
                       :x                  x
                       :y                  y
                       :attr-label         attr-l
                       :obj-label          obj-l
                       :valuation          (get vals id)
                       :attribute-concept? (not (str/blank? attr-l))
                       :object-concept?    (not (str/blank? obj-l))}))
        nodes     (mapv node-of (keys positions))
        edges     (vec (for [[from tos] edge-map, to tos] [from (id-str to)]))
        xs        (map :x nodes)
        ys        (map :y nodes)]
    {:nodes  nodes
     :edges  edges
     :bounds (when (seq nodes)
               {:min-x (apply min xs) :max-x (apply max xs)
                :min-y (apply min ys) :max-y (apply max ys)})}))

(defn neighbour-fns
  "Given a render model, returns {:uppers f, :lowers f} where each maps a node
  id to the seq of its cover-neighbour ids -- the inputs conexp.layouts.movement
  expects for computing co-moving node sets during a drag.  Edges point from a
  node to its upper covers."
  [{:keys [nodes edges]}]
  (let [ids     (set (map :id nodes))
        uppers  (reduce (fn [m [from to]] (update m from (fnil conj []) to)) {} edges)
        lowers  (reduce (fn [m [from to]] (update m to (fnil conj []) from)) {} edges)]
    {:ids    ids
     :uppers (fn [id] (get uppers id []))
     :lowers (fn [id] (get lowers id []))}))
