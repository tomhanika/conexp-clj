;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.api.shorthands
  "Convenience wrappers for the JSON-RPC API, mainly for the web GUI.  A browser
  cannot pass Clojure functions over JSON, so operations that would normally
  take a function argument (choosing a layout algorithm, choosing a node
  valuation) are exposed here as single calls selected by a name string.  Every
  public var in this namespace is whitelisted for the API (see
  conexp.api.namespace)."
  (:use conexp.main)
  (:require [conexp.layouts.base    :refer [update-valuations]]
            [conexp.layouts.layered :refer [simple-layered-layout as-chain]]
            [conexp.layouts.dim-draw :refer [dim-draw-layout]]
            [conexp.layouts.freese  :refer [freese-layout]]
            [conexp.layouts.force   :refer [force-layout]]
            [conexp.fca.metrics     :refer [concept-stability separation-index
                                            concept-probability elements-distributivity
                                            elements-modularity *fast-computation*]]))

(apply use conexp-clj-namespaces)

;;; Layouts by name -----------------------------------------------------------

(defn- layout-for
  "Returns a layout of `lat` chosen by `layout-name` (defaults to standard)."
  [lat layout-name]
  (case layout-name
    "inf-additive"    (inf-additive-layout lat)
    "simple-layered"  (simple-layered-layout lat)
    "as-chain"        (as-chain lat)
    "dim-draw"        (dim-draw-layout lat)
    "freese"          (freese-layout lat)
    "force"           (force-layout (standard-layout lat))
    (standard-layout lat)))

(defn sh-lattice-layout
  "Returns a layout of the concept lattice of `ctx`, using the named layout
  algorithm (\"standard\", \"inf-additive\", \"simple-layered\", \"as-chain\",
  \"dim-draw\", \"freese\", \"force\")."
  [ctx layout-name]
  (layout-for (concept-lattice ctx) layout-name))

;;; Node valuations by name ---------------------------------------------------

(defn- valuation-mode
  "Returns a (lattice, concept) -> value function for the named valuation
  (mirrors the desktop GUI's valuation modes)."
  [valuation-name]
  (case valuation-name
    "count-ints"       (fn [_ c] (count (second c)))
    "count-exts"       (fn [_ c] (count (first c)))
    "modularity"       (fn [lat c] (float (elements-modularity lat c)))
    "distributivity"   (fn [lat c] (float (elements-distributivity lat c)))
    "separation-index" (fn [lat c] (float (separation-index (extract-context-from-bv lat) c)))
    "support"          (fn [lat c] (float (/ (count (first c))
                                             (apply max (map (comp count first)
                                                             (base-set lat))))))
    "stability"        (fn [lat c] (float (concept-stability (extract-context-from-bv lat) c)))
    "probability"      (fn [lat c] (binding [*fast-computation* true]
                                     (if (empty? (second c))
                                       1
                                       (float (concept-probability (extract-context-from-bv lat) c)))))
    ;; "none" / unknown -> no valuation
    (fn [_ _] nil)))

(defn sh-layout-with-valuation
  "Returns a layout of the concept lattice of `ctx` (chosen by `layout-name`)
  whose node valuations are set from the named valuation."
  [ctx layout-name valuation-name]
  (let [lat    (concept-lattice ctx)
        layout (layout-for lat layout-name)
        mode   (valuation-mode valuation-name)]
    (update-valuations layout (partial mode lat))))

;;; Context algebra by name ---------------------------------------------------

(defn sh-context-op
  "Applies the named unary context operation to `ctx`."
  [ctx op-name]
  (case op-name
    "clarify-objects"    (clarify-objects ctx)
    "clarify-attributes" (clarify-attributes ctx)
    "clarify"            (clarify-context ctx)
    "reduce-objects"     (reduce-objects ctx)
    "reduce-attributes"  (reduce-attributes ctx)
    "reduce"             (reduce-context ctx)
    "transitive-closure" (context-transitive-closure ctx)
    "dual"               (dual-context ctx)
    "invert"             (invert-context ctx)
    (illegal-argument "Unknown context operation: " op-name)))

(defn sh-context-binop
  "Applies the named binary context operation to `ctx1` and `ctx2`."
  [ctx1 ctx2 op-name]
  (case op-name
    "sum"          (context-sum ctx1 ctx2)
    "product"      (context-product ctx1 ctx2)
    "semiproduct"  (context-semiproduct ctx1 ctx2)
    "xia-product"  (context-xia-product ctx1 ctx2)
    "union"        (context-union ctx1 ctx2)
    "intersection" (context-intersection ctx1 ctx2)
    "composition"  (context-composition ctx1 ctx2)
    "apposition"   (context-apposition ctx1 ctx2)
    "subposition"  (context-subposition ctx1 ctx2)
    (illegal-argument "Unknown binary context operation: " op-name)))

;;;

nil
