(ns conexp.fca.incomplete-contexts.conexp-interop
  (:require [conexp.fca.contexts :as cxt]
            [conexp.fca.implications :as impls]
            [conexp.base :refer [cross-product defalias]]
            [conexp.fca.incomplete-contexts.incomplete-contexts :as icxt]
            ))


(defn incomplete-context-from-formal-context
  "convert a formal context to an incomplete context"
  [formal-context]
  (let [objs (cxt/objects formal-context)
        atts (cxt/attributes formal-context)
        inz (cxt/incidence formal-context)
        fn (fn [g m inz] (if (contains? inz [g m]) icxt/known-true icxt/known-false))
        new-inz (loop [hash  (transient {})
                   items (cross-product objs atts)]
                              (if (empty? items)
                                (persistent! hash)
                                 (let [[g m] (first items)]
                                     (recur (if (and (contains? objs g)
                                                     (contains? atts m))
                                              (assoc! hash [g m] (fn g m inz))
                                              hash)
                                            (rest items)))))]
    (icxt/make-incomplete-context objs atts new-inz)))

(defalias formal-context->incomplete-context incomplete-context-from-formal-context)

(defn formal-context-from-incomplete-context
  ""
  [partial-context]
  {:pre [(empty? (filter #(= (second %) icxt/unknown) (icxt/incidence partial-context)))]}
  (let [objs (icxt/objects partial-context)
        atts (icxt/attributes partial-context)
        inz (icxt/incidence partial-context)
        new-inz (->> inz
                     (filter #(= (second %) icxt/known-true))
                     (map #(first %))
                     (into #{}))]
    (cxt/make-context objs atts new-inz)))

(defalias incomplete-context->formal-context formal-context-from-incomplete-context)


(defn incomplete-context->certain-incidences-context
  "Given an incomplete context (G,M,V,I) return the formal context (G,M,J) where (g,m) in J iff I(g,m) = x"
  [cxt]
  (let [objs (icxt/objects cxt)
        atts (icxt/attributes cxt)]
    (cxt/make-context objs atts (map first (icxt/true-incidence cxt)))))

(defn incomplete-context->possible-incidences-context
  "Given an incomplete context (G,M,V,I) return the formal context (G,M,J) where (g,m) in J iff I(g,m) in {x,?}"
  [cxt]
  (let [objs (icxt/objects cxt)
        atts (icxt/attributes cxt)]
    (cxt/make-context objs atts (map first (concat (icxt/true-incidence cxt)
                                                    (icxt/unknown-incidence-set cxt))))))


(defn to-incomplete-context
  [cxt]
  (cond
      (satisfies? icxt/Incomplete-Context-Protocol cxt)
      cxt
      (satisfies? cxt/Context cxt)
      (formal-context->incomplete-context cxt)
      true
      (throw (Exception.  "input was neither an incomplete context nor a formal context")))
  )

(defn to-formal-context
  [cxt]
  (cond
      (satisfies? icxt/Incomplete-Context-Protocol cxt)
      (incomplete-context->formal-context cxt)
      (satisfies? cxt/Context cxt)
      cxt
      true
      (throw (Exception.  "input was neither an incomplete context nor a formal context")))
  )

