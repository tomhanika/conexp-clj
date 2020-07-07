;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.smeasure
  (:require [conexp.base :refer :all]
            [conexp.fca.contexts :refer :all]
            [conexp.fca.concept-transform :refer :all]
            [conexp.fca.cover :refer [generate-concept-cover]]
            [clojure.math.combinatorics :as comb]
            [loom.graph :as lg] [loom.alg :as la]
            [clojure.core.reducers :as r]))

(defprotocol Smeasure
  (context [sm] "Returns the original context that is measured.")
  (scale   [sm] "Returns the scale that measures the context.")
  (measure [sm] "Returns the scale measure map that associates objects of context with objects of scale."))

(deftype ScaleMeasure [context scale measure]
  Object
  (equals [this other]
    (and (= (class this) (class other))
         (= (.context this) (.context ^ScaleMeasure other))
         (every? #(= ((.measure this) %)
                     ((.measure ^ScaleMeasure other) %))
                 (objects context))))
  (hashCode [this]
    (hash-combine-hash ScaleMeasure context scale measure))
  ;;
  Smeasure
  (context [this] context)
  (scale [this] scale)
  (measure [this] measure))

;;; visualization terminal

(defn ^String smeasure-to-string
  "Prints smeasures in a human readable form."
  [sm]
  (let [context (context sm)
        scale   (scale sm)
        mapping (measure sm)
				groups  (group-by #(mapping %) (objects context))
				;;
        ctx-incident? (incidence context)
        sca-incident? (incidence scale)
        ;;
        max-att-ctx (reduce #(max %1 (count (str %2))) 0 (attributes context))
        max-obj-ctx (reduce #(max %1 (count (str %2))) 0 (objects context))
        max-att-sca (reduce #(max %1 (count (str %2))) 0 (attributes scale))
        max-obj-sca (reduce #(max %1 (count (str %2))) 0 (objects scale))
				;;
				seg-line [(ensure-length "" max-obj-ctx "-") "-+"
                  (for [att (attributes context)]
                    (ensure-length "" (inc (count (print-str att))) "-"))
                  "     "
				          (ensure-length "" max-obj-sca "-") "-+"
                  (for [att (attributes scale)]
                    (ensure-length "" (inc (count (print-str att))) "-"))
                  "\n"]]
    (with-str-out
			;; header
      (ensure-length "" max-obj-ctx " ") " |" (for [att (attributes context)]
                                                   [(print-str att) " "])
			"     "
      (ensure-length "" max-obj-ctx " ") " |" (for [att (attributes scale)]
                                                   [(print-str att) " "]) "\n"
			(for [[k group] groups]
				;; first line in group
        [seg-line
         (ensure-length (print-str (first group)) max-obj-ctx)
         " |"
         (for [att (attributes context)]
              [(ensure-length (if (ctx-incident? [(first group) att]) "x" ".")
                              (count (print-str att)))
               " "])
				 " ⟶   "
         (ensure-length (print-str (mapping (first group))) max-obj-sca)
         " |"
         (for [att (attributes scale)]
              [(ensure-length (if (sca-incident? [(mapping(first group)) att]) 
                                  "x" ".")
                              (count (print-str att)))
               " "])
         "\n"
				 ;; remaining lines
				 (for [obj (drop 1 group)]
              [(ensure-length (print-str obj) max-obj-ctx) " |"
               (for [att (attributes context)]
                    [(ensure-length (if (ctx-incident? [obj att]) "x" ".")
                                    (count (print-str att)))
                     " "])
							 "     "
               (ensure-length "" max-obj-ctx " ") " |"
               (for [att (attributes scale)]
                    [(ensure-length "" (count (print-str att)) " ")
                     " "])
               "\n"])]))))

(defn print-smeasure
  "Prints the result of applying smeasure-to-string to the given
   smeasure."
  [sm]
  (print (smeasure-to-string sm)))

(defmethod print-method ScaleMeasure [sm out]
  (.write ^java.io.Writer out
          ^String (smeasure-to-string sm)))

;;;

(defn- pre-image-measure
  "Returns the pre-image map of a scale measures function sigma."
  [sm]
  (let [m (measure sm)]
    (if (map? m)
      (apply (partial merge-with into) {}
             (for [[k v] m] {v #{k}}))
      (let [mapified (into {}
                           (for [obj (objects (context sm))]
                             [obj ((measure sm) obj)]))]
        (apply (partial merge-with into) {}
             (for [[k v] mapified] {v #{k}}))))))

(defn original-extents
  "Returns the pre-image of all extents whichs image is closed in the
  scale."
  [sm]
  (let [scale-extents (extents (scale sm))
        pre-image (pre-image-measure sm)]
    (map #(set (reduce into (map pre-image %)))
            scale-extents)))


(defn valid-scale-measure?
  "Checks if the input is a valid scale measure."
  [sm]
  (let [pre-extents (original-extents sm)]
    (every? #(extent? (context sm) %)
            pre-extents)))

(defn smeasure?
  "Checks if the input is a valid scale measure."
  [sm]
  (and (instance? ScaleMeasure sm)
       (valid-scale-measure? sm)))

(defn make-smeasure
  "Returns a scale-measure object of the input is a valid scale measure."
  [ctx scale m]
  (let [sm (ScaleMeasure. ctx scale m)]
    (assert (valid-scale-measure? sm) "The Input is no valid Scale Measure")
    sm))

(defn make-smeasure-nc
  "Generates a scale measure object without checking the validity."
  [ctx scale m]
  (ScaleMeasure. ctx scale m))

(defn make-id-smeasure
  "Generates a scale-measure with the identity map and the context as scale."
  [ctx]
  (make-smeasure-nc ctx ctx identity))

(defn remove-attributes-sm
  "Removes 'attr attributes from the scale."
  [sm attr]
  (let [s (scale sm)
        new-scale (make-context (objects s)
                              (difference (attributes s) attr)
                              (incidence s))]
    (make-smeasure-nc (context sm) new-scale (measure sm))))

(defmulti rename-scale
  "Renames objects or attributes in the scale. Input the renaming as function on the
  set of objects or as key value pairs."
  (fn [type & args] type))
(alter-meta! #'rename-scale assoc :private true)


(defmethod rename-scale :objects
  ([_ sm rename-fn]
   (make-smeasure-nc (context sm) 
                       (rename-objects (scale sm) rename-fn)
                       (comp rename-fn (measure sm))))
  ([_ sm key val & keyvals]
   (let [rename-map (apply hash-map key val keyvals)
         rename-fn  (fn [o] (or (get rename-map o) o))]
     (rename-scale :objects sm rename-fn))))
 
(defmethod rename-scale :attributes
  ([_ sm rename-fn]
   (make-smeasure-nc (context sm) 
                     (rename-attributes (scale sm) rename-fn)
                     (measure sm)))
  ([_ sm key val & keyvals]
   (let [rename-map (apply hash-map key val keyvals)
         rename-fn  (fn [a] (or (get rename-map a) a))]
     (rename-scale :attributes sm rename-fn))))

(defn- valid-cluster
  "This function is a predicate factory for scale measure
  clustering. Original is the cover structure of the original contexts
  concepts."
  [scale original]
  (let [get-exts (fn [cover] (set (map #(get-in cover [% ::extent]) (keys cover))))
        ext (get-exts original)]
    (fn [clustered-scale pre-image]
      (let [ext-new (get-exts (transform-bv-cover scale clustered-scale original))]

        (subset? (set (map pre-image ext-new)) ext)))))

(defmulti cluster-applier
  "Clusters attributes or objects in the scale context.
  For exaegawegaewrgaewrgsfagfagewgwegwegwegmple the set #{1 2 3} become #{[1 2] 3} in the scale.  The
  new incidence is build such that (g, [1 2]) if (g,m) for all/some m
  in [1 2] in the case of attributes. Possible clusters are
  attribute/object clusters with existential and all quantified
  incidence."
  (fn [type quantifier & args] [type quantifier]))
(alter-meta! #'cluster-applier assoc :private true)


(defmethod cluster-applier [:attributes ::ex]
  [_ _ ctx]
  (let [i (incidence-relation ctx)]
    (fn [attr] (make-context (objects ctx) 
                             (conj (difference (attributes ctx) attr) attr) 
                             (fn [a b] 
                               (if (= attr b) 
                                 (some #((incidence ctx) [a %])
                                       b) 
                                 ((incidence ctx) [a b])))))))

(defmethod cluster-applier [:attributes ::all]
  [_ _ ctx]
  (let [i (incidence-relation ctx)]
    (fn [attr] 
      (make-context (objects ctx) 
                    (conj (difference (set (attributes ctx)) attr)
                          attr) 
                    (fn [a b] 
                      (if (= attr b) 
                        (every? #((incidence ctx) [a %])
                                b) 
                        ((incidence ctx) [a b])))))))

(defmethod cluster-applier [:objects ::all]
  [_ _ ctx]
  (fn [obj] (make-context (conj (difference (objects ctx) obj) obj) 
                          (attributes ctx)
                          (fn [a b] 
                            (if (= a obj) 
                              (every? #((incidence ctx) [% b]) 
                                      a) 
                              ((incidence ctx) [a b]))))))

(defmethod cluster-applier [:objects ::ex]
  [_ _ ctx]
  (fn [obj] (make-context (conj (difference (objects ctx) obj) obj) 
                          (attributes ctx)
                          (fn [a b] 
                            (if (= obj a) 
                              (some #((incidence ctx) [% b]) 
                                    a) 
                              ((incidence ctx) [a b]))))))

(defn- flattened-cluster-applier
  "Returns a cluster method depending on the 'kind and 'quant specifier."
  [ctx kind quant]
  (fn [cl] (-> ((cluster-applier kind quant ctx) cl)
               (rename-objects (fn [o] 
                                 (reduce #(if (coll? %2) (into %1 %2) (conj %1 %2))
                                         #{} o)))
               (rename-attributes (fn [a] 
                                 (reduce #(if (coll? %2) (into %1 %2) (conj %1 %2))
                                         #{} a))))))

; todo check if smast cluster works after ordinary cluster

(defn- repair-cluster 
  "Given a scale context constructs a valid smeasure scale by using as
  few cluster methods as possible. 'quantifier specifies the cluster
  quantifier used by the clustering methods."
  [quant ctx s]
     (loop [broken-scale s]
     (let [br-exts (extents broken-scale)
          ;todo smallest ext and repair obj or largest and repair attr
           pre-images (sort-by count 
                               (map (partial reduce into #{}) ;; pre-image: no renamed used and clusters are flat
                                    br-exts)) 
           rev-pre-images (reverse pre-images)
           not-closed? (fn [e] (let [orig-e (context-object-closure ctx e)]
                                 (if (= orig-e e) nil orig-e)))
           image (fn [o] (some 
                          #(if (contains? % o) % false) (objects broken-scale)))
           broken (some not-closed? pre-images); returns first non extent of lowest cardinality 
           rev-broken (if broken (some not-closed? rev-pre-images) nil)] 
       (if broken
         (let [fixed-intent (if (= quant ::all) 
                              (object-derivation broken-scale (reduce conj #{} (map image rev-broken)))
                              (reduce into #{} 
                                      (map (partial object-derivation broken-scale)
                                           (map (partial conj #{}) 
                                                (map image broken))))) ;; TODO need different derivation for ::ex
               fixed-extent (reduce conj #{} (map image broken))
               [kind fixed-cluster] (first (filter #(-> % second count (> 1)) ;get smallest greater 1
                                                   (sort-by (comp count second) [[:objects fixed-extent]
                                                                              [:attributes fixed-intent]])))
               apply-flattened-cluster (flattened-cluster-applier broken-scale kind quant)]
           (recur (apply-flattened-cluster fixed-cluster)))
         broken-scale))))

(defn- cluster-until-valid
  "This is a helper method, that applies a certain clustering until a
  valid scale measure is outputted. If not unique, returns a seq of
  valid candidates."
  [sm init-cluster apply-cluster valid-cluster? build-candidates comp-scale-image scale-pre-image]
  (loop [i 0]
    (let [candidates (build-candidates i) 
          valids  (filter valid-cluster? 
                                   candidates)]
      (if candidates
        (if (empty?  valids)
          (recur (inc i))
          (if (empty? (first valids))
            (let [new-scale (apply-cluster init-cluster)]
              (make-smeasure-nc (context sm) 
                                (make-context (objects new-scale)
                                              (attributes new-scale)
                                              (incidence-relation new-scale))
                                (comp comp-scale-image (measure sm))))
            (map #(into init-cluster %) valids)))
        nil))))

(derive ::all ::quant)
(derive ::ex ::quant)

(defmulti cluster 
  "Clusters attributes or objects in the scale context.
  For example the set #{1 2 3} become #{[1 2] 3} in the scale.  The
  new incidence is build such that (g, [1 2]) if (g,m) for all/some m
  in [1 2] in the case of attributes. Possible clusters are
  attribute/object clusters with existential and all quantified
  incidence."
  (fn [type quantifier & args] [type quantifier]))


(defmethod cluster [:attributes ::all]
  [_ _ sm attr  & {:keys [flat]}]
  (let [ctx (context sm)
        s (scale sm)]
    (cond 
      flat (let [encoder (fn [a] (if (coll? a) (set a) #{a}))
                 decoder (fn [a] (if (< 1 (count a)) a (first a)))
                 ctx-encoder (fn [ctx] (-> ctx
                                           (rename-attributes encoder)
                                           (rename-objects encoder)))
                 ctx-decoder (fn [ctx] (-> ctx
                                           (rename-attributes  decoder)
                                           (rename-objects  decoder)))
                 apply-flattened-cluster (flattened-cluster-applier (ctx-encoder s) :attributes ::ex )]
             (make-smeasure-nc ctx 
                               (ctx-decoder (apply-flattened-cluster (map encoder attr))) 
                               (measure sm)))
      :else (let [apply-cluster (cluster-applier :attributes ::all s)]
              (make-smeasure-nc (context sm) (apply-cluster attr) (measure sm))))))

(defmethod cluster [:attributes ::ex]
  [_ _ sm attr & {:keys [flat no-check original]}]
  (let [ctx (context sm)
        s (scale sm)]
    (cond 
      (and flat no-check) (let [encoder (fn [a] (if (coll? a) (set a) #{a}))
                                decoder (fn [a] (if (< 1 (count a)) a (first a)))
                                ctx-encoder (fn [ctx] (-> ctx
                                                          (rename-attributes encoder)
                                                          (rename-objects encoder)))
                                ctx-decoder (fn [ctx] (-> ctx
                                                          (rename-attributes  decoder)
                                                          (rename-objects  decoder)))
                                apply-flattened-cluster (flattened-cluster-applier (ctx-encoder s) :attributes ::ex)]
                            (make-smeasure-nc ctx 
                                              (ctx-decoder (apply-flattened-cluster (map encoder attr))) 
                                              (measure sm)))
      flat (let [encoder (fn [a] (if (coll? a) (set a) #{a}))
                 decoder (fn [a] (if (< 1 (count a)) a (first a)))
                 ctx-encoder (fn [ctx] (-> ctx
                                           (rename-attributes encoder)
                                           (rename-objects encoder)))
                 ctx-decoder (fn [ctx] (-> ctx
                                           (rename-attributes  decoder)
                                           (rename-objects  decoder)))
                 
                 apply-flattened-cluster (flattened-cluster-applier (ctx-encoder s) :attributes ::ex )
                 original (if original original (generate-concept-cover (concepts s)))
                 valid-cluster? (valid-cluster s original)
                 clustered (ctx-decoder (apply-flattened-cluster (map encoder attr)))]
             (if (valid-cluster? clustered (measure sm))
               (make-smeasure-nc ctx clustered (measure sm)) 
               (do (println attr "is not a valid clustering") sm)))
      no-check (let [apply-cluster (cluster-applier :attributes ::ex s)]
                 (make-smeasure-nc (context sm) (apply-cluster attr) (measure sm)))
      :else (let [apply-cluster (cluster-applier :attributes ::ex s)
                  original (if original original (generate-concept-cover (concepts s)))
                  valid-cluster? (valid-cluster s original)
                  clustered (apply-cluster attr)
                  ;; comp-scale-image identity
                  ;; scale-pre-image (constantly identity)
                  
                  ;; valid-cluster? (fn [additional-attr] (if ((valid-cluster s original) 
                  ;;                                               (apply-cluster (into attr additional-attr)) 
                  ;;                                               (scale-pre-image (into attr additional-attr)))
                  ;;                                            additional-attr false))
                  ;;     build-candidates (fn [i] (comb/combinations (seq (difference (attributes s) attr)) i))]
                  ;; (cluster-until-valid
                  ;;  sm attr apply-cluster valid-cluster? build-candidates comp-scale-image scale-pre-image)
                  ]
              (if (valid-cluster? clustered (measure sm))
                (make-smeasure-nc ctx clustered (measure sm)) 
                (do (println attr "is not a valid clustering") sm))))))
    
;;repair new map for flattened cluster
(defmethod cluster [:objects ::quant]
  [_ quant sm obj & {:keys [flat no-check original]}]
  (let [ctx (context sm)
        s (scale sm)]
    (cond 
      (and flat no-check) (let [encoder (fn [a] (if (coll? a) (set a) #{a}))
                                decoder (fn [a] (if (< 1 (count a)) a (first a)))
                                ctx-encoder (fn [ctx] (-> ctx
                                                     (rename-attributes encoder)
                                                     (rename-objects encoder)))
                                ctx-decoder (fn [ctx] (-> ctx
                                                     (rename-attributes  decoder)
                                                     (rename-objects  decoder)))
                                apply-flattened-cluster (flattened-cluster-applier (ctx-encoder s) :objects quant)
                                clustered (ctx-decoder (apply-flattened-cluster (set (map encoder obj))))
                                new-map (fn [o] 
                                          (let [entailed (some #(if (contains? % o) % nil) (filter coll? (objects clustered)))]
                                            (if entailed entailed o)))]
                            (make-smeasure-nc ctx clustered new-map))
      flat (let [encoder (fn [a] (if (coll? a) (set a) #{a}))
                 decoder (fn [a] (if (< 1 (count a)) a (first a)))
                 ctx-encoder (fn [ctx] (-> ctx
                                           (rename-attributes encoder)
                                           (rename-objects encoder)))
                 ctx-decoder (fn [ctx] (-> ctx
                                           (rename-attributes  decoder)
                                           (rename-objects  decoder)))
                 apply-flattened-cluster (flattened-cluster-applier (ctx-encoder s) :objects quant )
                 original (if original original (generate-concept-cover (concepts s)))
                 valid-cluster? (valid-cluster s original)
                 clustered (ctx-decoder (apply-flattened-cluster obj))
                 new-map (fn [o] (if (contains? obj ((measure sm) o)) (set (flatten obj)) ((measure sm) o)))]
             (if (valid-cluster? clustered (measure sm))
               (make-smeasure-nc ctx clustered new-map) 
               (do (println obj "is not a valid clustering") sm)))
      no-check (let [apply-cluster (cluster-applier :objects quant s)
                     clustered (apply-cluster obj)
                     new-map (fn [o] (if (contains? obj ((measure sm) o)) obj ((measure sm) o)))]
                 (make-smeasure-nc ctx clustered new-map) )
      :else (let [apply-cluster (cluster-applier :objects quant s)
                  original (if original original (generate-concept-cover (concepts s)))
                  valid-cluster? (valid-cluster s original)
                  clustered (apply-cluster obj)
                  new-map (fn [o] (if (contains? obj ((measure sm) o)) obj ((measure sm) o)))
                  ;; comp-scale-image (fn [g] (if (contains? obj g) obj g))
                  ;; scale-pre-image (fn [o] (fn [oset] (reduce #(if (= o %2) (into %1 %2) (conj %1 %2)) #{} oset)))
                  
                  ;; valid-cluster? (fn [additional-obj] (if ((valid-cluster s original) 
                  ;;                                          (apply-cluster (into obj additional-obj)) 
                  ;;                                          (scale-pre-image (into obj additional-obj)))
                  ;;                                       additional-obj false))
                  ;;     build-candidates (fn [i] (comb/combinations (seq (difference (objects s) obj)) i))]
                  ;; (cluster-until-valid
                  ;;  sm obj apply-cluster valid-cluster? build-candidates comp-scale-image scale-pre-image)
                  ]
              (if (valid-cluster? clustered (measure sm))
                (make-smeasure-nc ctx clustered new-map) 
                (do (println obj "is not a valid clustering") sm))))))

(defn- flattened-cluster
  "This is a wrapper method to flatten the cluster result."
  [kind quant sm thing]
  (let [result (cluster kind quant sm thing)]
    (if result
      (if (coll? result)
        (map (partial reduce #(if (coll? %2) (into %1 %2) (conj %1 %2)) #{})
             result)
        (rename-scale kind result (fn [k] 
                                    (reduce #(if (coll? %2) (into %1 %2) (conj %1 %2))
                                            #{} k))))
      nil)))

;;; tester for valid cluster sets
(defmulti valid-cluster-set
  "This method checks, if a set of objects/attributes can be clustered
  in a context to yield a valid scale measure."
  (fn [type quantifier & args] [type quantifier]))

(defmethod valid-cluster-set [:attributes ::all]
  [_ quant ctx attr]
  true)

(defmethod valid-cluster-set [:attributes ::ex]
  [_ quant ctx attr]
  (let [cluster-deri (apply union (map #(attribute-derivation ctx #{%}) attr))]
    (extent? ctx cluster-deri)))

(defmethod valid-cluster-set [:objects ::all]
  [_ quant ctx obj]
  (let [exts (extents ctx)]
    (not (some #(not (contains? exts (difference % obj))) exts))))

(defmethod valid-cluster-set [:objects ::ex]
  [_ quant ctx obj]
  (let [cluster-deri (apply union (map #(object-derivation ctx #{%}) obj))
        con (concepts ctx)]
    (not (some #(and (subset? (second %) cluster-deri)
                     (not (extent? (union (first %) obj)))) con))))

(defn all-cluster-candidates
"This methods computes all sets of attributes/objects that yield a
  scale measure. The computed sets can be limited to a size or to be
  supersets of an input."
  [kind quant ctx {:keys [size init-set]}]
  (let [get-set (kind {:attributes attributes :objects objects})
        init (if init-set init-set #{})
        limit (- (if size size (count (get-set ctx))) (count init))]
    (filter (partial kind quant ctx)
            (map #(union % init)
                 (apply concat
                        (for [n (range (inc limit))]
                          (comb/combinations (get-set ctx) n)))))))
 
(defn- incompatibility-graph
  "For a formal context computes the imcompatibility graph of its
  incidence relation."
  [ctx]
  (let [nodes (difference (cross-product (objects ctx) (attributes ctx))
                          (incidence-relation ctx))
        incompatible? (fn [[[g m] [h n]]]
                       (and ((incidence ctx) [g n])
                            ((incidence ctx) [h m])))
        edges (r/foldcat (r/filter incompatible? (cross-product nodes nodes)))]
    (apply lg/graph edges)))

(defn- updated-incompatibility-graph 
  [graph scale]
  (let [new-nodes (difference (cross-product (objects scale) (attributes scale))
                              (incidence-relation scale))
        sub (lg/subgraph graph (intersection (lg/nodes graph) new-nodes))
        
        incompatible? (fn [[[g m] [h n]]]
                        (and ((incidence scale) [g n])
                             ((incidence scale) [h m])))
        new-edges (filter incompatible? (cross-product (difference new-nodes (lg/nodes graph)) new-nodes))]
    (apply lg/add-edges sub new-edges)))

(defn- cluster-ind-intersecter 
  "This method computed the intersection between two cluster individuals by computing the splittance."
  [{iobjs1 :objects iattr1 :attributes :as ind1} 
   {iobjs2 :objects iattr2 :attributes :as ind2}]
  {:objects (->> iobjs2 
                 ;; computes splitance 
                 (reduce (fn [tmp-ind cl] (union (set (map #(difference % cl) tmp-ind)) 
                                                 (set (map #(intersection % cl) tmp-ind))))
                         iobjs1)
                 ;; remove empty fragment
                 (#(disj % #{})))
   :attributes (->> iattr2 
                 ;; computes splitance 
                    (reduce (fn [tmp-ind cl] (union (set (map #(difference % cl) tmp-ind)) 
                                                    (set (map #(intersection % cl) tmp-ind))))
                         iattr1)
                 ;; remove empty fragment
                 (#(disj % #{})))})

;; first gen needs to convert each attribute/objects into a set
(defn- ind2clustered-ctx
  "This is a helper method to apply the by the individual specified clustering."
  [args ctx {iobjs :objects iattr :attributes :as ind}]
  (let [apply-cluster-obj (fn [tocluster-ctx cl] ((cluster-applier :objects (:quant args) tocluster-ctx) cl))
        apply-cluster-attr (fn [tocluster-ctx cl] ((cluster-applier :attributes (:quant args) tocluster-ctx) cl))] 
    (reduce apply-cluster-obj (reduce apply-cluster-attr ctx iattr)
            iobjs)))

(defn- apply-small-rand-cluster 
  "This is a helper method to apply the smallest clustering out of a few
  random valid clusterings."
  [args ctx scale-ctx]
  (let [kind (rand-nth [:attributes :objects])
        quant (:quant args)
        candidates (if (and (= :objects kind) (-> scale-ctx objects count (= 1) not)) 
                      (take 10 (partition 2 (shuffle (objects scale-ctx))))
                      (take 10 (partition 2 (shuffle (attributes scale-ctx)))))
        apply-flattened-cluster (flattened-cluster-applier scale-ctx kind quant)
        
        derivation-op (if (= kind :objects) object-derivation attribute-derivation)
        key-fkt (fn [[a b]]  (let [da (derivation-op scale-ctx #{a})
                                                  db (derivation-op scale-ctx #{b})] 
                            (+ (count (difference da db))
                               (count (difference db da)))))
        most-in-common (apply min-key key-fkt candidates)
        smallest (repair-cluster quant ctx (apply-flattened-cluster (set most-in-common)))]
    (if smallest smallest       
        (make-context #{(set (objects ctx))} #{(set (attributes ctx))} 
                      #{[#{(set (objects ctx))} #{(set (attributes ctx))}]})))) ;TODO fix incidence

(defn- fill-individual
  "Inserts as many objects/attributes to the individual as possible.
  An individual is a set of nodes of the contexts incompatibility graph."
  [args ctx graph {iobjs :objects iattr :attributes :as ind}]
  (let [repaired-ind  (->> ind 
                           (ind2clustered-ctx args ctx)
                           (repair-cluster (:quant args) ctx))
        make-ind-map #(hash-map :objects (objects %) :attributes (attributes %))
        new-graph (updated-incompatibility-graph graph repaired-ind)]
    (loop [tmp-ind repaired-ind g new-graph]      ;; apply small clusters until 2D
      (if (la/bipartite? g)
        (make-ind-map tmp-ind)
        (let [new-ind (apply-small-rand-cluster args ctx tmp-ind)
              new-g (updated-incompatibility-graph g new-ind)]
          (recur new-ind new-g))))))

;;genetic algorithm

(defn- make-args-map 
  "Returns default args updated by user input."
  [args]
  (let [default-args {:generation-size 10 :generations 10
                      :survival-ratio 0.3 :mutation-rate 0.3
                      :fresh-chance 0.05 :mode :incidence-clustered
                      :quant ::all
                      :init {:objects #{} :attributes #{}}
                      :init-cluster (fn [ctx] 
                                      {:objects (reduce #(conj %1 #{%2}) #{} (objects ctx)) 
                                       :attributes (reduce #(conj %1 #{%2}) #{} (attributes ctx))})}]
    (assert (or (not (:init args)) 
                 (la/bipartite? (incompatibility-graph (:init args))))
            "Inputted initial Context must have a concept lattice of
              order dimension two.")
    (let [combined-args 
          (-> (reduce #(assoc %1 %2 (%2 args)) default-args (keys args))
              (update :init #(if (context? %) 
                               {:objects (objects %) :attributes (attributes %)}
                               %)))]
      (assoc combined-args :survival-count (int (* (:generation-size combined-args)
                                     (:survival-ratio combined-args)))))))

(defmulti fitness
  "For a bipartite incompatibility graph of a subset of attributes
  and objects of a context, computes if adding an object/attribute
  preserves the bipartite property."
  (fn [args & params] (:mode args)))
(alter-meta! #'fitness assoc :private true)

(defmethod fitness :concepts
  [args ctx {iobjs :objects iattr :attributes :as ind}]
  (-> (ind2clustered-ctx args ctx ind)
         concepts
         count))

(defmethod fitness :nontrivia
  [args ctx {iobjs :objects iattr :attributes :as ind}]
  (let [sub-ctx (ind2clustered-ctx args ctx ind)
        nontrivia-obj (filter #(not (empty? (object-derivation sub-ctx #{%}))) 
                              (objects sub-ctx))
        nontrivia-attr (filter #(not (empty? (attribute-derivation sub-ctx #{%})))
                               (attributes sub-ctx))]
    (* (count nontrivia-obj)
       (count nontrivia-attr))))

(defmethod fitness :incidence
  [args ctx {iobjs :objects iattr :attributes :as ind}]
  (let [sub-ctx (ind2clustered-ctx args ctx ind)]
    (count (incidence-relation sub-ctx))))

(defmethod fitness :default
  [_ ctx {iobjs :objects iattr :attributes :as ind}]
  (* (count iobjs)
     (count iattr)))

(defn- mutation
  "Mutates the current individual by removing an object or attribute
  given a probability."
  [args ctx graph {iobjs :objects iattr :attributes :as ind}]
  ;; (if (> (:fresh-chance args) (rand))
  ;;   (fill-individual-cluster args ctx graph ((:init-cluster args) ctx)))
  (let [mutated-obj  (into #{} (random-sample (:mutation-rate args) iobjs))
        mutated-attr  (into #{} (random-sample (:mutation-rate args) iattr))]
    (-> ind 
        (assoc :objects (reduce (fn [ind-objs tomutate-obj-cl] ;; flattens the to mutate cluster #{1 2 3} -> #{1} #{2} #{3}
                                  (reduce #(conj %1 #{%2}) ind-objs tomutate-obj-cl)) 
                                (difference (:objects ind) mutated-obj) mutated-obj))
        (assoc :attributes (reduce (fn [ind-attr tomutate-attr-cl] 
                                     (reduce #(conj %1 #{%2}) ind-attr tomutate-attr-cl))
                                   (difference (:attributes ind) mutated-attr) mutated-attr)))))

(defn- breeding
  "Given two individuals, i.e. contexts, breeds a next new individual by
  first computing the context of common attributes and
  objects. Secondly as many attributes/objects of ctx are inserted
  without increasing the order dimension of the new individual."
  [args ctx graph ind1 ind2]
  (->> (cluster-ind-intersecter ind1 ind2)
       (mutation args ctx graph)
       (fill-individual args ctx graph)))

(defn- next-generation
  "Given the current generation of contexts, breeds the next generation."
  [args ctx graph generation]
  (let [survivals (->> generation
                          (sort-by (partial fitness args ctx) >) ;; todo parallel fitness apply
                          (take (:survival-count args)))
        pairing (->> survivals
                     shuffle
                     (partition 2))
        rand-pair (fn [] (->> survivals
                              shuffle
                              (take 2)))]
    (into [] 
     (concat survivals
             (->> (concat pairing (repeat (rand-pair)))
                  (take (- (:generation-size args) (:survival-count args)))
                  (into [])
                  (pmap (fn [[ind1 ind2]] (breeding args ctx graph ind1 ind2))))))))

(defn- first-generation
  "Computes a first random generation of contexts with order dimension
  at most two."
  [args ctx init-ctx graph]
  (into [] 
        (pmap (fn [i]
                (fill-individual args ctx graph init-ctx)) 
              (range (:generation-size args)))))

(defn- genetic-2d
  "This Method is a genetic algorithm to determine a scale whichs
  concept lattice is of order dimension 2D. The methods simulated by
  this algorithm are cluster methods only and for comprehensibility we
  use only one cluster variation (::all, :or) at a time. Further note
  that nested clusters of the same type (::all, ::ex) are equivalent to
  their flattened correspondence. This method uses a genetic algorithm
  that determines a large sub-context, i.e. the number of objects
  times attributes, whichs concept lattice is of order dimension at
  most two. After that missing attributes are combined with existing
  objects, attributes such that they preserve the order dimension."
  [ctx & [args]]
  (let [args (make-args-map (if (nil? args) {} args))
        init-ind ((:init-cluster args) ctx)
        graph (incompatibility-graph (ind2clustered-ctx args ctx init-ind))
        decode (fn [ctx] (-> ctx
                             (rename-attributes  (fn [a] (if (< 1 (count a)) a (first a))))
                             (rename-objects  (fn [b] (if (< 1 (count b)) b (first b))))))
        to-sm (fn [scale] (make-smeasure-nc ctx scale 
                                            (fn [g] (if (contains? (objects scale) g) g 
                                                        (some (fn [o] (and (set? o) (contains? o g))) 
                                                              (objects scale))))))]
    (loop [n (:generations args) 
           generation (first-generation args ctx init-ind graph)]
      (println "Generation: " (- (:generations args) n))
      (if (= n 0)
        (->> generation 
             (apply max-key (partial fitness args ctx))
             (ind2clustered-ctx args ctx)
             decode
             to-sm)
        (recur (dec n) (next-generation args ctx graph generation))))))



;;; Declare REPL commands
(defmulti run-repl-command
  "Runs a command for the counterexample REPL."
  (fn [& args] (first args)))
(alter-meta! #'run-repl-command assoc :private true)

(defmulti help-repl-command
  "Returns the help string of the given command."
  (fn [& args] (first args)))
(alter-meta! #'help-repl-command assoc :private true)

(defn- suitable-repl-commands
  "Returns all known repl commands for query, which can be a symbol or
  a string."
  [query]
  (let [str-query (str query)]
    (filter #(.startsWith (str %) str-query)
            (remove #{:default} (keys (methods run-repl-command))))))

(def ^:private abortion-sentinal (Exception. "You should never see this"))

(defn- eval-command
  "Runs the given REPL command query with state, in the case the query uniquely
  determines a command.  If not, an error message is printed and state is
  returned."
  [query state]
  (if (= query 'abort)
    (throw abortion-sentinal)
    (let [suitable-methods (suitable-repl-commands query)]
      (cond
       (second suitable-methods)
       (do
         (println "Ambigious command, suitable methods are")
         (doseq [name suitable-methods]
           (println "  " name))
         state),
       (empty? suitable-methods)
       (do
         (println "Unknown command")
         state)
       :else
       (try
         (run-repl-command (first suitable-methods) state)
         (catch Throwable t
           (print "Encountered Error: ")
           (println t)
           state))))))

(defmacro- define-repl-fn [name doc & body]
  `(do
     (defmethod run-repl-command '~name
       ~'[_ state]
       (let [~'smeasure (:smeasure ~'state)
             
             ~'scale (scale ~'smeasure)]
         ~@body))
     (defmethod help-repl-command '~name
       ~'[_]
       ~doc)))

(define-repl-fn done
  "Ends the Scale Exploration."
  (assoc state :done true))

(define-repl-fn clear
  "Clears the current state and restarts from scratch."
    (assoc state :smeasure (make-id-smeasure (context scale))))

(define-repl-fn truncate
  "Enter an attribute that should be removed from the scale."
  (assoc state :smeasure
         (remove-attributes-sm smeasure 
                               (ask (str "Please enter all to be removed attribute spereated by ';': \n")
                                    #(map read-string (clojure.string/split (str (read-line)) #";"))
                                    #(subset? % (attributes scale))
                                    "The attributes are not all present, please enter an existing attribute: \n"))))

(define-repl-fn rename
  "Renames objects or attributes in the scale."
  (let [rename-kind (ask (str "Please enter if you want to rename attributes (:attributes) or objects (:objects): \n")
                         #(read-string (str (read-line)))
                         #(or (= :objects %) (= :attributes  %))
                         "The input must be :attributes or :objects: \n")
        rename-method (partial rename rename-kind)
        rename-content (ask (str "Please all " (rename-kind {:attributes "attributes" :objects "objects"})
                                 " that should be renamed and their new name with ; seperator (name1;new-name1;name2;...): \n")
                            #(map read-string (clojure.string/split (str (read-line)) #";"))
                            (fn [input] (and (even? (count input))
                                             (every? 
                                              #(contains? ((rename-kind {:attributes attributes :objects objects}) scale) %)
                                              (take-nth 2 input))))
                            (str "Input must be ; seperated an contain only " (rename-kind {:attributes "attributes" :objects "objects"}) "of the scale and their new name:\n"))]
    (if (empty? rename-content) state
        (assoc state :smeasure (apply rename-method smeasure rename-content)))))

(define-repl-fn cluster
  "Apply a clustering to the scales objects or attributes."
  (let [cluster-kind (ask (str "Please enter if you want to cluster attributes (:attributes) or objects (:objects): \n")
                          #(read-string (str (read-line)))
                          #(or (= :objects %) (= :attributes  %))
                          "The input must be :attributes or :objects: \n")
        cluster-incidence (ask (str "Please enter if their  common (::all) or conjoined (::ex) incidence is used as new cluster incidences: \n")
                               #(read-string (str (read-line)))
                               #(or (= ::all %) (= ::ex  %))
                               "The input must be ::all or ::ex: \n")
        cluster-method (partial cluster cluster-kind cluster-incidence)
        cluster-content (ask (str "Please enter all "  (cluster-kind {:attributes "attributes" :objects "objects"}) " to be clustered with ; seperator: \n")
                             #(set (map read-string (clojure.string/split (str (read-line)) #";")))
                             (fn [input] (println input) (every? 
                                          #(contains? ((cluster-kind {:attributes attributes :objects objects}) scale) %)
                                          input))
                             (str "Input must be ; seperated an contain only " (cluster-kind {:attributes "attributes" :objects "objects"}) " of the scale:\n"))]
    (if (empty? cluster-content) state
        (let [clustered (cluster-method smeasure cluster-content)]
          (if (smeasure? clustered)
            (assoc state :smeasure clustered)
            (let [decision (ask (str "Your input does not form a valid Scale Measure. Here are some valid super-sets of your input with lowest possible cardinality: \n"
                                     (clojure.string/join ", " 
                                                          (map (partial apply str) 
                                                               (seq (zipmap 
                                                                     (map #(str "(" % "): ") (range (count clustered)))
                                                                     clustered))))"\n"
                                     "Enter the number of one of these clusters or -1 for none of them: \n")
                                #(read-string (str (read-line)))
                                #(and (<= -1 %) (< % (count clustered)))
                                (str"The input must a number between -1 and " (count clustered) "\n"))]
              (if (= -1 decision)
                state
                (assoc state :smeasure (cluster-method smeasure (nth clustered decision))))))))))

(define-repl-fn show
  "Prints the current scale context, attributes or objects."
  [state]
  (let [toshow (ask (str "Please enter if you want display the scale (:context) its attributes (:attributes) or objects (:objects): \n")
                    #(read-string (str (read-line)))
                    #(or (= :objects %) (= :attributes  %) (= :context %))
                    "The input must be :context, :attributes or :objects: \n")]
    (println "\n" ((toshow {:context identity
                            :attributes (comp (partial clojure.string/join "; ") attributes)
                            :objects (comp (partial clojure.string/join "; ") objects)}) 
                   scale))
    state))

(define-repl-fn help
  "Prints help."
  (let [commands (suitable-repl-commands "")]
    (println "Type «abort» to abort exploration.")
    (println "Any other command can be abbreviated, as long as this is unambigious.")
    (doseq [cmd commands]
      (println (str "  " cmd))
      (println (str "    -> " (help-repl-command cmd))))
    state))

(define-repl-fn suggest
  "Suggests a scale with two dimensional concept lattice."
  (let [suggestion (genetic-2d (context smeasure))]
    (println suggestion)
    (if (= 1 (ask "Do you wish to continue with the suggested scale? 1: yes, 0: no:"
                  #(read-string (str (read-line)))
                  (constantly true)
                  ""))
      (assoc state :smeasure (make-smeasure-nc (context smeasure) suggestion))
      state)))

;;; Scale Exploration 

(defn scale-exploration 
  "Exploration for a scale context to measure a given context.
  The exploration is done with online editing methods.

  - rename:   Rename objects or attributes of the scale
  - cluster:  Clusters objects or attributes in the scale
              The cluster incidence is set as either the common
              or conjoined incidences of all entries
  - truncate: Removes attributes form the scale 
  - clear:    Restarts the exploration

  general functions for exploration interaction
  - show: prints the current scale
  - done: finishes exploration
  - help: prints doc string"
  [ctx]
  (assert (context? ctx) "Input must be a Context.")
  (println (:doc (meta #'scale-exploration)) "\n\n\n")
  (println "Start scale exploration for:\n" ctx)
  (loop [state {:smeasure (make-id-smeasure ctx)}]
    (let [evaluated (eval-command (ask (str "Please enter an operation:\n")
                                       #(read-string (str (read-line)))
                                       (constantly true)
                                       "Input must be a valid command: \n") state)]
      (if (:done evaluated) 
        (:smeasure  evaluated)
        (recur  evaluated)))))
