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
            [loom.graph :as lg]
            [loom.alg :as la])
  (:import [org.dimdraw Bipartite]))

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
  "This function is a predicate factory for valid scale measure clustering."
  [scale original]
  (let [get-exts (fn [cover] (set (map #(get-in cover [% :extent]) (keys cover))))
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

(defmethod cluster-applier [:attributes :ex]
  [_ _ ctx]
  (let [i (incidence-relation ctx)]
    (fn [attr] (make-context (objects ctx) 
                             (conj (difference (attributes ctx) attr) attr) 
                             (fn [a b] 
                               (if (= attr b) 
                                 (some #((incidence ctx) [a %])
                                       b) 
                                 ((incidence ctx) [a b])))))))

(defmethod cluster-applier [:attributes :all]
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

(defmethod cluster-applier [:objects :all]
  [_ _ ctx]
  (fn [obj] (make-context (conj (difference (objects ctx) obj) obj) 
                          (attributes ctx)
                          (fn [a b] 
                            (if (= a obj) 
                              (every? #((incidence ctx) [% b]) 
                                      a) 
                              ((incidence ctx) [a b]))))))

(defmethod cluster-applier [:objects :ex]
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

(defn- cluster-until-valid
  "This is a helper method, that applies a certain clustering until a
  valid scale measure is outputted. If not unique, returns a seq of
  valid candidates."
  [sm init-cluster apply-cluster valid-cluster? build-candidates comp-scale-image scale-pre-image]
  (loop [i 0]
    (let [candidates (build-candidates i) 
          valids (filter valid-cluster? 
                           candidates)]
      (if candidates
        (if (empty? valids)
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

(defmulti cluster 
  "Clusters attributes or objects in the scale context.
  For example the set #{1 2 3} become #{[1 2] 3} in the scale.  The
  new incidence is build such that (g, [1 2]) if (g,m) for all/some m
  in [1 2] in the case of attributes. Possible clusters are
  attribute/object clusters with existential and all quantified
  incidence."
  (fn [type quantifier & args] [type quantifier]))
(alter-meta! #'cluster assoc :private true)

(defmethod cluster [:attributes :all]
  [_ _ sm attr]
  (let [ctx (context sm)
        s (scale sm)
        apply-cluster (cluster-applier :attributes :all s)]
    (make-smeasure-nc (context sm) (apply-cluster attr) (measure sm))))

(defmethod cluster [:attributes :ex]
  [_ _ sm attr]
  (let [s (scale sm)
        apply-cluster (cluster-applier :attributes :ex s)
        original (generate-concept-cover (concepts s))
        comp-scale-image identity
        scale-pre-image identity
        valid-cluster? (valid-cluster s original)

        valid-cluster? (fn [additional-attr] ((valid-cluster s original) 
                                             (apply-cluster (into attr additional-attr)) 
                                             (scale-pre-image (into attr additional-attr))))
        build-candidates (fn [i] (comb/combinations (seq (difference (attributes s) attr)) i))]
    (cluster-until-valid
     sm attr apply-cluster valid-cluster? build-candidates comp-scale-image scale-pre-image)))

(defmethod cluster [:objects :all]
  [_ _ sm obj]
  (let [s (scale sm)
        apply-cluster (cluster-applier :objects :all s)
        original (generate-concept-cover (concepts s))
        comp-scale-image (fn [g] (if (contains? obj g) obj g))
        scale-pre-image (fn [o] (fn [oset] (reduce #(if (= o %2) (into %1 %2) (conj %1 %2)) #{} oset)))
        valid-cluster? (fn [additional-obj] ((valid-cluster s original) 
                                             (apply-cluster (into obj additional-obj)) 
                                             (scale-pre-image (into obj additional-obj))))
        build-candidates (fn [i] (comb/combinations (seq (difference (objects s) obj)) i))]
    (cluster-until-valid
     sm obj apply-cluster valid-cluster? build-candidates comp-scale-image scale-pre-image)))

(defmethod cluster [:objects :ex]
  [_ _ sm obj]
  (let [s (scale sm)
        apply-cluster (cluster-applier :objects :ex s)
        original (generate-concept-cover (concepts s))
        comp-scale-image (fn [g] (if (contains? obj g) obj g))
        scale-pre-image (fn [o] (fn [oset] (reduce #(if (= o %2) (into %1 %2) (conj %1 %2)) #{} oset)))
        valid-cluster? (fn [additional-obj] ((valid-cluster s original) 
                                             (apply-cluster (into obj additional-obj)) 
                                             (scale-pre-image (into obj additional-obj))))
        build-candidates (fn [i] (comb/combinations (seq (difference (objects s) obj)) i))]
    (cluster-until-valid
     sm obj apply-cluster valid-cluster? build-candidates comp-scale-image scale-pre-image)))

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

(defmulti repair-cluster 
  "Given a scale context constructs a valid smeasure scale by using as
  few cluster methods as possible. 'quantifier specifies the cluster
  quantifier used by the clustering methods."
  (fn [quantifier & args] quantifier))
(alter-meta! #'repair-cluster assoc :private true)

(defmethod repair-cluster :all 
  [_ ctx s]
  (loop [broken-scale s]
    (let [br-exts (extents broken-scale)
          pre-images (sort-by count 
                              (map (partial reduce into #{}) ;; pre-image: no renamed used and clusters are flat
                                   br-exts)) 
          not-closed? (fn [e] (let [orig-e (context-object-closure ctx e)]
                                (if (= orig-e e) nil orig-e)))
          image (fn [o] (some 
                         #(if (contains? % o) % false) (objects broken-scale)))
          broken (some not-closed? pre-images)] ; returns first non extent of lowest cardinality 
      (if broken
        (let [apply-flattened-cluster (flattened-cluster-applier broken-scale :objects :all)
              fixed-extent (reduce conj #{} (map image broken))]
          (recur (apply-flattened-cluster fixed-extent)))
        broken-scale))))

(defmethod repair-cluster :ex
  [_ ctx s]
  (loop [broken-scale s]
    (let [br-exts (extents broken-scale)
          pre-images (sort-by count 
                              (map (partial reduce into #{}) ;; pre-image: no renamed used and clusters are flat
                                   br-exts)) 
          not-closed? (fn [e] (let [orig-e (context-object-closure ctx e)]
                                (if (= orig-e e) nil orig-e)))
          image (fn [o] (some 
                         #(if (contains? % o) % false) (objects broken-scale)))
          broken (some not-closed? pre-images)] ; returns first non extent of lowest cardinality 
      (if broken
        (let [apply-flattened-cluster (flattened-cluster-applier broken-scale :objects :ex)
              fixed-extent (reduce conj #{} (map image broken))]
          (recur (apply-flattened-cluster fixed-extent)))
        broken-scale))))
;; (defmethod repair-cluster :ex
;;   [_ ctx broken-scale]
;;   TODO)

(defn- incompatibility-graph
  "For a formal context computes the imcompatibility graph of its
  incidence relation."
  [ctx]
  (let [nodes (difference (cross-product (objects ctx) (attributes ctx))
                          (incidence-relation ctx))
        incompatible? (fn [[[g m] [h n]]]
                       (and ((incidence ctx) [g n])
                            ((incidence ctx) [h m])))
        edges (filter incompatible? (cross-product nodes nodes))]
    (apply lg/graph edges)))


(defmulti add-if-bipartite 
  "For a bipartite incompatibility graph of a subset of attributes
  and objects of a context, computes if adding an object/attribute
  preserves the bipartite property."
  (fn [& args] (first args)))
(alter-meta! #'add-if-bipartite assoc :private true)

(defmethod add-if-bipartite :object
  [_ graph {iobjs :objects iattr :attributes :as ind} add-objs]
  (let [old-nodes (into #{}
                        (filter #(and (contains? iattr (second %)) 
                                      (contains? iobjs (first %))) (lg/nodes graph)))
        {new-objs :objects :as new-ind} (update ind :objects conj add-objs)
        new-nodes (into #{}
                       (filter #(and (contains? new-objs (first %))
                                     (contains? iattr (second %))) (lg/nodes graph)))
        ;; A node can be added if it is not in an odd cycle with itself
        not-addable? (fn [n] 
                       (. Bipartite isInOddCycle (:adj graph) (disj new-nodes n) n))]
    (if (->> (difference new-nodes old-nodes)
             (some not-addable?))
      ind new-ind)))

(defmethod add-if-bipartite :attribute
  [_ graph {iobjs :objects iattr :attributes :as ind} add-attr]
  (let [old-nodes (into #{} 
                        (filter #(and (contains? iattr (second %)) 
                                      (contains? iobjs (first %))) (lg/nodes graph)))
        {new-attr :attributes :as new-ind} (update ind :attributes conj add-attr)
        new-nodes (into #{} 
                        (filter #(and (contains? new-attr (second %))
                                      (contains? iobjs (first %))) (lg/nodes graph)))
        ;; A node can be added if it is not in an odd cycle with itself
        not-addable? (fn [n] 
                       (. Bipartite isInOddCycle (:adj graph) (disj new-nodes n) n))]
    (if (->> (difference new-nodes old-nodes)
             (some not-addable?))
      ind new-ind)))

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
  (let [apply-cluster-obj (fn [tocluster-ctx cl] ((cluster-applier :objects (:obj-cl-quantifier args) tocluster-ctx) cl))
        apply-cluster-attr (fn [tocluster-ctx cl] ((cluster-applier :attributes (:attr-cl-quantifier args) tocluster-ctx) cl))] 
    (reduce apply-cluster-obj (reduce apply-cluster-attr ctx iattr)
            iobjs)))

(defn- apply-small-rand-cluster 
  "This is a helper method to apply the smallest clustering out of a few
  random valid clusterings."
  [args ctx scale-ctx]
  (let [kind (rand-nth [:attributes :objects])
        quant (if (= :attributes kind)
                (:attr-cl-quantifier args)
                (:obj-cl-quantifier args))
        candidates (if (= :objects kind) 
                     (map set (take 5 (partition 2 (shuffle (objects scale-ctx)))))
                     (map set (take 5 (partition 2 (shuffle (attributes scale-ctx))))))
        ;; how large the clusters get
        sm (make-smeasure-nc ctx scale-ctx 
                             (fn [o] 
                               (some 
                                #(if (contains? % o) % false)
                                (objects scale-ctx))))
        key-fkt (fn [thing] 
                  (let [result (flattened-cluster kind 
                                                  quant
                                                  sm thing)]
                    (if result
                      (if (coll? result) (count (first result)) 
                          (apply + (map count thing)))
                      Integer/MAX_VALUE)))
        smallest (apply min-key key-fkt candidates)
        flat-clustered (cluster kind quant sm smallest)]
    (if flat-clustered
      (if (coll? flat-clustered) ;;clustering was not valid, but returned a list of valid superclusters
        (scale (flattened-cluster kind quant sm (first flat-clustered)))
        (scale (flattened-cluster kind quant sm smallest)))
      (make-context #{(set (objects ctx))} #{(set (attributes ctx))} 
                    #{[#{(set (objects ctx))} #{(set (attributes ctx))}]})))) ;TODO fix incidence

(defn- fill-individual-cluster
  "Inserts as many objects/attributes to the individual as possible.
  An individual is a set of nodes of the contexts incompatibility graph."
  [args ctx {iobjs :objects iattr :attributes :as ind}]
  (let [repaired-ind (->> ind 
                          (ind2clustered-ctx args ctx)
                          (repair-cluster (:obj-cl-quantifier args) ctx))
        make-ind-map #(hash-map :objects (objects %) :attributes (attributes %))
        context2d? (comp la/bipartite? incompatibility-graph)]
    (loop [tmp-ind repaired-ind]      ;; apply small clusters until 2D
      (if (context2d? tmp-ind)
        (make-ind-map tmp-ind)
        (recur (apply-small-rand-cluster args ctx tmp-ind))))))

(defn- fill-individual
  "Inserts as many objects/attributes to the individual as possible.
  An individual is a set of nodes of the contexts incompatibility graph."
  [ctx graph {iobjs :objects iattr :attributes :as ind}]
  (let [;; missing objects and attributes in the individual
        tofill-obj (difference (objects ctx) iobjs)
        tofill-attr (difference (attributes ctx) iattr)
        tofill (into [] (concat (zip (repeat :attribute) tofill-attr)
                                (zip (repeat :object) tofill-obj)))
        ;; add if addable 
        add-if-addable (fn [tmp-ind [kind toadd]] 
                         (add-if-bipartite kind graph tmp-ind toadd))]
    (->> tofill
         shuffle
         (reduce add-if-addable ind))))




;;genetic algorithm

(defn- make-args-map 
  "Returns default args updated by user input."
  [args]
  (let [default-args {:generation-size 30 :generations 10
                      :survival-ratio 0.1 :mutation-rate 0.3
                      :fresh-chance 0.05 :mode :incidence-clustered
                      :obj-cl-quantifier :all
                      :attr-cl-quantifier :all
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

(defmethod fitness :clustered
  [_ & args]
  (apply fitness :default args))

(defmethod fitness :concepts
  [_ ctx {iobjs :objects iattr :attributes :as ind}]
  (-> (make-context iobjs iattr (incidence ctx))
         concepts
         count))

(defmethod fitness :concepts-cluster
  [args ctx {iobjs :objects iattr :attributes :as ind}]
  (-> (ind2clustered-ctx args ctx ind)
         concepts
         count))

(defmethod fitness :ctx-nontrivia
  [_ ctx {iobjs :objects iattr :attributes :as ind}]
  (let [sub-ctx (make-context iobjs iattr (incidence ctx))
        nontrivia-obj (filter #(not (empty? (object-derivation sub-ctx #{%}))) 
                              (objects sub-ctx))
        nontrivia-attr (filter #(not (empty? (attribute-derivation sub-ctx #{%})))
                               (attributes sub-ctx))]
    (* (count nontrivia-obj)
       (count nontrivia-attr))))

(defmethod fitness :nontrivia-clustered
  [args ctx {iobjs :objects iattr :attributes :as ind}]
  (let [sub-ctx (ind2clustered-ctx args ctx ind)
        nontrivia-obj (filter #(not (empty? (object-derivation sub-ctx #{%}))) 
                              (objects sub-ctx))
        nontrivia-attr (filter #(not (empty? (attribute-derivation sub-ctx #{%})))
                               (attributes sub-ctx))]
    (* (count nontrivia-obj)
       (count nontrivia-attr))))


(defmethod fitness :incidence
  [_ ctx {iobjs :objects iattr :attributes :as ind}]
  (let [sub-ctx (make-context iobjs iattr (incidence ctx))]
    (count (incidence-relation sub-ctx))))

(defmethod fitness :incidence-clustered
  [args ctx {iobjs :objects iattr :attributes :as ind}]
  (let [sub-ctx (ind2clustered-ctx args ctx ind)]
    (count (incidence-relation sub-ctx))))

(defmethod fitness :default
  [_ ctx {iobjs :objects iattr :attributes :as ind}]
  (* (count iobjs)
     (count iattr)))

(defn- mutation-cluster
  "Mutates the current individual by removing an object or attribute
  given a probability."
  [args ctx {iobjs :objects iattr :attributes :as ind}]
  (if (> (:fresh-chance args) (rand))
    (fill-individual-cluster args ctx ((:init-cluster args) ctx))
    (let [mutated-obj  (into #{} (random-sample (:mutation-rate args) iobjs))
          mutated-attr  (into #{} (random-sample (:mutation-rate args) iattr))]
      (-> ind 
          (assoc :objects (reduce (fn [ind-objs tomutate-obj-cl] ;; flattens the to mutate cluster #{1 2 3} -> #{1} #{2} #{3}
                                    (reduce #(conj %1 #{%2}) ind-objs tomutate-obj-cl)) 
                                  (difference (:objects ind) mutated-obj) mutated-obj))
          (assoc :attributes (reduce (fn [ind-attr tomutate-attr-cl] 
                                       (reduce #(conj %1 #{%2}) ind-attr tomutate-attr-cl))
                                     (difference (:attributes ind) mutated-attr) mutated-attr))))))

(defn- mutation
  "Mutates the current individual by removing an object or attribute
  given a probability."
  [args ctx graph {iobjs :objects iattr :attributes :as ind}]
  (if (> (:fresh-chance args) (rand))
    (fill-individual ctx graph (:init args))
    (let [mutated-obj  (into #{} (random-sample (- 1 (:mutation-rate args)) iobjs))
          mutated-attr  (into #{} (random-sample (- 1 (:mutation-rate args)) iattr))]
      (-> ind 
          (assoc :objects mutated-obj)
          (assoc :attributes mutated-attr)))))

(defn- breeding-cluster
  "Given two individuals, i.e. contexts, breeds a next new individual by
  first computing the context of common attributes and
  objects. Secondly as many attributes/objects of ctx are inserted
  without increasing the order dimension of the new individual."
  [args ctx ind1 ind2]
  (->> (cluster-ind-intersecter  ind1 ind2)
       (mutation-cluster args ctx)
       (fill-individual-cluster args ctx)))

(defn- breeding 
  "Given two individuals, i.e. contexts, breeds a next new individual by
  first computing the context of common attributes and
  objects. Secondly as many attributes/objects of ctx are inserted
  without increasing the order dimension of the new individual."
  [args ctx graph 
   {iobjs :objects iattr :attributes} {iobjs2 :objects iattr2 :attributes}]
  (->> {:objects (intersection iobjs iobjs2) :attributes (intersection iattr iattr2)}
       (mutation args ctx graph)
       (fill-individual ctx graph)))

(defn- next-generation-cluster
  "Given the current generation of contexts, breeds the next generation."
  [args ctx generation]
  (let [fitness-vals (->> generation
                          (sort-by (partial fitness args ctx))
                          (take (:survival-count args)))
        survivals (take (:survival-count args) fitness-vals)
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
                  (pmap (fn [[ind1 ind2]] (breeding-cluster args ctx ind1 ind2))))))))

(defn- next-generation
  "Given the current generation of contexts, breeds the next generation."
  [args ctx graph generation]
  (let [fitness-vals (->> generation
                          (sort-by (partial fitness args ctx))
                          (take (:survival-count args)))
        survivals (take (:survival-count args) fitness-vals)
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
                  (map #(apply breeding args ctx graph %)))))))

(defn- first-generation-cluster
  "Computes a first random generation of contexts with order dimension
  at most two."
  [args ctx]
  (into [] 
        (pmap (fn [i]
                (fill-individual-cluster args ctx
                                         ((:init-cluster args) ctx))) 
              (range (:generation-size args)))))

(defn- first-generation
  "Computes a first random generation of contexts with order dimension
  at most two."
  [args ctx graph]
  (into [] 
        (for [i (range (:generation-size args))]
          (fill-individual ctx graph 
                           (:init args)))))


(defn genetic-2d-cluster
  "Genetic algorithm to determine a maximal sub-context with order
  dimension at most two.  Maximal in terms of number of objects times
  number of attributes."
  [ctx & [args]]
  (let [args (make-args-map (if (nil? args) {} args))]
    (loop [n (:generations args) 
           generation (first-generation-cluster args ctx)]
      (println "Generation: " (- (:generations args) n))
      (if (= n 0)
        (->> generation 
             (apply max-key (partial fitness args ctx))
             (ind2clustered-ctx args ctx))
        (recur (dec n) (next-generation-cluster args ctx generation))))))

(defn genetic-2d-subctx
  "Genetic algorithm to determine a maximal sub-context with order
  dimension at most two.  Maximal in terms of number of objects times
  number of attributes."
  [ctx & [args]]
  (let [args (make-args-map args)
        graph (incompatibility-graph ctx)
        ind2subctx (fn [{iobjs :objects iattr :attributes}]
                        (make-context iobjs iattr (incidence ctx)))]
    (loop [n (:generations args) 
           generation (first-generation args ctx graph)]
      (if (= n 0)
        (->> generation 
             (apply max-key (partial fitness args ctx))
             ind2subctx)
        (recur (dec n) (next-generation args ctx graph generation))))))

;; post processing

(defn- fit-rest
  "This method is a helper method for suggest_2d and given a scale
  context and an original context, fits as many missing
  objects/attributes to the scale by clustering without increasing the order
  dimension of the scale concept lattice."
  [ctx scale]
  scale
  ; compute ferres covering
  ; loop over attribtues/objects
  )

;; Suggest Scale

(defn suggest_2d 
  "This Method is a genetic algorithm to determine a scale whichs
  concept lattice is of order dimension 2D. The methods simulated by
  this algorithm are cluster methods only and for comprehensibility we
  use only one cluster variation (:all, :or) at a time. Further note
  that nested clusters of the same type (:all, :ex) are equivalent to
  their flattened correspondence. This method uses a genetic algorithm
  that determines a large sub-context, i.e. the number of objects
  times attributes, whichs concept lattice is of order dimension at
  most two. After that missing attributes are combined with existing
  objects, attributes such that they preserve the order dimension."
  [ctx]
  ; first determine subcontext by genetic algorithm
  ; second fill in all missing attributes objects
  (-> ctx
      genetic-2d-subctx
      (fit-rest ctx)))


;; (defn- two-ferres-covering
;;   "Given a formal context compute a covering of the incidence relation
;;   by two ferres relations."
;;   [ctx]

;;   )

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
        cluster-incidence (ask (str "Please enter if their  common (:all) or conjoined (:ex) incidence is used as new cluster incidences: \n")
                               #(read-string (str (read-line)))
                               #(or (= :all %) (= :ex  %))
                               "The input must be :all or :ex: \n")
        cluster-method (partial cluster cluster-kind cluster-incidence)
        cluster-content (ask (str "Please enter all "  (cluster-kind {:attributes "attributes" :objects "objects"}) " to be clustered with ; seperator: \n")
                             #(set (map read-string (clojure.string/split (str (read-line)) #";")))
                             (fn [input] (every? 
                                          #(contains? ((cluster-kind {:attributes attributes :objects objects}) scale) %)
                                          input))
                             (str "Input must be ; seperated an contain only " (cluster-kind {:attributes "attributes" :objects "objects"}) "of the scale:\n"))]
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
  (let [suggestion (suggest_2d (context smeasure))]
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


