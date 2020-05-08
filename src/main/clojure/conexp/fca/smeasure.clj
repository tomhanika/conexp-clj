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
            [loom.alg :as la]))

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


(defn cluster-attributes-all 
  "Clusters 'attr attributes in the scale context.
  For example the attributes #{1 2 3} become #{[1 2] 3} in the scale.
  The new incidence is build such that (g, [attr]) if (g,m) for all m in attr." 
  [sm attr]
  (let [ctx (context sm)
        s (scale sm)]
    (make-smeasure-nc (context sm) 
                      (make-context (objects s) 
                                    (conj (difference (set (attributes s)) attr)
                                          attr) 
                                    (fn [a b] 
                                      (if (set? b) 
                                        (every? #((incidence s) [a %]) 
                                              b) 
                                        ((incidence s) [a b])))) 
                      (measure sm))))

(defn- valid-cluster 
  "This function is a predicate factory for valid scale measure clustering."
  [scale original]
  (let [get-exts (fn [cover] (set (map #(get-in cover [% :extent]) (keys cover))))
        ext (get-exts original)]
    (fn [clustered-scale pre-image] 
      (let [ext-new (get-exts (transform-bv-cover scale clustered-scale original))]
        (subset? (set (map pre-image ext-new)) ext)))))

(defn cluster-attributes-ex 
  "Clusters 'attr attributes in the scale context.
  For example the attributes #{1 2 3} become #{[1 2] 3} in the scale.
  The new incidence is build such that (g, [attr]) if (g,m) for some m
  in attr.  If the 'attr cluster does not form a valid scale measure,
  a sequence of valid supersets of lowest cardinality is returned." 
  [sm attr]
  (let [s (scale sm)
        apply-cluster (fn [at] (make-context (objects s) 
                                           (conj (difference (attributes s) at) at) 
                                           (fn [a b] 
                                             (if (set? b) 
                                               (some #((incidence s) [a %]) 
                                                     b) 
                                               ((incidence s) [a b])))))
        original (generate-concept-cover (concepts s))
        comp-scale-image identity
        scale-pre-image identity
        valid-cluster? (valid-cluster s original)]
    (loop [i 0]
      (let [candidates (comb/combinations (seq (difference (attributes s) attr)) i)
            valids (filter #(valid-cluster? (apply-cluster (into attr %)) scale-pre-image) candidates)]
        (if (empty? valids)
          (recur (inc i))
          (if (empty? (first valids))
            (let [new-scale (apply-cluster attr)]
              (make-smeasure-nc (context sm) 
                                (make-context (objects new-scale)
                                              (attributes new-scale)
                                              (incidence-relation new-scale))
                                (comp comp-scale-image (measure sm))))
            (map #(into attr %) valids)))))))

(defn cluster-objects-all 
  "Clusters 'obj objects in the scale context.
  For example the attributes #{1 2 3} become #{[1 2] 3} in the scale.
  The new incidence is build such that ([obj],m) if (g,m) for all g
  in obj.  If the 'obj cluster does not form a valid scale measure,
  a sequence of valid supersets of lowest cardinality is returned." 
  [sm obj]
  (let [s (scale sm)
        apply-cluster (fn [o] (make-context (conj (difference (objects s) o) o) 
                                           (attributes s)
                                           (fn [a b] 
                                             (if (set? a) 
                                               (every? #((incidence s) [% b]) 
                                                     a) 
                                               ((incidence s) [a b])))))
        original (generate-concept-cover (concepts s))
        comp-scale-image (fn [g] (if (contains? obj g) obj g))
        scale-pre-image (fn [o] (fn [oset] (reduce #(if (= o %2) (into %1 %2) (conj %1 %2)) #{} oset)))
        valid-cluster? (valid-cluster s original)]
    (loop [i 0]
      (let [candidates (comb/combinations (seq (difference (objects s) obj)) i)
            valids (filter #(valid-cluster? (apply-cluster (into obj %)) (scale-pre-image (into obj %))) candidates)]
        (if (empty? valids)
          (recur (inc i))
          (if (empty? (first valids))
            (let [new-scale (apply-cluster obj)]
              (make-smeasure-nc (context sm) 
                                (make-context (objects new-scale)
                                              (attributes new-scale)
                                              (incidence-relation new-scale))
                                (comp comp-scale-image (measure sm))))
            (map #(into obj %) valids)))))))


(defn cluster-objects-ex 
  "Clusters 'obj objects in the scale context.
  For example the attributes #{1 2 3} become #{[1 2] 3} in the scale.
  The new incidence is build such that ([obj],m) if (g,m) for some g
  in obj.  If the 'obj cluster does not form a valid scale measure,
  a sequence of valid supersets of lowest cardinality is returned." 
  [sm obj]
  (let [s (scale sm)
        apply-cluster (fn [o] (make-context (conj (difference (objects s) o) o) 
                                           (attributes s)
                                           (fn [a b] 
                                             (if (set? a) 
                                               (some #((incidence s) [% b]) 
                                                     a) 
                                               ((incidence s) [a b])))))
        original (generate-concept-cover (concepts s))
        comp-scale-image (fn [g] (if (contains? obj g) obj g))
        scale-pre-image (fn [o] (fn [oset] (reduce #(if (= o %2) (into %1 %2) (conj %1 %2)) #{} oset)))
        valid-cluster? (valid-cluster s original)]
    (loop [i 0]
      (let [candidates (comb/combinations (seq (difference (objects s) obj)) i)
            valids (filter #(valid-cluster? (apply-cluster (into obj %)) (scale-pre-image (into obj %))) candidates)]
        (if (empty? valids)
          (recur (inc i))
          (if (empty? (first valids))
            (let [new-scale (apply-cluster obj)]
              (make-smeasure-nc (context sm) 
                                (make-context (objects new-scale)
                                              (attributes new-scale)
                                              (incidence-relation new-scale))
                                (comp comp-scale-image (measure sm))))
            (map #(into obj %) valids)))))))

(defn rename-scale-objects
  "Renames objects in the scale. Input the renaming as function on the
  set of objects or as key value pairs."
  ([sm rename-fn]
   (make-smeasure-nc (context sm) 
                       (rename-objects (scale sm) rename-fn)
                       (comp rename-fn (measure sm))))
  ([sm key val & keyvals]
   (let [rename-map (apply hash-map key val keyvals)
         rename-fn  (fn [o] (or (get rename-map o) o))]
     (rename-scale-objects sm rename-fn))))

(defn rename-scale-attributes
  "Renames attribute in the scale. Input the renaming as function on the
  set of attributes or as key value pairs."
  ([sm rename-fn]
   (make-smeasure-nc (context sm) 
                     (rename-attributes (scale sm) rename-fn)
                     (measure sm)))
  ([sm key val & keyvals]
   (let [rename-map (apply hash-map key val keyvals)
         rename-fn  (fn [a] (or (get rename-map a) a))]
     (rename-scale-attributes sm rename-fn))))



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
        rename-method (get {:objects rename-scale-objects :attributes rename-scale-attributes}
                           rename-kind)
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
        cluster-method (get-in {:objects {:ex cluster-objects-ex :all cluster-objects-all} 
                                :attributes {:ex cluster-attributes-ex :all cluster-attributes-all}} 
                               [cluster-kind cluster-incidence])
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


; todo 2D begriffsverband wenn nicht kreuz graph bipartite
; rotes buch Satz 36 page 134
; Genau dann ist die Ferrersdimension von (G, M, I) höchstens zwei, wenn der Un-
; verträglichkeitsgraph bipartit ist.


(defn cross-graph 
  "For a formal context (G,M,I) the graph is defined
  as (I,E), with (g,m)(h,n) in E, if and only if (g,n) not in I
  and (h,m) not in I." 
  [cxt] 
  (let [obj (objects cxt) atr (attributes cxt) 
        vert (filter (incidence cxt)
                     (reduce concat (for [g obj] (for [m atr] [g m]))))
        incidences (zipmap vert (for [v1 vert] 
                                  (filter (fn [v2] 
                                            (and (not ((incidence cxt) [(first v1) (peek v2)])) 
                                                 (not (( incidence cxt) [(first v2) (peek v1)]))))
                                          vert)))] 
    (lg/graph incidences)))


;;genetic algorithm

(defn- two-ferres-covering
  "Given a formal context compute a covering of the incidence relation
  by two ferres relations."
  [ctx]

  )

(defn- fill-individual
  "Inserts as many objects/ attributes to the context as possible".
  [ctx ind]
  (let [tofill-obj (difference (objects ctx) (objects ind))
        tofill-attr (difference (attributes ctx) (attributes ind))
        tofill (concept (zip (repeat :attr) tofill-attr)
                  (zip (repeat :obj) tofill-obj))
        recreate-individual (fn [param] (make-context 
                                         (:obj param) (:attr param) 
                                         (incidence ctx)))]
    (->> tofill
         shuffle
         (reduce #(if (fit??? %1 %2) ;todo
                    (update %1 (first %2) conj (second %2))
                    %1)
                 {:attr (attributes ind)
                  :obj (objects ind)})
         recreate-individual)))

(defn- breeding 
  "Given two individuals, i.e. contexts, breeds a next new individual by
  first computing the context of common attributes and
  objects. Secondly as many attributes/objects of ctx are inserted
  without increasing the order dimension of the new individual."
  [ctx ind1 ind2]
  (->> (make-context 
       (intersection (objects ind1) (objects ind2))
       (intersection (attributes ind1) (attributes ind2))
       (incidence ctx))
      (fill-individual ctx)))

(defn- breed-next-generation
  "Given the current generation of contexts, breeds the next generation."
  [ctx generation]
  (let [survivors (->> generation
                     (sort-by #(* (count (objects %))
                                  (count (attributes %)))))
        [suvivors ]]
    
    )
  

  )

(defn- first-generation
  "Computes a first random generation of contexts with order dimension
  at most two."
  [ctx])

(defn- genetic-2d-subctx
  "Genetic algorithm to determine a maximal sub-context with order
  dimension at most two.  Maximal in terms of number of objects times
  number of attributes."
  [ctx])

;; post processing

(defn- fit-rest
  "This method is a helper method for suggest_2d and given a scale
  context and an original context, fits as many missing
  objects/attributes to the scale by clustering without increasing the order
  dimension of the scale concept lattice."
  [ctx scale]
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
