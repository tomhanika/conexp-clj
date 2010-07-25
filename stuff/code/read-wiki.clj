;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; A program to convert data from the dbpedia project to DL models

(in-ns 'user)

(use '[clojure.contrib.io :only (with-in-reader)]
     '[clojure.walk :only (walk)]
     '[conexp.base :only (set-of)])

;;;

(defn- line-to-pair
  "Converts RDF line to a pair [role [First Second]]."
  [line]
  (let [[A to B] (rest (re-find #"<(.*)> <(.*)> <(.*)>" line))]
    [to [A B]]))

(defn- map-count
  "Counts overall entries in a map."
  [hash-map]
  (reduce + (map #(count (get hash-map %)) (keys hash-map))))

(defn- read-lines-from-file [file interesting-role? interesting-A? interesting-B?]
  (with-in-reader file
    (binding [*in* (clojure.lang.LineNumberingPushbackReader. *in*)]
      (loop [map {},
             line-count 0]
        (if-let [line (read-line)]
          (do
            (when (zero? (mod line-count 10000))
              (println line-count (map-count map)))
            (let [[role [A B]] (line-to-pair line)]
              (recur (if (and (interesting-role? role)
                              (interesting-A? A)
                              (interesting-B? B))
                       (update-in map [role] conj [A B])
                       map)
                     (inc line-count))))
          (do
            (println line-count)
            map))))))

(defn- capitalize
  "Capitalizes word."
  [word]
  (if (empty? word)
    word
    (apply str (Character/toUpperCase (first word)) (rest word))))

(defn- symbolify
  "Transforms every string in coll to a symbol, walking through
  sequential collectiones recursively."
  [coll]
  ((fn transform [thing]
     (cond
      (string? thing) (symbol (capitalize thing)),
      (or (sequential? thing)
          (map? thing)
          (set? thing))
      (walk transform identity thing),
      :else thing))
   coll))

(defn- prepare-for-conexp [hash-map]
  (into {} (for [[k v] (symbolify hash-map)]
             [k (set v)])))

(defn- role-map->concept-map [role-map]
  (assert (= 1 (count role-map)))
  (loop [concept-map {},
         is-as (get role-map (first (keys role-map)))]
    (if (empty? is-as)
      concept-map
      (let [[A B] (first is-as)]
        (recur (update-in concept-map [B] conj A)
               (rest is-as))))))

;;;

(defvar *wikipedia-properties* "/users/lat/borch/wikiprops.nt"
  "File containing the properties as defined by dbpedia")

(defvar *wikipedia-instances* "/users/lat/borch/wikiinstances.nt"
  "File containing the instances as defined by dbpedia")

(defn read-wiki [roles]
  "Reads model from wikipedia entries. roles can be any quoted
  sequence of child, father, mother, influenced, influencedBy, relation,
  relative, spouse, partner, opponent, ..."
  (let [relations (read-lines-from-file *wikipedia-properties*
                                        (set-of (str "http://dbpedia.org/ontology/" role)
                                                [role roles])
                                        (constantly true)
                                        (constantly true)),
        instances (set (flatten (vals relations))),
        concepts (role-map->concept-map
                  (read-lines-from-file *wikipedia-instances*
                                        (constantly true)
                                        #(contains? instances %)
                                        #(not (re-find #"owl#Thing" %))))]
    [(prepare-for-conexp concepts), (prepare-for-conexp relations)]))

;;;

(use 'conexp.base
     'conexp.contrib.profiler
     'conexp.contrib.dl.framework.syntax
     'conexp.contrib.dl.framework.boxes
     'conexp.contrib.dl.framework.semantics
     'conexp.contrib.dl.languages.EL-gfp
     'conexp.contrib.dl.languages.EL-gfp-exploration
     'conexp.contrib.dl.languages.interaction)

(defn read-wiki-model
  "For the given set of roles (as symbols) returns the smallest model
  containing the interpretations of roles in the data-set of dbpedia."
  [roles]
  (let [[concepts, roles] (read-wiki roles)]
    (interpretation->model concepts roles :base-lang EL-gfp)))

(defn collect
  "Returns the smallest connected subrelation of relation containing start."
  [start relation]
  (let [related (set (for [[x y] relation,
                           :when (or (contains? start x)
                                     (contains? start y)),
                           z [x y]]
                       z))]
    (if (= start related)
      start
      (recur related relation))))

(defn smallest-submodel
  "Returns the smallest submodel of model containing the given
  individuals."
  [model individuals]
  (let [relation (reduce union #{} (map #(interpret model %) (role-names (model-language model)))),
        base-set (collect (set individuals) relation),

        name-int (into {} (for [name (concept-names (model-language model))]
                            [name (intersection base-set (interpret model name))])),
        role-int (let [base-square (cross-product base-set base-set)]
                   (into {} (for [role (role-names (model-language model))]
                              [role (intersection base-square (interpret model role))])))],
    (make-model (model-language model)
                base-set
                (merge name-int role-int))))

(defn explore-wiki-model
  "Computes a basis of gcis holding in wiki-model. Returns a reference
  to the gcis collected so far, a reference to the gcis returned so
  far and the thread where the computation is done."
  [wiki-model]
  (let [collected-gcis (ref []),
        resulting-gcis (ref []),

        explore (fn [model]
                  (binding [expert-refuses? (fn [susu]
                                              (dosync (alter collected-gcis
                                                             conj susu))
                                              false)]
                    (explore-model model))),
        thread (Thread. #(let [result (time (explore wiki-model))]
                           (time (doseq [gci result]
                                   (dosync (alter resulting-gcis conj gci))))))]
    (.start thread)
    (add-watch collected-gcis 1
               (fn [k r o n]
                 (println "collected:" (count n) (now))))
    (add-watch resulting-gcis 1
               (fn [k r o n]
                 (println "returned:" (count n) (now))))
    (start-profiling :thread thread)
    [collected-gcis, resulting-gcis, thread]))

;;;

(defn number-of-counterexamples
  "Returns for a model and a gci the number of counterexamples,
  i.e. the cardinality of the extension of the concept (and A
  (not B)), where the gci is of the form A -> B."
  [model A B]
  (count (interpret model (list 'and A (list 'not B)))))

(defn support
  "Returns the support of the given concept, i.e. the cardinality of
  its extension in model."
  [model A]
  (count (interpret model A)))

(defn confidence
  "Returns some kind of confidence for the gci A -> B in model."
  [model A B]
  (- 1 (/ (number-of-counterexamples model A B)
          (count (interpret model A)))))

(defn concept-size
  "Returns the size of an EL-gfp concept description."
  [dl-expression]
  (let [counter (fn counter [term]
                  (cond
                   (sequential? term) (reduce + (map counter term)),
                   (tbox? term) (reduce + 1 (map #(+ 1
                                                     (counter (definition-target %))
                                                     (counter (expression-term (definition-expression %))))
                                                 (tbox-definitions term))),
                   :else 1))]
    (counter (expression-term dl-expression))))

(defn dubiousness
  "Returns some kind of measure for the dubiousness of the gci A -> B
  in model."
  [model A B]
  (/ (concept-size B)
     (concept-size A)
     (+ 1 (support model A))))

;;;

nil
