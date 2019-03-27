(ns conexp.fca.applications.wikidata
  (:require [conexp.fca.contexts :refer [attributes objects make-context incidence-relation]]
            [conexp.fca.implications :refer [make-implication premise conclusion]]
            [conexp.fca.fast :refer :all]
            [conexp.io.contexts :refer :all]
            [clojure.java.io :as io]
            [clojure.set :refer :all]
            [clj-http.client :as client]
            [clojure.data.json :as json])
  (:import org.apache.http.impl.client.HttpClientBuilder))

(def ^:dynamic *sparql-endpoint*
  "https://query.wikidata.org/bigdata/namespace/wdq/sparql")

(def ^:dynamic *tool-banner*
  "#TOOL:conexp-clj, https://github.com/exot/conexp-clj")

(def ^:dynamic *max-entities-per-query*
  500)

(def ^:dynamic *query-delay*
  500)

(defn- disable-cookies [^HttpClientBuilder builder
                        request]
  (.disableCookieManagement builder))

(defn sparql-query
  "retrieve the result of SPARQL query qry from the Wikidata query service *sparql-endpoint*"
  [qry]
  (client/get
   *sparql-endpoint*
   {:accept :json
    :query-params {"query"
                   (str *tool-banner*
                        "\n"
                        qry)}
    :http-builder-fns [disable-cookies]}))

(defmacro with-sparql-bindings
  "execute body with bindings bound to the results of query"
  [query & body]
  (let [v (gensym "v")]
    `(as-> ~query ~v
       (sparql-query ~v)
       (get ~v :body)
       (json/read-str ~v)
       (let [{{bindings "bindings"} "results"} ~v]
         ~@body))))

(defn- entity-add-sparql-prefix
  "prefix an entity-id for use with the Wikidata query service"
  [entity-id]
   (str "wd:" (str entity-id)))

(defn- entity-id-from-uri
  "retrieve an entity id from a Wikidata entity URI"
  [uri]
  (let [slash (clojure.string/last-index-of uri "/")]
    (subs uri (+ 1 slash))))

(defn- label-query-for-entities
  ""
  [entities & {:keys [lang] :or {lang "en"}}]
  (str "SELECT ?entity ?label WHERE { VALUES ?entity {"
       (clojure.string/join " "
                            (map entity-add-sparql-prefix entities))
       "} ?entity rdfs:label ?label FILTER(LANG(?label)=\""
       lang
       "\") }"))

(defn- extract-entity-label-from-json
  "extract entity label result from the JSON data returned by the Wikidata query service"
  [{{entity "value"} "entity"
    {label "value"
     lang "xml:lang"} "label"}]
  (let [entity-id (entity-id-from-uri entity)]
    {entity-id {:entity entity-id
                :label label
                :lang lang}}))

(defn- find-labels-for-some-entities
  "retrieve labels for all entities"
  [entities & {:keys [lang delay] :or {lang "en"}}]
  (if delay
    (Thread/sleep delay))
  (with-sparql-bindings
    (label-query-for-entities entities :lang lang)
    (apply merge
           (map extract-entity-label-from-json bindings))))

(defn find-labels-for-entities
  "retrieve labels for all entities"
  [entities & {:keys [lang] :or {lang "en"}}]
  (apply merge
   (map (fn [ents] (find-labels-for-some-entities ents :delay *query-delay*))
        (partition *max-entities-per-query*
                   *max-entities-per-query*
                   [] entities))))

(defn format-label
  "retrieve a nicely formatted label for an entity id"
  [labels needle]
  (let [{label :label} (get labels needle "[no label found]")]
    (str label
         " ("
         needle
         ")")))

(defn find-labels-for-context
  "find labels for all objects and attributes in the context ctx"
  [ctx]
  (let [objs (objects ctx)
        atts (attributes ctx)
        labels (find-labels-for-entities (union objs atts))
        lookup (fn [needle] (format-label labels needle))
        translate (fn [& [names]] (map lookup names))]
    (make-context (map lookup objs)
                  (map lookup atts)
                  (map translate (incidence-relation ctx)))))

(defn- id-from-label
  "find the entity id from the label"
  [label]
  (subs label
        (+ 1
           (clojure.string/last-index-of label "("))
        (clojure.string/last-index-of label ")")))

(defn- unlabel-implication
  "turn an implication on labels into an implication on ids"
  [implication]
  (make-implication
   (map id-from-label (premise implication))
   (map id-from-label (conclusion implication))))


(defn- pattern-for-premise
  "generate a graph pattern matching a premise"
  [premise]
  (let [to-clause (fn [property]
                    (str "wdt:"
                         property
                         " []"))]
    (str "?entity "
         (clojure.string/join
          ";\n    "
          (map to-clause premise))
         " .")))

(defn- pattern-for-conclusion
  "generate a graph pattern not matching the given conclusion"
  [conclusion]
  (str "{ FILTER NOT EXISTS { ?entity wdt:"
       conclusion
       " [] . } }"))

(defn counterexample-query-for-implication
  "generate a query to check for counterexamples to the implication"
  [implication & {:keys [amount limit]}]
  {:pre [(if limit
           (and (integer? limit)
                (< 0 limit))
           true)]}
  (let [impl (unlabel-implication implication)
        body (premise impl)
        head (conclusion impl)]
    (str
     "SELECT "
     (if amount
       "(COUNT(?entity) AS ?entities)"
       "?entity")
     " WHERE {\n  "
     (pattern-for-premise body)
     "\n  "
     (clojure.string/join
      " UNION\n  "
      (map pattern-for-conclusion
           head))
     "}"
     (when limit
       (str " LIMIT " limit)))))

(defn counterexample?
  "check whether there is a counterexample to the given implication"
  [implication]
  (with-sparql-bindings
    (counterexample-query-for-implication implication :limit 1)
    (< 0 (count bindings))))

(defn counterexample
  "find a counterexample to the given implication, or nil if there is none"
  [implication]
  (with-sparql-bindings
    (counterexample-query-for-implication implication :limit 1)
    (let [[{entity "entity"}] bindings]
      entity)))

(defn counterexamples
  "find all counterexample to the given implication, or nil if there is none"
  [implication]
  (with-sparql-bindings
    (counterexample-query-for-implication implication)
    (map (fn [{entity "entity"}]
           entity)
         bindings)))

(defn number-of-counterexamples
  "find the number of all counterexamples to the given implication"
  [implication]
  (with-sparql-bindings
    (counterexample-query-for-implication implication :amount true)
    (let [[{{value "value"} "entities"}] bindings]
      (if value
        (read-string value)
        0))))
