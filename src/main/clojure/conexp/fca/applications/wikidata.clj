(ns conexp.fca.applications.wikidata
  (:require [conexp.fca.contexts :refer [attributes objects
                                         make-context incidence-relation]]
            [conexp.fca.implications :refer [make-implication premise
                                             conclusion tautology?]]
            [conexp.fca.fast :refer :all]
            [conexp.io.contexts :refer :all]
            [conexp.base :refer [conexp-version]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :refer :all]
            [clj-http.client :as client]
            [clojure.data.json :as json])
  (:import org.apache.http.impl.client.HttpClientBuilder))

(def ^:dynamic *sparql-endpoint*
  "Wikidata SPARQL query endpoint URI"
  "https://query.wikidata.org/bigdata/namespace/wdq/sparql")

(def ^:dynamic *tool-uri*
  "URI to include in tool banner and HTTP user agent header"
  "https://github.com/tomhanika/conexp-clj")

(def ^:dynamic *tool-banner*
  "tool banner to send with SPARQL queries"
  (str "#TOOL:conexp-clj, " *tool-uri*))

(def ^:dynamic *tool-agent*
  "user agent to send with SPARQL queries"
  (str "conexp-clj/"
       (conexp-version)
       " ("
       *tool-uri*
       ")"))

(def ^:dynamic *max-entities-per-query*
  "maximum number of entities requested in a single query (entities
  will be split over multiple queries if above this threshold)" 500)

(def ^:dynamic *query-delay*
  "delay between two queries, in milliseconds"
  500)

(defn- disable-cookies [^HttpClientBuilder builder
                        request]
  "helper to disable cookie management in HTTP requests"
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
    :headers {"User-Agent" *tool-agent*}
    :http-builder-fns [disable-cookies]}))

(defmacro with-sparql-bindings
  "execute body with bindings bound to the results of query"
  [query & body]
  (let [v (gensym "v")]
    `(as-> ~query ~v
       (sparql-query ~v)
       (get ~v :body)
       (json/read-str ~v)
       (let [{{~'bindings "bindings"} "results"} ~v]
         ~@body))))

(defn- entity-add-sparql-prefix
  "prefix an entity-id for use with the Wikidata query service"
  [entity-id]
   (str "wd:" (str entity-id)))

(defn- entity-id-from-uri
  "retrieve an entity id from a Wikidata entity URI"
  [uri]
  (let [slash (str/last-index-of uri "/")]
    (subs uri (+ 1 slash))))

(defn- label-query-for-entities
  "construct a query that retrieves labels for a list of entities, in a given language (default english)"
  [entities & {:keys [lang] :or {lang "en"}}]
  (str "SELECT ?entity ?label WHERE { VALUES ?entity {"
       (str/join " "
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
   (map (fn [ents] (find-labels-for-some-entities ents :lang lang :delay *query-delay*))
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
           (str/last-index-of label "("))
        (str/last-index-of label ")")))

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
    (if (empty? premise)
      "?entity ?someProperty [] .\n [] wikibase:directClaim ?someProperty"
      (str "?entity "
           (str/join
            ";\n    "
            (map to-clause premise))
           " .")
      )))

(defn- pattern-for-conclusion
  "generate a graph pattern not matching the given conclusion"
  [conclusion]
  (str "{ FILTER NOT EXISTS { ?entity wdt:"
       conclusion
       " [] . } }"))

(defn counterexample-query-for-implication
  "generate a query to check for counterexamples to the implication"
  [implication & {:keys [amount limit ask-only]}]
  {:pre [(if ask-only
           (not (or amount limit))
           true)
         (if limit
           (and (integer? limit)
                (< 0 limit))
           true)]}
  (let [impl (unlabel-implication implication)
        body (premise impl)
        head (conclusion impl)]
    (str
     (if ask-only
       "ASK "
       "SELECT ")
     (if amount
       "(COUNT(DISTINCT ?entity) AS ?entities)"
       (when-not ask-only
         "DISTINCT ?entity"))
     " WHERE {\n  "
     (pattern-for-premise body)
     "\n  "
     (str/join
      " UNION\n  "
      (map pattern-for-conclusion
           head))
     "}"
     (when limit
       (str " LIMIT " limit)))))

(defmacro tautology-or-counterexample
  [implication & body]
  `(if (tautology? ~implication)
     nil
     ~@body))

(defn counterexample?
  "check whether there is a counterexample to the given implication"
  [implication]
  (tautology-or-counterexample
   implication
   (-> implication
       (counterexample-query-for-implication :ask-only true)
       (sparql-query)
       (get :body)
       (json/read-str)
       (get "boolean"))))

(defn counterexample
  "find n counterexample to the given implication, or nil if there is none"
  [implication]
  (tautology-or-counterexample
   implication
   (with-sparql-bindings
     (counterexample-query-for-implication implication :limit 1)
     (let [[{entity "entity"}] bindings]
       entity))))

(defn counterexamples
  "Find all counterexamples to the given implication, or nil if there is none. If n
  is provided, find only n counterexamples "
  ([implication]
   (counterexamples implication -1))
  ([implication n]
   (tautology-or-counterexample
    implication
    (with-sparql-bindings
      (if (< n 0)
      (counterexample-query-for-implication implication)
      (counterexample-query-for-implication implication :limit n))
      (map (fn [{entity "entity"}]
             entity)
           bindings)))))

(defn number-of-counterexamples
  "find the number of all counterexamples to the given implication"
  [implication]
  (if (tautology? implication)
    0
    (with-sparql-bindings
      (counterexample-query-for-implication implication :amount true)
      (let [[{{value "value"} "entities"}] bindings]
        (if value
          (read-string value)
          0)))))
