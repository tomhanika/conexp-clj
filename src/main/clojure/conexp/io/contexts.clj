;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.contexts
  (:require [conexp.base                     :refer :all]
            [conexp.fca.contexts             :refer :all]
            [conexp.io.util                  :refer :all]
            [conexp.fca.many-valued-contexts :refer :all]
            [conexp.io.latex                 :refer :all]
            [conexp.io.json                  :refer :all]
            [clojure.string                  :refer (split)]
            [clojure.data.xml                :as xml]
            [clojure.data.json               :as json]
            [json-schema.core                :as json-schema]
            [clojure.data.csv                :as csv]
            [clojure.java.io                 :as io])
  (:import [java.io PushbackReader]))


;;; Input format dispatch

(define-format-dispatch "context")
(set-default-context-format! :simple)

;;; Formats

;; Simple conexp-clj Format

(add-context-input-format :simple
                          (fn [rdr]
                            (= "conexp-clj simple" (read-line))))

(define-context-output-format :simple
  [ctx file]
  (with-out-writer file
    (binding [*print-length* nil]
      (println "conexp-clj simple")
      (prn {:context [(objects ctx)
                      (attributes ctx)
                      (incidence-relation ctx)]}))))

(define-context-input-format :simple
  [file]
  (with-in-reader file
    (let [_        (get-line)
          hash-map (binding [*in* (PushbackReader. *in*)]
                     (read)),
          context  (:context hash-map)]
      (when-not context
        (unsupported-operation "File " file " does not contain a context."))
      (apply make-context context))))


;; Burmeister Format

(add-context-input-format :burmeister
                          (fn [rdr]
                            (= "B" (read-line))))

(define-context-output-format :burmeister
  [ctx file]
  (with-out-writer file
    (println \B)
    (println)
    (println (count (objects ctx)))
    (println (count (attributes ctx)))
    (println)
    (doseq [g (objects ctx)] (println g))
    (doseq [m (attributes ctx)] (println m))
    (let [inz (incidence ctx)]
      (doseq [g (objects ctx)]
        (doseq [m (attributes ctx)]
          (print (if (inz [g m]) "X" ".")))
        (println)))))

(define-context-input-format :burmeister
  [file]
  (with-in-reader file
    (let [_                    (get-lines 2)    ; "B\n\n", we don't support names

          number-of-objects    (Integer/parseInt (.trim (get-line)))
          number-of-attributes (Integer/parseInt (.trim (get-line)))

          _                    (get-line)         ; "\n"

          seq-of-objects       (get-lines number-of-objects)
          seq-of-attributes    (get-lines number-of-attributes)]
      (loop [objs seq-of-objects
             incidence #{}]
        (if (empty? objs)
          (make-context-nc (set seq-of-objects)
                           (set seq-of-attributes)
                           incidence)
          (let [line (get-line)]
            (recur (rest objs)
                   (into incidence
                          (for [idx-m (range number-of-attributes)
                                :when (#{\X,\x} (nth line idx-m))]
                             [(first objs) (nth seq-of-attributes idx-m)])))))))))


;; Anonymous Burmeister, aka Burmeister Format without names

(add-context-input-format :anonymous-burmeister
                          (fn [rdr]
                            (= "A" (read-line))))

(define-context-output-format :anonymous-burmeister
  [ctx file]
  (with-out-writer file
    (println \A)
    (println (count (objects ctx)))
    (println (count (attributes ctx)))
    (let [inz (incidence ctx)]
      (doseq [g (objects ctx)]
        (doseq [m (attributes ctx)]
          (print (if (inz [g m]) "X" ".")))
        (println)))))

(define-context-input-format :anonymous-burmeister
  [file]
  (with-in-reader file
    (let [_                    (get-lines 1),    ; "A\n"
          number-of-objects    (Integer/parseInt (.trim (get-line))),
          number-of-attributes (Integer/parseInt (.trim (get-line))),
          seq-of-objects       (range number-of-objects),
          seq-of-attributes    (range number-of-attributes)]
      (loop [objs seq-of-objects
             incidence #{}]
        (if (empty? objs)
          (make-context-nc (set seq-of-objects)
                           (set seq-of-attributes)
                           incidence)
          (let [line (get-line)]
            (recur (rest objs)
                   (into incidence
                         (for [idx-m (range number-of-attributes)
                               :when (#{\X,\x} (nth line idx-m))]
                            [(first objs) (nth seq-of-attributes idx-m)])))))))))

;; XML helpers

(defn- find-tags [seq-of-hashes tag]
  (for [hash seq-of-hashes :when (= tag (:tag hash))] hash))

(defn- find-tag [seq-of-hashes tag]
  (first (find-tags seq-of-hashes tag)))

(defn- trim [^String str]
  (.trim str))

(defn- hash-from-pairs [pairs]
  (into {} pairs))

;; ConExp

(add-context-input-format :conexp
                          (fn [rdr]
                            (try
                             (= :ConceptualSystem (-> (xml/parse rdr) :tag))
                             (catch Exception _))))

(define-context-input-format :conexp
  [file]
  (with-in-reader file
    (let [xml-tree (xml/parse *in*)
          contexts (:content (first (find-tags (:content xml-tree) :Contexts)))]
      (cond
        (= 0 (count contexts))
        (throw (IllegalArgumentException. (str "No context specified in " file)))
        (< 1 (count contexts))
        (throw (IllegalArgumentException. (str "More than one context specified in " file))))
      (let [context (first contexts)
            atts-map (find-tag (:content context) :Attributes)
            objs-map (find-tag (:content context) :Objects)

            obj-idxs-map (hash-from-pairs
                          (for [obj-map (:content objs-map)]
                            [(apply str (-> obj-map :content (find-tag :Name) :content))
                             (set-of (get-in att [:attrs :AttributeIdentifier])
                                     [att (-> obj-map :content (find-tag :Intent) :content)])]))

            idx-atts-map (hash-from-pairs
                          (for [att-map (:content atts-map)]
                            [(get-in att-map [:attrs :Identifier])
                             (apply str (-> att-map :content (find-tag :Name) :content))]))]
        (assert (distinct? (vals idx-atts-map))
                "Names of attributes must be different.")
        (make-context-nc (set (keys obj-idxs-map))
                         (set (vals idx-atts-map))
                         (set-of [g (idx-atts-map idx) ]
                                 [[g att-idxs] obj-idxs-map
                                  idx att-idxs]))))))

(defn- ctx->xml-vector [ctx id]
  (let [ctx-atts (zipmap (attributes ctx) (iterate inc 0))
        ctx-objs (objects ctx)
        attributes (vector :Attributes
                           (map (fn [[att id]]
                                  [:Attribute {:Identifier id}
                                   [:Name att]])
                                ctx-atts))
        objects (vector :Objects
                        (for [obj ctx-objs]
                          [:Object
                           [:Name obj]
                           (vector :Intent
                                   (for [att (object-derivation ctx #{obj})]
                                     [:HasAttribute {:AttributeIdentifier (ctx-atts att)}]))]))]
    [:Context {:Identifier "0", :Type "Binary"}
     attributes
     objects]))

(define-context-output-format :conexp
  [ctx file]
  (with-out-writer file
    (xml/emit (xml/sexp-as-element [:ConceptualSystem
                                    [:Version {:MajorNumber "1", :MinorNumber "0"}]
                                    [:Contexts (ctx->xml-vector ctx 0)]])
              *out*)))


;; Galicia (.bin.xml)

(add-context-input-format :galicia
                          (fn [rdr]
                            (try
                             (let [xml-tree (xml/parse rdr)]
                               (and (= :Galicia_Document (-> xml-tree :tag))
                                    (= :BinaryContext (-> xml-tree :content first :tag))))
                             (catch Exception _))))

(define-context-output-format :galicia
  [ctx file]
  (let [atts (apply hash-map (interleave (attributes ctx) (iterate inc 0)))
        objs (apply hash-map (interleave (objects ctx) (iterate inc 0)))

        atts-vector (sort #(< (atts %1) (atts %2)) (attributes ctx))
        objs-vector (sort #(< (objs %1) (objs %2)) (objects ctx))]
    (with-out-writer file
      (xml/emit (xml/sexp-as-element [:Galicia_Document
                                      [:BinaryContext {:numberObj (str (count objs-vector)),
                                                       :numberAtt (str (count atts-vector))}
                                       [:Name "conexp-clj generated context"]
                                       (for [obj objs-vector]
                                         [:Object obj])
                                       (for [att atts-vector]
                                         [:Attribute att])
                                       (for [[g m] (incidence-relation ctx)]
                                         [:BinRel {:idxO (str (objs g)),
                                                   :idxA (str (atts m))}])]])
                *out*))))

(define-context-input-format :galicia
  [file]
  (with-in-reader file
    (let [ctx-xml-tree (-> (xml/parse *in*) :content first)

          nr-objs (Integer/parseInt (-> ctx-xml-tree :attrs :numberObj))
          nr-atts (Integer/parseInt (-> ctx-xml-tree :attrs :numberAtt))

          ;; can be done better (one run instead of three)
          objs (map (comp first :content) (filter #(= (:tag %) :Object) (-> ctx-xml-tree :content)))
          atts (map (comp first :content) (filter #(= (:tag %) :Attribute) (-> ctx-xml-tree :content)))
          idxs (map #(vector (Integer/parseInt (:idxO (:attrs %)))
                             (Integer/parseInt (:idxA (:attrs %))))
                    (filter #(= (:tag %) :BinRel) (-> ctx-xml-tree :content)))]
      (make-context-nc objs
                       atts
                       (set-of [(nth objs idxO) (nth atts idxA)]
                               [[idxO idxA] idxs])))))


;; Colibri (.bri, .con)

(add-context-input-format :colibri
                          (fn [rdr]
                            (let [comment #"^\s*#.*$"
                                  blank   #"^\s*$"
                                  row     #"^\s*.+\s*:.*;\s*$"]
                            (forall [line (line-seq rdr)]
                               (or (re-matches comment line)
                                   (re-matches blank line)
                                   (re-matches row line))))))

(define-context-output-format :colibri
  [ctx file]
  (when (some (fn [m]
                (and (string? m) (some #(#{\ ,\:,\;} %) m)))
              (attributes ctx))
    (unsupported-operation
     "Cannot export to :colibri format, object or attribute names contain invalid characters."))
  (when (not (empty? (difference (attributes ctx)
                                 (set-of m [[g m] (incidence-relation ctx)]))))
    (unsupported-operation
     "Cannot export to :colibri format, context contains empty columns."))
  (with-out-writer file
    (doseq [g (objects ctx)]
      (print g)
      (print ":")
      (doseq [m (object-derivation ctx #{g})]
        (print "\t")
        (print m))
      (print ";\n"))))

(define-context-input-format :colibri
  [file]
  (with-in-reader file
    (loop [objs #{},
           inz  #{}]
      (let [line (read-line)]
        (cond
         (not line)
         (make-context-nc objs (set-of m [[g m] inz]) inz),
         (or (re-matches #"^\s*$" line)     ; blank
             (re-matches #"^\s*#.*$" line)) ; comment
         (recur objs inz),
         :else
         (let [[_ g atts] (re-matches #"^\s*(.+)\s*:\s*(.+)?\s*;\s*(?:#.*)?$" line)
               atts (and atts (split atts #"\s+"))]
           (recur (conj objs g) (union inz (set-of [g m] [m atts])))))))))


;; Comma Seperated Values (.csv)

(add-context-input-format :csv
                          (fn [rdr]
                            (try
                              (re-matches #"^[^,]+,[^,]+$" (read-line))
                              (catch Exception _))))

(define-context-input-format :csv
  [file]
  (with-open [reader (io/reader file)]
    (let [csv-list (doall
                    (csv/read-csv reader))
          obj (set (map first csv-list))
          attr (set (map second csv-list))
          inc (set csv-list)]
      (make-context-nc obj attr inc))))

(define-context-output-format :csv
  [ctx file]
  (when (some (fn [x]
                (and (string? x) (some #(= \, %) x)))
              (concat (objects ctx) (attributes ctx)))
    (unsupported-operation "Cannot export to :csv format, object or attribute names contain \",\"."))
  (with-out-writer file
    (doseq [[g m] (incidence-relation ctx)]
      (println (str g "," m)))))


;; Binary CSV (:binary-csv)

(add-context-input-format :binary-csv
                          (fn [rdr]
                            (= "binary CSV" (read-line))))

(define-context-input-format :binary-csv
  [file]
  (with-in-reader file
    (read-line)
    (let [first-line (split (read-line) #",")
          atts       (range (count first-line))]
      (loop [objs      #{0},
             incidence (set-of [0 n] | n atts, :when (= (nth first-line n) "1"))]
        (if-let [line (read-line)]
          (let [line (split line #","),
                i    (count objs)]
            (recur (conj objs i)
                   (into incidence
                         (for [n atts :when (= (nth line n) "1")]
                           [i n]))))
          (make-context objs atts incidence))))))

(define-context-output-format :binary-csv
  [ctx file]
  (when (or (empty? (objects ctx))
            (empty? (attributes ctx)))
    (unsupported-operation "Cannot export empty context in binary-csv format"))
  (when (some (fn [x]
              (and (string? x) (some #(= \, %) x)))
            (concat (objects ctx) (attributes ctx)))
  (unsupported-operation "Cannot export to :binary-csv format, object or attribute names contain \",\"."))
  (let [objs (sort (objects ctx)),
        atts (sort (attributes ctx))]
    (with-out-writer file
      (println "binary CSV")
      (doseq [g objs]
        (loop [atts atts]
          (when-let [m (first atts)]
            (print (if (incident? ctx g m)
                     "1"
                     "0"))
            (when (next atts)
              (print ","))
            (recur (rest atts))))
        (println)))))

(add-context-input-format :named-binary-csv
                          (fn [rdr]
                            (= "NB" (subs (read-line) 0 2))))

(define-context-input-format :named-binary-csv
  [file]
  (with-open [reader (io/reader file)]
    (let [csv-list (doall
                    (csv/read-csv reader))]
      (let [M ((comp rest first) csv-list) rows (rest csv-list)
                     G (reduce (fn [s row] 
                                 (conj s (first row))) 
                               [] 
                               rows)
                     I (reduce (fn [s row] 
                                 (into s 
                                       (map #(Integer/parseInt %) 
                                            (rest row)))) 
                               []
                               rows)]
                 (make-context-from-matrix G M I)))))

(define-context-output-format :named-binary-csv
  [ctx file]
  (when (or (empty? (objects ctx))
            (empty? (attributes ctx)))
    (unsupported-operation "Cannot export empty context in binary-csv format"))
  (when (some (fn [x]
                (and (string? x) (some #(= \, %) x)))
              (concat (objects ctx) (attributes ctx)))
    (unsupported-operation "Cannot export to :binary-csv format, object or attribute names contain \",\"."))
  (let [objs (sort (objects ctx)),
        atts (sort (attributes ctx))]
    (with-out-writer file
      (println (clojure.string/join "," (into ["NB"] atts)))
      (doseq [g objs]
        (println (clojure.string/join "," 
                                      (into [g] 
                                            (map #(if (incident? ctx g %) 1 0) atts))))))))


;; output as tex array

(define-context-output-format :tex
  [ctx file]
  (with-out-writer file
    (println (latex ctx))))


;; fcalgs

(add-context-input-format
 :fcalgs
 (fn [rdr]
   (try (forall [line (line-seq rdr),
                 x    line]
          (or (= x \space)
              (Character/isDigit ^Character x)))
        (catch Exception _ false))))

(define-context-input-format :fcalgs
  [file]
  (with-in-reader file
    (loop [count     0,
           incidence #{}]
      (if-let [line (read-line)]
        (recur (inc count)
               (into incidence (for [x (read-string (str "(" line ")"))]
                                 [count x])))
        (let [objects    (set-of g [[g _] incidence]),
              attributes (set-of m [[_ m] incidence])]
          (make-context objects attributes incidence))))))

(define-context-output-format :fcalgs
  [context file]
  (with-out-writer file
    (when-not (and (= (set-of-range (count (objects context)))
                      (objects context))
                   (= (set-of-range (count (attributes context)))
                      (attributes context)))
      (unsupported-operation "Format :fcalgs can only store contexts with "
                             "integral objects and attributes >= 0, counting upwards."))
    (when (let [max-att (dec (count (attributes context)))]
            (forall [g (objects context)]
              (not ((incidence context) [g max-att]))))
      (unsupported-operation "Cannot store context with last column empty in format :fcalgs"))
    (when (exists [g (objects context)]
            (forall [m (attributes context)]
              (not ((incidence context) [g m]))))
      (unsupported-operation "Cannot store context with empty rows in format :fcalgs"))
    (let [object-count    (count (objects context))
          attribute-count (count (attributes context))]
      (doseq [g (range object-count)]
        (doseq [m (range attribute-count)]
          (when ((incidence context) [g m])
            (print (str m " "))))
        (println)))))

;; GraphML

(define-context-input-format :graphml
  [file]
  (try 
    (let [graphml (xml/parse (reader file))]
      (if (= :graphml (:tag graphml))
          (doall (for [graph (:content graphml) :when (= :graph (:tag graph))]
            (let [default    (:edgedefault (:attrs graph))
                  nodes      (filter #(= :node (:tag %)) (:content graph))
                  edges      (filter #(= :edge (:tag %)) (:content graph))
                  hyperedges (filter #(= :hyperedge (:tag %)) 
                                     (:content graph))] 
              (if (empty? hyperedges)
                ;; nodes X nodes context generated from normal graph
                (let [objects   (set (for [node nodes] (:id (:attrs node))))
                      incidence (map
                                  #(filter identity %)
                                  (apply concat
                                    (for [edge edges] 
                                      (let [attributes (:attrs edge)
                                            src-trg    [(:source attributes) 
                                                        (:target attributes)]
                                            data       (:content edge)
                                            value      (first
                                                         (:content 
                                                           (first data)))]
                                        (when (< 1 (count data))
                                              (illegal-argument 
                                                (str "Multiple data values for" 
                                                     " edges are not" 
                                                     " supported.")))
                                        (when (= clojure.data.xml.Element
                                                 (type value)) 
                                              (illegal-argument
                                                (str "Only single values are"
                                                     " supported as edge"
                                                     " data.")))
                                        (if (or (not (:directed attributes))
                                                (= default "undirected")) 
                                            (map
                                              #(conj % value)
                                              (list src-trg 
                                                    (vec 
                                                      (reverse src-trg))))
                                            (conj src-trg value))))))
                      weights   (filter #(= (count %) 3) incidence)]
                  (if (empty? weights)
                    (make-context objects objects (set incidence))
                    (make-mv-context objects objects (set weights))))
                ;; edges X nodes context generated from hypergraph
                (let [objects    (set (for [node nodes] (:id (:attrs node))))
                      attributes (set (concat (for [hyper hyperedges] 
                                                (:id (:attrs hyper)))
                                              (for [edge edges]
                                                (:id (:attrs edge))))) 
                      ;; read in hyperedges
                      incidence1 (apply concat
                                   (for [hyper hyperedges]
                                     (for [endpoint (:content hyper)]
                                       (filter 
                                         identity
                                         [(:node (:attrs endpoint))
                                          (:id (:attrs hyper))]))))
                      ;; read in normal edges
                      incidence2 (apply concat
                                   (for [edge edges]
                                     (map 
                                       #(filter identity %)
                                       (list
                                         [(:target (:attrs edge))
                                          (:id (:attrs edge))]
                                         [(:source (:attrs edge))
                                          (:id (:attrs edge))]))))
                      incidence (set (concat incidence1 incidence2))]
                  (if (empty? (filter #(< (count %) 2) incidence))
                      (make-context objects attributes incidence)
                      (illegal-argument (str "All edges and hyperedges of an"
                                             " hypergraph need ids."))))))))
          (illegal-argument "XML file does not contain GraphML.")))
    (catch java.io.FileNotFoundException _
           (illegal-argument "Specified file not found."))
    (catch javax.xml.stream.XMLStreamException _
           (illegal-argument "Specified file does not contain valid XML."))))

(define-context-output-format :tex
  [ctx file & options]
  (let [{:keys [objorder attrorder]
         :or {objorder (constantly true), 
              attrorder (constantly true)}} options]
    (with-out-writer file
      (println "\\begin{cxt}")
      (println "\\cxtName{}")
      (let [attr (sort-by  attrorder (attributes ctx))
            obj (sort-by objorder (objects ctx))]
        (doseq [a attr]
          (println (str "\\att{" a "}")))
        (doseq [o obj]
          (println (str "\\obj{" 
                        (clojure.string/join "" 
                                             (for [a attr] 
                                               (if ((incidence ctx) [o a]) "x" ".")))
                        "}{" o "}")))
        (println "\\end{cxt}")))))

;; Json helpers

(defn- object->json
  "Returns an objects with its attributes as a map that can easily be converted into json format.
  
  Example output:
  {object: \"b\",
   attributes: [\"1\", \"2\"]}"
  [ctx object]
  {:object object
   :attributes (filter #(incident? ctx object %) (attributes ctx))})

(defn ctx->json
  "Returns a formal context as a map that can easily be converted into json format.
  
  Example:
  {formal_context: {
     object: \"b\",
     attributes: [\"1\", \"2\"]}}"
  [ctx]
  {:formal_context 
     (mapv (partial object->json ctx) (objects ctx))})

(defn- json-ctx->incidence
  "Returns the incidence of a json context as set of tuples."
  [json-ctx]
  (set-of [o a] [o [(:object json-ctx)], a (:attributes json-ctx)]))

(defn json->ctx
  "Returns a Context object for the given json context."
  [json-ctx]
  (let [objects (map :object json-ctx)
        attributes (distinct (flatten (map :attributes json-ctx)))
        incidence (apply union (mapv json-ctx->incidence json-ctx))]
    (make-context objects attributes incidence)))

;; Json Format (src/main/resources/schemas/context_schema_v1.0.json)

(add-context-input-format :json
                          (fn [rdr]
                            (try (json-object? rdr)
                                 (catch Exception _))))

(define-context-output-format :json
  [ctx file]
  (with-out-writer file
    (print (json/write-str (ctx->json ctx)))))

(define-context-input-format :json
  [file]
  (with-in-reader file 
    (let [file-content (json/read *in* :key-fn keyword)
          json-ctx (:formal_context file-content)
          schema-file "src/main/resources/schemas/context_schema_v1.0.json"]
      (assert (matches-schema? file-content schema-file)
              (str "The input file does not match the schema given at " schema-file "."))
      (json->ctx json-ctx))))

;;; TODO

;; slf
;; csc
;; csx?
;; tuples

;;;

nil
