;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.contexts
  (:require [conexp.base         :refer :all]
            [conexp.fca.contexts :refer :all]
            [conexp.io.util      :refer :all]
            [conexp.io.latex     :refer :all]
            [clojure.string      :refer (split)]
            [clojure.data.xml    :as xml])
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
    (println (count objects))
    (println (count attributes))
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
  (with-in-reader file
    (loop [inz #{}]
      (let [line (read-line)]
        (if (not line)
          (make-context-nc (set-of g [[g m] inz])
                           (set-of m [[g m] inz])
                           inz)
          (let [[r g m] (re-matches #"^((?:[^,]+|\".*\")),((?:[^,]+|\".*\"))$" line)]
            (recur (conj inz [g m]))))))))

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


;;; TODO

;; slf
;; csc
;; csx?
;; tuples

;;;

nil
