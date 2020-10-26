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


(derive ::all ::quant)
(derive ::ex ::quant)


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
  "Ends the Scale Navigation."
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

(define-repl-fn logical-attr
  "Generates a new attribute as logical formula of existing attributes."
  (let [formula (ask (str "Current Attributes: \n " (clojure.string/join " " (attributes scale)) "\n Please enter a logical formula using logical formuals e.g. \"(C :or (A :and (:not B)))\": \n ")
                     #(read-string (str (read-line)))
                     (partial formula-syntax-checker scale)
                     "Enter a formula in nested list format using only attributes of the scale: \n")
        formula-derivation (logical-attribute-derivation scale formula)
        formula-name (ask (str "Enter a name for your formula: \n ")
                          #(str (read-line))
                          (constantly true) 
                          "")]
    (if (extent? (context (:smeasure state)) formula-derivation)
      (assoc state :smeasure (make-smeasure-nc (-> state :smeasure context)
                                               (make-context (objects scale) 
                                                             (conj (attributes scale) formula-name)
                                                             (union (cross-product formula-derivation #{formula-name}) 
                                                                    (incidence-relation scale)))
                                               (-> state :smeasure measure)))
      state)))

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

(defn conceptual-navigation
  "Exploration for a scale context to measure a given context.
  The exploration is done with online editing methods.

  - rename:        Rename objects or attributes of the scale
  - logical-attr:  Clusters objects or attributes in the scale
                   The cluster incidence is set as either the common
                   or conjoined incidences of all entries
  - truncate:      Removes attributes form the scale 
  - clear:         Restarts the exploration

  general functions for exploration interaction
  - show:          prints the current scale
  - done:          finishes exploration
  - help:          prints doc string"
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
