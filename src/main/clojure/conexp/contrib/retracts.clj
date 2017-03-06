;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.retracts
  "Package for computing retracts from formal contexts."
  (:use [conexp.base :only (set-of illegal-argument)]
        [conexp.fca.contexts :only (restrict-concept
                                    concepts
                                    objects
                                    attributes
                                    incidence
                                    make-context
                                    object-derivation
                                    attribute-derivation
                                    intents
                                    context-reduced?)]
        [conexp.fca.misc :only (compatible-subcontexts)])
  (:use [clojure.pprint :only (cl-format)]))

;;;

(defn- homomorphism-by-csc
  "Returns the homomorphisms obtained by resticting every concept to the
  given compatible subcontext. (csc == compatible subcontext)"
  [subcontext]
  (fn [concept]
    (restrict-concept concept subcontext)))

(defn- homomorphisms-by-cscs
  "Returns all homomorphisms originating from compatible
  subcontexts. context has to be reduced."
  [context]
  (map homomorphism-by-csc (compatible-subcontexts context)))

(defn- endofunctions-as-hash ; name is misleading
  "Given a homomorphisms which has been obtained by a compatible
  subcontext this function computes a hash representing all
  endofunctions of context originating from this homomorphism. The
  result may be thought of as a \"multifunction\"."
  [context hom]
  (persistent!
   (reduce (fn [hash concept]
             (assoc! hash
               (hom concept) (conj (hash (hom concept)) concept)))
           (transient {})
           (concepts context))))

(defn- endofunctions-from-hash ; name is misleading
  "Computes all endofunctions (as a hash) in endo-hash."
  [endo-hash]
  (let [runner (fn runner [func rem]
                 (if (empty? rem)
                   [func]
                   (let [[preimg imgs] (first rem)]
                     (for [img imgs,
                           fun (runner (assoc func preimg img)
                                       (rest rem))]
                       fun))))]
    (runner {} (seq endo-hash))))

(defn- endofunctions-by-homomorphism ; name is misleading
  "Returns all endofunctions obtained by homomorphism."
  [context hom]
  (endofunctions-from-hash (endofunctions-as-hash context hom)))

(defn- retract?
  "Tests whether given concept endofunction, given as hash-map, is a
  retract of context or not."
  [context endo]
  (let [zero-concept [(objects context),
                      (object-derivation context (objects context))]
        one-concept  [(attribute-derivation context (attributes context)),
                      (attributes context)]]
    (and (= zero-concept (endo zero-concept))
         (= one-concept (endo one-concept))
         (let [imgs    (vals endo)
               img-ctx (make-context (objects context)
                                     (attributes context)
                                     (set-of [g m] [[objs atts] imgs,
                                                    g objs,
                                                    m atts]))]
           (= (count (distinct imgs))
              (count (intents img-ctx)))))))

(defn retracts
  "Returns all retracts of context as computed by the algorithm of
  Felix Kästner."
  [context]
  (when-not (context-reduced? context)
    (illegal-argument "Can only compute retracts of reduced contexts."))
  (let [concepts (concepts context)]
    (for [hom  (homomorphisms-by-cscs context),
          endo (endofunctions-by-homomorphism context hom),
          :let [ret (zipmap concepts
                            (map (comp endo hom) concepts))]
          :when (retract? context ret)]
      ret)))

(defn- retract-to-pprint-str
  "Returns a string for pretty printing a retract of a context."
  [retract]
  (let [key-value-pairs (sort (fn [[[A_1, _] _] [[A_2, _] _]]
                                (< 0 (compare (vec A_1) (vec A_2))))
                              (seq retract)),
        string-pairs    (map #(map str %) key-value-pairs),
        max-key         (reduce max 0 (map (comp count first) string-pairs)),
        max-val         (reduce max 0 (map (comp count second) string-pairs))]
    (map (fn [[k v]]
           (cl-format nil (str "~" max-key "@A  +->  ~" max-val "@A") k v))
         string-pairs)))

(defn pprint-retracts
  "Pretty prints retracts of context"
  [context]
  (doseq [r (retracts context)]
    (cl-format true "~&~{~a~^~%~}~%~%" (retract-to-pprint-str r))))

;;;

nil
