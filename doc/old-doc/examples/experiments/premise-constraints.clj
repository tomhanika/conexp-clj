;;;
;;; Hypergraph Transversals with Cardinality Constraints
;;;

(ns conexp.contrib.experimental.premise-constraints
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications))

;;; Hypergrap Tranversals with Predicates

(defn- intersection-set?
  "Tests whether set has non-empty intersection with every set in sets."
  [set sets]
  (forall [other-set sets]
    (exists [x set]
      (contains? other-set x))))

(defn minimal-hypergraph-transversals-with-constraints
  "Returns all minimal hypergraph transversals of the hypergraph defined by «edges» on the
  vertex sets «vertices»."
  [vertices edges predicate]
  (let [cards    (map-by-fn (fn [x]
                              (count (set-of X | X edges :when (contains? X x))))
                            vertices),
        elements (sort #(compare (cards %2) (cards %1))
                       vertices),
        result   (atom []),
        search   (fn search [rest-sets current rest-elements]
                   (cond
                    (exists [x current]
                      (intersection-set? (disj current x) edges))
                    nil,
                    ;;
                    (not (predicate current))
                    nil,
                    ;;
                    (intersection-set? current edges)
                    (swap! result conj current),
                    ;;
                    :else
                    (when-let [x (first rest-elements)]
                      (when (exists [set rest-sets]
                              (contains? set x))
                        (search (remove #(contains? % x) rest-sets)
                                (conj current x)
                                (rest rest-elements)))
                      (search rest-sets
                              current
                              (rest rest-elements)))))]
    (search edges #{} elements)
    @result))

;;; Testing

(defn proper-premises-for-attribute-1
  [ctx m predicate]
  (let [objs (set-of g | [g n] (down-arrows ctx) :when (= n m))]
    (minimal-hypergraph-transversals-with-constraints
     (disj (attributes ctx) m)
     (set-of (difference (attributes ctx) (oprime ctx #{g})) | g objs)
     predicate)))

(def testing-context (make-context 10 10 #(= 1 (gcd %1 %2))))

(defn proper-premise-implications-1
  "Returns the proper premises of the given context ctx as a lazy sequence."
  [ctx predicate]
  (doall
   (with-var-bindings [conexp.base/minimal-hypergraph-transversals
                       #(minimal-hypergraph-transversals-with-constraints %1 %2 predicate)]
     (proper-premise-implications ctx))))

;;;

nil
