;;;
;;; Task: Compute k-elemental sets of preconcept covering the incidence relation
;;;
;;; Author: Daniel Borchmann, License: CC0
;;;

;;; Preamble

(use 'conexp.main
     'clojure.test)
(require '[clojure.math.combinatorics :as c]
         '[clojure.pprint :as f]
         '[clojure.core.reducers :as r])

;;; Implementation

(defn preconcepts
  "Computes the preconcepts of the given formal context ctx"
  [ctx]
  (set-of [A B] | A (subsets (objects ctx)), B (subsets (oprime ctx A))))

(defn k-elemental-concept-covers
  "Computes all k-elemental subsets of the concepts of ctx which cover the incidence
  relation if ctx"
  [ctx k]
  (assert (and (number? k) (<= 0 k)))
  (for [pcs (c/combinations (concepts ctx) k)
        :when (and (forall [[g m] (incidence ctx)]
                     (exists [[A B] pcs]
                       (and (contains? A g)
                            (contains? B m)))))]
    pcs))

(defn k-elemental-intent-semiconcept-covers
  "Computes all k-elemental subsets of the semiconcepts of ctx which cover the incidence
  relation if ctx, where the intents of the semiconcepts are actually intents of ctx."
  [ctx k]
  (assert (and (number? k) (<= 0 k)))
  ;; this is yet another Ganter's algorithm
  (let [precovers (fn precovers [cover inc]
                    (if (empty? cover)
                      (if (empty? inc)
                        '(())
                        '())
                      (let [[A B] (first cover)]
                        (for [A-bar (subsets A),
                              precover (precovers (rest cover)
                                                  (set-of [g m] | [g m] inc
                                                                  :when (not (and (contains? A-bar g)
                                                                                  (contains? B m)))))]
                          (conj precover [A-bar B])))))]
    (for [cover (k-elemental-concept-covers ctx k),
          precover (precovers cover (incidence ctx))]
      precover)))

(defn preconcepts-to-context
  "Given a collection of preconcepts of ctx, returns the formal contexts which has has
  rows the extents of the preconcepts in this collection"
  [ctx coll-of-preconcepts]
  (let [extents (vec (map first coll-of-preconcepts))]
    (make-context (objects ctx)
                  (count extents)
                  (fn [g n]
                    (contains? (nth extents n) g)))))

(defn main
  "Given a formal context ctx and a natural number k, computes all k-elemental sets of
  preconcepts of ctx that cover its incidence relation.  For these sets of preconcepts it
  computes the formal context that has all the extents as object extents, and writes those
  contexts to files (in Burmeister format) starting with file-prefix."
  [ctx k file-prefix]
  (let [count (atom 0)]
    (doseq [pcs (k-elemental-intent-semiconcept-covers ctx k)]
      (write-context :burmeister
                     (preconcepts-to-context ctx pcs)
                     (f/cl-format false "~A-~D.ctx" file-prefix @count))
      (swap! count inc))))

;;; Tests

(def ctx-1 (make-context 7 7
                         #{[2 1] [3 2] [6 5] [1 0] [2 2] [3 3] [4 4]
                           [6 6] [0 0] [2 3] [3 4] [4 5] [5 6] [0 1]
                           [1 2] [2 4] [3 5] [4 6] [2 5] [3 6] [0 3]
                           [1 4] [0 4] [1 5] [0 5] [1 6] [6 0] [5 1]
                           [3 0] [6 3] [5 3] [2 0]}))

(def ctx-2 (make-context 5 5
                         #{[3 2] [1 0] [0 0] [3 4] [0 1] [2 4] [0 2]
                           [1 3] [0 3] [1 4] [0 4] [3 1] [2 0]}))

(def ctx-3 (make-context '[a b c]
                         [1 2 3]
                         '#{[a 1] [a 2]
                            [b 1] [b 3]
                            [c 3]}))

(def ctx-A (make-context-from-matrix ["Obj 1" "Obj 2" "Obj 3"]
                                     ["Attr 1" "Attr 2"]
                                     [1 1 1 1 1 0]))

(def ctx-RK (make-context #{"Obj 1" "Obj 2" "Obj 3" "Obj 4" "Obj 5" "Obj 6" "Obj 7" "Obj 8"}
                          #{"a" "b" "c" "d" "e" "f"}
                          #{["Obj 8" "a"] ["Obj 7" "a"] ["Obj 8" "b"] ["Obj 7" "b"] ["Obj 8" "c"]
                            ["Obj 5" "a"] ["Obj 7" "c"] ["Obj 5" "b"] ["Obj 7" "d"] ["Obj 3" "a"]
                            ["Obj 5" "c"] ["Obj 6" "d"] ["Obj 8" "f"] ["Obj 2" "a"] ["Obj 4" "c"]
                            ["Obj 1" "a"] ["Obj 2" "b"] ["Obj 3" "c"] ["Obj 4" "e"] ["Obj 1" "c"]
                            ["Obj 4" "f"] ["Obj 1" "d"] ["Obj 1" "e"] ["Obj 1" "f"]}))

(deftest test-all
  (is (= 862 (count (preconcepts ctx-1))))
  (is (= 0 (count (k-elemental-concept-covers ctx-2 2))))
  ;; the following may not be preconcepts, but that's not important
  (is (= (preconcepts-to-context ctx-3 '[ [#{a b} #{1}] [#{a} #{2}] [#{a b c} #{1 3}] ])
         (make-context '[a b c] [0 1 2] '#{[b 0] [b 2] [c 2] [a 0] [a 1] [a 2]})))
  (is (= 4 (count (k-elemental-intent-semiconcept-covers ctx-A 2))))
  (is (forall [precover (k-elemental-intent-semiconcept-covers ctx-RK 5)]
        (= (incidence ctx-RK)
           (set-of [g m] | [A B] precover, g A, m B)))))

;;;

nil
