;; Copyright: Daniel Borchmann, 2010
;; This file is in the public domain.

;; A simple macro that computes a special context from a given set of
;; implications (it's the standard context of the closure system of
;; the given implications). Use it with
;; 
;;   (closure-context #{1 2 3 4}
;;                    1 2   -> 3  --
;;                    2     -> 4  --
;;                    1 3 4 -> 2)
;; 
;; It will give you something like
;; 
;;            |#{1 3} #{1 4} #{2 4} #{3 4} #{2 3 4}
;;     -------+-------------------------------------
;;     #{1}   |x      x      .      .      .
;;     #{3}   |x      .      .      x      x
;;     #{4}   |.      x      x      x      x
;;     #{2 4} |.      .      x      .      x
;; 
;; Note that -- is used to separate the implication descriptions and
;; that #{..} is Clojure's notation for sets. This file also contains
;; implementations for {intersection,union}-of-closure-system.


(in-ns 'user)

(use 'conexp.main)

(defn closure-context-by-implications
  "For a given collection of implications and a base set returns the
  full ordinal scala of the lattice of all closed subsets of the base
  set with respect to the given implications."
  ;; nearly the same as context-for-clop
  [base-set implications]
  (let [clop (clop-by-implications implications),
        base (all-closed-sets base-set clop)]
    (reduce-context-attributes
     (make-context (set-of (clop #{i}) [i base-set])
                   base
                   subset?))))

(defn- implications-from-macro
  "Implements the syntax used by closure-context."
  [implication-specification]
  (let [impl-blocks (remove #(= '-- (first %))
                            (partition-by #(= '-- %)
                                          implication-specification))]
    (vec (for [impl-spec impl-blocks]
           (let [parts (partition-by #(= '-> %) impl-spec)]
             (when-not (= 3 (count parts))
               (illegal-argument (str "Malformed implication specification: "
                                      impl-spec ".")))
             `(make-implication ~(set (nth parts 0))
                                ~(set (nth parts 2))))))))

(defmacro closure-context
  "For a given collection of implications and a base set returns the
  full ordinal scala of the lattice of all closed subsets of the base
  set with respect to the given implications. Implications are written
  as shown:

    (closure-context #{1 2 3 4}
                     1 2 3 -> 4 --
                     3 4   -> 1 --
                     1 4   -> 2)

  Note that the first argument is the base set. After that the
  implications are written as <elements> -> <elements>, separated by a
  --. Note that the elements can be everything except the symbols ->
  and -- (of course)."
  [base-set & implications]
  (let [implications (implications-from-macro implications)]
    `(do
       (let [impls# ~implications]
         (when-not (forall [impl# impls#]
                           (and (subset? (premise impl#) ~base-set)
                                (subset? (conclusion impl#) ~base-set)))
           (illegal-argument "Given implications are not compatible with "
                             "given base-set."))
         (closure-context-by-implications ~base-set impls#)))))

;;;

(defn intersection-of-closure-systems
  "Given two contexts ctx-1 and ctx-2 with the same object set
  computes a context for the intersection of the concept lattices of
  both."
  [ctx-1 ctx-2]
  (assert (= (objects ctx-1) (objects ctx-2)))
  (closure-context-by-implications (objects ctx-1)
                                   (union (stem-base (dual-context ctx-1))
                                          (stem-base (dual-context ctx-2)))))

(defn union-of-closure-systems
  "Given two contexts ctx-1 and ctx-2 with the same object set
  computes a context for the smalles lattice containing the union of
  both concept lattices."
  [ctx-1 ctx-2]
  (assert (= (objects ctx-1) (objects ctx-2)))
  (context-apposition ctx-1 ctx-2))

;;;

nil