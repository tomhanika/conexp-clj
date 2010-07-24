;; Copyright: Daniel Borchmann, 2010
;; This file is in the public domain.

;; Computes a context of function used by Artem to explore certain
;; properties of functions on a 2-elemental set. After loading (which
;; may take a moment) you can use stem-base or explore-attributes to
;; explore the context function-context.

(in-ns 'user)

(use 'conexp.main)

;;; First define some properties of functions (not all defined by Artem)

(defn idempotent? [f S]
  (forall [A (subsets S)]
    (= (f (f A)) (f A))))

(defn intensive? [f S]
  (forall [A (subsets S)]
    (subset? (f A) A)))

(defn extensive? [f S]
  (forall [A (subsets S)]
    (subset? A (f A))))

(defn monotone? [f S]
  (forall [A (subsets S),
           B (subsets S)]
    (=> (subset? A B)
        (subset? (f A) (f B)))))

(defn antitone? [f S]
  (forall [A (subsets S),
           B (subsets S)]
    (=> (subset? A B)
        (subset? (f B) (f A)))))

(defn outcast? [f S]
  (forall [A (subsets S),
           B (subsets S)]
    (=> (and (subset? (f B) A) (subset? A B))
        (= (f B) (f A)))))

(defn heritage? [f S]
  (forall [A (subsets S),
           B (subsets S)]
    (=> (subset? A B)
        (subset? (intersection (f B) A)
                 (f A)))))

(defn constancy? [f S]
  (forall [A (subsets S),
           B (subsets S)]
    (=> (subset? A B)
        (and (=> (empty? (f B))
                 (empty? (f A)))
             (=> (not (empty? (intersection A (f B))))
                 (= (f A) (intersection (f B) A)))))))

(defn exchange? [f S]
  (forall [A (subsets S),
           x S, 
           y S]
    (=> (and (not= x y)
             (not (contains? (f A) x))
             (not (contains? (f A) y))
             (contains? (f (conj A x)) y))
        (contains? (f (conj A y)) x))))

(defn antiexchange? [f S]
  (forall [A (subsets S),
           x S, 
           y S]
    (=> (and (not= x y)
             (not (contains? (f A) x))
             (not (contains? (f A) y))
             (contains? (f (conj A x)) y))
        (not (contains? (f (conj A y)) x)))))

;;; Now generate all functions on an n-elemental set

(defn generate-numbers [n digits]
  (if (zero? n)
    [[]]
    (let [numbers (generate-numbers (- n 1) digits)]
      (for [num numbers,
            d digits]
        (conj num d)))))

(defn all-possible-funcs-on [base]
  ;; this is not the best implementation, see
  ;; clojure.contrib.combinatorics/selections
  (let [subs (subsets base)]
    (map #(zipmap subs %)
         (generate-numbers (expt 2 (count base)) subs))))

(defvar function-context
  (make-context (set (all-possible-funcs-on #{0 1}))
                '#{idempotent?, intensive?, extensive?, monotone?, antitone?,
                   outcast?,    heritage?,  exchange?,  constancy?}
                ;; Note: This is a trick to get good names for the
                ;; attributes (instead of Clojure's way of writing
                ;; functions)
                (fn [f p]
                  (@(resolve p) f #{0 1}))))

;;;

nil
