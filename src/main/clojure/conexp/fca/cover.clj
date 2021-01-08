;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.cover
  (:require [conexp.base :refer :all]
             ; :exclude [next-closed-set]]
            [conexp.fca.contexts :refer :all] 
            [conexp.fca.fast :refer 
             [next-intent-async
              ;next-closed-set
              to-hashset to-binary-context 
              ;bitwise-attribute-derivation
              ;bitwise-object-derivation 
              to-binary-matrix
              bitwise-context-attribute-closure]]
             [clojure.core.reducers :as r]
             [clojure.core.async :refer [<!!]])
  (:import [java.util BitSet]))

;;;;;;;;;;;;;;;;;;;;; General Cover Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; merger
(defn- cover-merger 
  "Merges two cover relations in dictionary format."
  ([c1 c2]
   (merge-with (partial merge-with into) c1 c2))
  ([c1] c1) ([] {}))

(defn meet-irreducible-by-cover? 
  "This method checks if an element is meet irreducible in the cover
  relation."
  [element cover]
  (>= 1 (count (:upper (get cover element)))))

(defn join-irreducible-by-cover? 
  "This method checks if an element is meet irreducible in the cover
  relation."
  [element cover]
  (>= 1 (count (:lower (get cover element)))))


;;;;;;;;;;;;;;;;;;;;; Cover Methods for Intents ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; comperator
(defn- subsetneq? 
  "Tests if 'a is a subset of 'b, but not equal."
  [a b]
  (and (not (= a b)) 
       (subset? a b)))


;;; Generate Cover Relation as Dictionary
(defn- direct-lower-neighbor?
  "Checks if 'candidate is a direct lower neighbor of element in 'order"
  [order candidate element]
  (let [upper (get-in order [candidate :upper])
        next (filter #(subsetneq? % element) 
                     upper)]
    (empty? next)))

(defn- find-lower-neighbors 
  "Finds all direct lower neighbors of element in 'cover."
  [cover root element]
  (loop [search #{root} lower-covers #{}]
    (let [upper (r/reduce union #{} (for [e search] (get-in cover [e :upper])))
          next-search  (filter #(subsetneq? % element)
                               upper)
          direct-lower (filter #(direct-lower-neighbor? cover % element)
                               search)]
      (if (empty? next-search)
        (union direct-lower lower-covers)
        (recur next-search (union direct-lower 
                                  lower-covers))))))

(defn- build-size-bucket-map 
  "This method groups all sets in base-set according to their
  cardinality. These are returned as map."
  [base-set]
  (r/reduce (partial merge-with into)
            (for [b base-set] {(count b) #{b}})))


(defn- add-to-cover 
  "This method generates part of the cover structure. It generates a
  map that includes 'element assigned with all it's lower
  neighbors. Additionally, the map includes for each lower the
  reference that 'element is it's upper neighbor."
  [element lowers]
  (let [adder {element {:lower (set lowers) :upper #{}}}
        lower-updates 
        (r/reduce cover-merger 
                  {} (for [l lowers] {l {:upper #{element}}}))]
    (cover-merger adder lower-updates)))

(defn generate-cover 
  "For a base-set containing set's of elements, this method computed the
  cover relation of the subset order. The cover relation is returned
  as a map which assigns each element of the base-set to it's direct
  lower and upper neighbors in the following way:

  :element {:upper #{} :lower #{}}"
  [base-set]
  (let [buckets (build-size-bucket-map base-set)
        sort-keys (sort (keys buckets))
        root (first (get buckets (first sort-keys)))
        init-cover {root {:lower #{} :upper #{}}}]
    (loop [rest-sort-keys (rest sort-keys) cover init-cover]
      (if (empty? rest-sort-keys)
        cover
        (let [new-cover (r/reduce cover-merger cover 
                         (pmap #(add-to-cover % (find-lower-neighbors cover root %)) 
                              (get buckets (first rest-sort-keys))))]
          (recur (rest rest-sort-keys) new-cover))))))

;;; Methods to update context changes in the cover relation

;; object updates
(defn- into-but-supersets 
  "Inserts all element e of s2 into s1 if there exists no subset of e in s1"
  [s1 s2]
  (set (reduce (fn [s1 e] (if (some #(subsetneq? % e) s1) s1 
                              (conj s1 e))) s1 s2)))

(defn- into-but-subsets
    "Inserts all element e of s2 into s1 if there exists no superset of e in s1"
  [s1 s2]
  (set (reduce (fn [s1 e] (if (some #(subsetneq? e %) s1) s1 
                              (conj s1 e)))
               s1 s2)))

(defn- reassign-cover 
  "This method removes an intent from the cover structure and updates
  the cover relation's :lower and :upper assignments."
  [cover intent]
  (let [upper (:upper (get cover intent))
        lower (:lower (get cover intent))
        upper-updated (if (not (= 0 (count upper))) 
                        (loop [cur (first upper) other (rest upper) cur-update cover]
                          (let [newCover (-> cur-update 
                                             (update-in [cur :lower] disj intent)
                                             (update-in [cur :lower] into-but-subsets lower))]
                            (if (= 0 (count other)) newCover
                              (recur (first other) (rest other) newCover))))
                        cover)
        lower-updated (if (not (= 0 (count lower))) 
                        (loop [cur (first lower) other (rest lower) cur-update upper-updated] 
                          (let [newCover (-> cur-update
                                             (update-in [cur :upper] disj intent)
                                             (update-in [cur :upper] into-but-supersets upper))]
                            (if (= 0 (count other)) newCover
                              (recur (first other) (rest other) newCover))))
                        upper-updated)]
    (dissoc lower-updated intent)))

(defn- update-cover 
  "Removes all intents of toRemove in the cover relation and update
  all :lower and :upper assignments."
  [toRemove cover]
  (loop [cur (first toRemove) iterate (rest toRemove) updatedCover cover]
    (if (= 0 (count iterate))
      (reassign-cover updatedCover cur)
      (recur (first iterate) (rest iterate) (reassign-cover updatedCover cur)))))

(defn- remove-if-meet-irreducible 
  "This method removes an element in the cover structure if it is meet
  irreducible."
  [element cover]
  (if (meet-irreducible-by-cover? element cover)
    (reassign-cover cover element) 
    cover))

(defn- remove-meet-irreducible-lower-p 
  "This method removes all meet irreducible elements of the cover
  structure with cardinality lower then p."
  [cover p]
  (let [k (reverse
           (sort-by count (filter #(< (count %) p) (keys cover))))]
    (if (= 0 (count k)) cover
        (loop [cur (first k) other (rest k) updated cover]
          (if (= 0 (count other))
            (remove-if-meet-irreducible cur updated)
            (recur (first other) (rest other) (remove-if-meet-irreducible cur updated)))))))

;; update attributes

(defn- intersecter 
  "This is a helper method to update cover elements by
  intersection. It intersects the element and all it's :lower
  and :upper references with the new attr domain."
  [element cover attr] 
  (let [newkey (intersection element attr)
        newlower (disj (set (for [i (:lower cover)] (intersection attr i))) newkey)
        newupper (disj (set (for [i (:upper cover)] (intersection attr i))) newkey)]
    {newkey (assoc cover :lower newlower :upper newupper) }))

(defn attribute-intersection-cover 
  "This methods updates the concept lattice cover relation on a change
  of the attribute domain to a subset. For this the method intersects
  all keys with the new attribute domain. Additionally, elements
  which's intersection with 'attr are the same are merged."
  [cover attr]
  (let [toupdate (keys cover)
        merger (fn [c1 c2] (merge-with (partial merge-with into) c1 c2))]
    (loop [cur (first toupdate) other (rest toupdate) newcover {}]
      (let [updated-newcover (merger newcover 
                                     (intersecter cur (get cover cur) attr))]
        (if (= 0 (count other)) updated-newcover
          (recur (first other) (rest other) updated-newcover))))))

;; faster attribute deleter
;; (defn- next-closed-set-iterator
;;   "This method is a wrapper for the next-closed-set method in fast
;;   fca. It returns the next closed set given 'start."
;;   [[object-vector attribute-vector object-count attribute-count incidence-matrix] start]
;;   (let [o-prime (partial bitwise-object-derivation incidence-matrix object-count attribute-count),
;;         a-prime (partial bitwise-attribute-derivation incidence-matrix object-count attribute-count),
;;         next (next-closed-set attribute-count
;;                          (partial bitwise-context-attribute-closure
;;                                   object-count
;;                                   attribute-count
;;                                   incidence-matrix)
;;                          start)]
;;     next))

(defn- find-all-updates 
  "This method determines all updates that need to be made to the attribute lattice."
  [attribute-concepts cover]
  (loop [cur (set attribute-concepts) toupdate (set attribute-concepts)]
    (let [upper (future (set (reduce into #{} (pmap #(get-in cover [% :upper]) cur))))
          lower (set (pmap #(get-in cover [% :lower]) cur))
          nexttoupdate (r/reduce into toupdate (merge lower @upper))]
      (if (empty? cur)
        nexttoupdate
        (recur @upper nexttoupdate)))))

(defn attribute-deletion-cover 
  "This method updates the concept lattice given by cover on a
  deletion of attributes in the context."
  [cover old-ctx del-attributes]
  (if (empty? del-attributes) cover
    (let [prev-attributes (difference (attributes old-ctx)
                                      del-attributes)
          attr-order (into
                      (vec del-attributes) 
                      (vec prev-attributes))
          obj-vec (vec (objects old-ctx))
          attr-count (count attr-order)
          bin-incidence (to-binary-matrix obj-vec attr-order 
                                          (fn ([a b] ((incidence old-ctx) [a b]))
                                            ([[a b]] ((incidence old-ctx) [a b]))))
          bin-ctx [obj-vec attr-order (count obj-vec) attr-count  bin-incidence]
          attribute-concepts  (for [i del-attributes] 
                                (let [a (BitSet. attr-count)] 
                                  (.set a (.indexOf attr-order i))
                                  (to-hashset attr-order 
                                              (bitwise-context-attribute-closure (count obj-vec) (count attr-order) bin-incidence a))))
          toupdate (find-all-updates attribute-concepts cover)]
      (loop [cur (first toupdate) other (rest toupdate) newcover {}]
        (let [updated-newcover (cover-merger newcover (intersecter cur (get cover cur) prev-attributes))]
          (if (= 0 (count other))
            (cover-merger (apply dissoc cover toupdate) updated-newcover)
            (recur (first other) (rest other) updated-newcover)))))))



;; general updater for intent covers
(defn cover-reducer 
  "This method updates the cover relation if the context changed to a
  pq-core. For this it first updates the attribute domain. Secondly,
  all meet irreducible intents with cardinality lower then p are
  removed."
  ([cover ctx core p]
   (let [first-attr (if (not (= (attributes ctx) (attributes core)))
                      (attribute-deletion-cover cover ctx (difference (attributes ctx) (attributes core)))
                      cover)
         second-objects (if (not (= (objects ctx) (objects core)))
                          (remove-meet-irreducible-lower-p first-attr p)
                          first-attr)]
     second-objects)))

;;;;;;;;;;;;;;;;;;;;; Cover Methods for Concepts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helper

(defn- dual-entry 
  "This function swaps the extent and intent values of a cover entry."
  [old-cover intent {extent :extent lower :lower upper :upper}]
  (let [derive #(get-in old-cover [% :extent])]
    {extent {:lower (set (map derive upper)) :upper (set (map derive lower)) :extent intent}}))

(defn dual-concept-cover
  "This method swaps the intents and extent entries in the concept
  cover"
  [cover]
  (let [intents (keys cover)]
    (r/reduce merge (r/map #(dual-entry cover % (get cover %)) intents))))

;;; generate
(defn generate-concept-cover 
  "For a base-set containing set's of elements, this method computed the
  cover relation of the subconcept order. The cover relation is returned
  as a map which assigns each concept of the base-set to it's direct
  lower and upper neighbors in the following way:

  :intent {:upper {intent ..} :lower {intent ..} :extent #{}}"
  [base-set]
  (let [intents (map last base-set)
        extent-map (future (r/fold (partial merge) 
                                   (r/map #(hash-map (second %) {:extent (first %)})
                                          base-set)))
        intent-cover (future (generate-cover intents))
        concept-cover (merge-with into @intent-cover @extent-map)]
    concept-cover))


(defn- insert-as-root-concept 
  "This method inserts a concept into the cover relation."
  [intent extent]
  {intent {:lower #{} :upper #{} :extent extent}})

(defn insert-concept 
  "This method inserts a concept into the cover relation."
  [cover intent extent]
  (let [root-candidates (filter #(empty? (:lower (get cover %))) (keys cover))
        roots (filter #(subset? % intent) root-candidates)
        inserted (cover-merger cover (insert-as-root-concept intent extent))]
    (update (reduce cover-merger inserted 
             (for [root roots]
               (add-to-cover intent (find-lower-neighbors inserted root intent))))
     intent #(assoc % :extent extent))))

;; (defn attribute-insertion-cover 
;; "This method updates the concept lattice given by cover on an
;;   insertion of attributes to the context. The added attributes are
;;   already included in new-context and are further given by
;;   new-attributes."
;;   [cover new-ctx new-attributes]
;;   (if (empty? new-attributes) cover
;;       (let [prev-attributes (difference 
;;                              (attributes new-ctx)
;;                              new-attributes)
;;             attr-order (into
;;                         (vec new-attributes) 
;;                         (vec prev-attributes))
;;             obj-vec (vec (objects new-ctx))
;;             bin-incidence (to-binary-matrix obj-vec attr-order 
;;                                             (fn ([a b] ((incidence new-ctx) [a b]))
;;                                               ([[a b]] ((incidence new-ctx) [a b]))))
;;             bin-ctx [obj-vec attr-order (count obj-vec) (count attr-order)  bin-incidence]
;;             start (BitSet.)
;;             setter (.set start (count new-attributes) (count attr-order) true)
;;             cur-lattice (agent cover)
;;             bin-first-next (next-closed-set-iterator bin-ctx start)]
;;         (if (nil? bin-first-next) cover
;;             (loop [next (to-hashset attr-order bin-first-next) bin-next bin-first-next]
;;               (let [old (intersection next prev-attributes)]
;;                 ;; async update cover
;;                 (send-off cur-lattice insert-concept next (attribute-derivation new-ctx next))

;;                 (send-off cur-lattice 
;;                           #(if (= old 
;;                                   (to-hashset attr-order 
;;                                               (let [n (.clone bin-next) oldn (.and n start)]
;;                                                 (bitwise-context-attribute-closure (count obj-vec) (count attr-order) bin-incidence n))))
;;                              %
;;                              (reassign-cover % old)))
;;                 (if (= next (attributes new-ctx))
;;                   (do (await cur-lattice) @cur-lattice)
;;                   (let [bin-closure (next-closed-set-iterator bin-ctx bin-next)]
;;                     (recur (to-hashset attr-order bin-closure) bin-closure)))))))))

(defn attribute-insertion-cover 
"This method updates the concept lattice given by cover on an
  insertion of attributes to the context. The added attributes are
  already included in new-context and are further given by
  new-attributes."
  [cover new-ctx new-attributes]
  (if (empty? new-attributes) cover
      (let [intent-chan (next-intent-async new-ctx new-attributes :exlusive)
            prev-attributes (difference 
                             (attributes new-ctx)
                             new-attributes)
            cur-lattice (agent cover)]
        (loop [next-intent (<!! intent-chan)]
          (if (= :fin next-intent) (do (await cur-lattice) @cur-lattice)
              (let [old (intersection next-intent prev-attributes)]
                ;; async update cover
                (send-off cur-lattice insert-concept next-intent (attribute-derivation new-ctx next-intent))
                
                (send-off cur-lattice 
                          #(if (= old (context-attribute-closure new-ctx old))
                             %
                             (reassign-cover % old)))
                (recur (<!! intent-chan))))))))
