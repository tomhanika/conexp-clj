(ns #^{:doc "Package for computing retracts from formal contexts."}
  conexp.contrib.retracts
  (:use [conexp.fca.contexts]))

(defn homomorphism-by-csc
  "Returns the homomorphisms obtained by resticting every concept to the
  given compatible subcontext. (csc == compatible subcontext)"
  [subcontext]
  (fn [concept]
    (restrict-concept concept subcontext)))

(defn homomorphisms-by-cscs
  "Returns all homomorphisms originating from compatible subcontexts."
  [context]
  (map homomorphism-by-csc (compatible-subcontexts context)))

(defn endofunctions-as-hash
  "Given a homomorphisms which has been obtained by a compatible
  subcontext this function computes a hash representing all
  endofunctions of context originating from this homomorphism. The
  result may be thought of as a \"multifunction\"."
  [context hom]
  (reduce (fn [hash concept]
	    (assoc hash (hom concept) concept))
	  {}
	  (concepts context)))

(defn endofunctions-from-hash
  "Computes all endofunctions (as a hash) in endo-hash."
  [endo-hash]
; alle einzelnen Endofunktionen aus endo-hash nacheinander extrahieren
  nil)

(defn endofunctions-by-homomorphism
  "Returns all endofunctions obtained by homomorphism."
  [context hom]
  (endofunctions-from-hash (endofunctions-as-hash context hom)))

(defn retract?
  "Tests whether given concept endofunction, given as hash-map, is a
  retract of context."
  [context endo]
; Begriffe zählen, um Retraktionen zu erhalten
  nil)

(defn retracts
  "Returns all retracts of context as computed by the algorithm of
  Felix Kästner."
  [context]
  (for [hom (homomorphisms-by-cscs context)
	endo (endofunctions-by-homomorphism context hom)
	:when (retract? endo)]
    endo))

nil
