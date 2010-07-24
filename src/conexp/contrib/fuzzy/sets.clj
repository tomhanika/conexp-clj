;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.fuzzy.sets
  (:use conexp.main))

(ns-doc "Basic definitions for fuzzy sets")

(set! *warn-on-reflection* true)

;;;

(deftype Fuzzy-Set [^clojure.lang.PersistentHashMap hashmap]
  ;;
  Object
  (equals [this other]
    (boolean (or (identical? this other)
                 (when (= (class this) (class other))
                   (let [ohashmap (.hashmap ^Fuzzy-Set other)]
                     (and (forall [k (keys hashmap)]
                            (=> (not (zero? (hashmap k)))
                                (= (hashmap k) (ohashmap k))))
                          (forall [k (keys ohashmap)]
                            (=> (not (zero? (ohashmap k)))
                                (= (hashmap k) (ohashmap k))))))))))
  (hashCode [this]
    (hash-combine-hash Fuzzy-Set hashmap))
  (toString [this]
    (str hashmap))
  ;;
  clojure.lang.ISeq
  (first [this]
    (first hashmap))
  (next [this]
    (next hashmap))
  (more [this]
    (if-let [n (next hashmap)]
      n
      ()))
  (cons [this [k v]]
    (when-not (and (number? v)
                   (< 0 v)
                   (<= v 1))
      (illegal-argument "Fuzzy sets only support real values in (0,1]"))
    (Fuzzy-Set. (conj hashmap [k v])))
  (seq [this]
    (seq hashmap))
  (count [this]
    (count hashmap))
  (empty [this]
    (Fuzzy-Set. {}))
  (equiv [this other]
    (.equals ^Fuzzy-Set this other))
  ;;
  clojure.lang.IFn
  (invoke [this thing]
    (let [result (hashmap thing)]
      (or result 0)))
  (applyTo [this seq]
    (if (= 1 (count seq))
      (.applyTo hashmap seq)
      (illegal-argument "Cannot apply fuzzy sets to non-singleton sequences.")))
  ;;
  clojure.lang.Associative
  (containsKey [this o]
    (unsupported-operation "Fuzzy sets do not support the contains? operation."))
  (entryAt [this o]
    (.entryAt hashmap o))
  (assoc [this k v]
    (.cons this [k v])))

(defn make-fuzzy-set
  "Constructs a fuzzy set from a given hashmap. The values must be
  numbers between 0 and 1."
  [hashmap]
  (assert (forall [[k v] hashmap]
            (and (number? v)
                 (<= 0 v)
                 (<= v 1))))
  (Fuzzy-Set. (select-keys hashmap (remove #(zero? (hashmap %)) (keys hashmap)))))

(defmethod print-method Fuzzy-Set [set out]
  (.write ^java.io.Writer out
          ^String (str "#F" set)))

(defn fuzzy-set?
  "Tests thing for being a fuzzy set."
  [thing]
  (instance? Fuzzy-Set thing))

;;; TODO: set operations

;;;

nil
