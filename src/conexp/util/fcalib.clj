;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.util.fcalib
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications)
  (:import [de.tudresden.inf.tcs.fcaapi FCAImplication FCAObject ObjectDescription]
           [de.tudresden.inf.tcs.fcalib Implication FullObject FormalContext ImplicationSet]))

(ns-doc "Functions to convert to and from fcalib")

;;

(defn stringify-context
  "Applies «str» to all objects and attributes of the given formal context «ctx» (in
  conexp-clj format).  Note that if two objects or attributes receive the same string
  representation this way, they are merged into one."
  [context]
  (-> context
      (rename-attributes str)
      (rename-objects str)))

(defn ^FormalContext to-fcalib-context
  "Converts formal contexts as they are used by conexp-clj to formal contexts as used by fcalib.

  Note that currently, due to implementation-specific restrictions, only formal contexts
  having objects and attributes as strings are allowed."
  [conexp-clj-context]
  (let [^FormalContext fcalib-context (fcatools.conexpng.model.FormalContext.)]
    (doseq [m (attributes conexp-clj-context)]
      (.addAttribute fcalib-context m))
    (doseq [g (objects conexp-clj-context)]
      (let [^FCAobject fcalib-g (FullObject. g (oprime conexp-clj-context #{g}))]
        (.addObject fcalib-context fcalib-g)))
    fcalib-context))

(defn from-fcalib-context
  "Converts formal contexts as they are used by fcalib to formal contexts as used by conexp.clj."
  [^FormalContext fcalib-context]
  (let [full-objects        (set (.getObjects fcalib-context)),
        attributes          (set (.getAttributes fcalib-context)),
        object-descriptions (reduce! (fn [map, ^FCAObject full-object]
                                       (assoc! map
                                         (.getIdentifier full-object)
                                         (.getDescription full-object)))
                                     {}
                                     full-objects),
        objects             (set (keys object-descriptions))]
    (make-context-nc objects
                     attributes
                     (set-of [g m] [g objects
                                    m attributes
                                    :when (.containsAttribute ^ObjectDescription (get object-descriptions g)
                                                              m)]))))

(defn ^FCAImplication to-fcalib-implication
  "Convert implications as used in conexp-clj to implications as used by fcalib."
  [conexp-clj-implication]
  (Implication. (premise conexp-clj-implication)
                (conclusion conexp-clj-implication)))

(defn from-fcalib-implication
  "Convert implications as used in fcalib to implications as used in conexp-clj."
  [^FCAImplication fcalib-implication]
  (make-implication (.getPremise fcalib-implication)
                    (.getConclusion fcalib-implication)))

(defn ^ImplicationSet to-fcalib-implication-set
  "Given a formal context and a sequence of implications of this contexts, returns an
  ImplicationSet as used in fcalib containing the implications, and having the given
  context as base context."
  [conexp-clj-context conexp-clj-implications]
  (let [^ImplicationSet fcalib-implication-set (ImplicationSet. (to-fcalib-context conexp-clj-context))]
    (doseq [impl (map to-fcalib-implication conexp-clj-implications)]
      (.add fcalib-implication-set impl))
    fcalib-implication-set))

;;

nil
