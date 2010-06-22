;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; This file has been written by Immanuel Albrecht, with modifications by DB

(ns conexp.contrib.gui.editors.context-editor.util
  (:use [clojure.contrib.string :only (join)]))


;; Helper functions

(defn string-to-cross
  "Takes a string and decides, whether it should be a cross or not."
  [s]
  (let [trimmed (.trim s)]
    (if (re-matches #"0*[,.]?0*" trimmed)
      false
      true)))

(defn smart-str
  "Takes a single argument and returns it as a string that is usable
   for object and attribute names."
  [x]
  (cond
   (or (vector? x)
       (seq? x))
   (join " " (map smart-str x)),
   (set? x)
   (str "{" (join ", " (map smart-str (vec x))) "}"),
   :else
   (str x)))

(defn map-to-unique-strings
  "Takes a sequence of (unique) keys and returns a map
   that maps each unique key to a unique string."
  [keys]
  (loop [k     (set keys),
         m     {},
         taken #{},
         nbr   0]
    (if (empty? k)
      m
      (let [k1        (first k),
            k1-cvrt   (smart-str k1),
            k1-as-str (if (< 0 nbr)
                        (join "-" [k1-cvrt (str nbr)])
                        k1-cvrt)]
        (if (contains? taken k1-as-str)
          (recur k m taken (+ 1 nbr))
          (recur (disj k k1)
                 (conj m {k1 k1-as-str})
                 (conj taken k1-as-str) 0))))))

(defn req-unique-string
  "Takes a sequence of keys and a new requested key and
  returns a new key that will not conflict with any of the
  keys in the sequence."
  [old-keys req-key]
  (let [keys (set old-keys)]
    (loop [nbr 0]
      (let [new-key (if (< 0 nbr)
                      (join "-" [(str req-key) (str nbr)])
                      (str req-key))]
        (if (contains? keys new-key)
          (recur (+ 1 nbr))
          new-key)))))

(defn switch-bipartit-auto
  "Takes a bipartit auto map and removes the old-key and old-value
   associations and associates new-key with new-value and new-value
   with new-key, then returns the new map."
  [m old-key old-value new-key new-value]
  (let [m-without-old (dissoc m old-key old-value),
        m-with-new    (assoc m-without-old new-key new-value new-value new-key)]
    m-with-new))

;;;

nil

