;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.math.algebra)

;;;

(defn closure-induced-preorder
  "Given a closure operation returns the preorder function for two elements a
   and b; 'a (<=_L) b'."
  [closure]
  (fn [a b] (some #{b} (closure a))))

(defn closure-equivalence
  "Given a set and closure operation computes the equivalence classes."
  [set closure]
  (group-by closure set))

;;;

nil
