;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.api.namespace
  (:use conexp.main
        conexp.layouts.force
        conexp.layouts.freese
        conexp.layouts.layered
        conexp.layouts.dim-draw
        conexp.api.shorthands))

;;;

(def functions
  (concat
    ;; get all functions defined in standard conexp-clj namespaces 
    (map name
      (keys 
        (merge 
          (apply merge (map ns-publics conexp-clj-namespaces))
          (apply merge (map 
                         #(ns-publics (symbol %)) 
                         (list 
                           "conexp.layouts.base"
                           "conexp.layouts.common"
                           "conexp.layouts.force"
                           "conexp.layouts.freese"
                           "conexp.layouts.layered"
                           "conexp.layouts.dim-draw"
                           "conexp.api.shorthands"))))))
    ;; just all wanted core functions
    (list "count" 
          "+" 
          "-" 
          "*" 
          "/")))

(defn whitelist-names
  "Gathers all relevants functions across conexp-clj and tests any input."
  [string]
  (some? (some #{string} functions)))

;;;

nil
