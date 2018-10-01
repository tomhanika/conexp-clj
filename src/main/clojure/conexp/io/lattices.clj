;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.lattices
  (:use conexp.base
        conexp.fca.lattices
        conexp.io.util)
  (:import [java.io PushbackReader]))

;;; Input format dispatch

(define-format-dispatch "lattice")
(set-default-lattice-format! :simple)

;;; Formats

;; Simple conexp-clj Format

(add-lattice-input-format :simple
                          (fn [rdr]
                            (= "conexp-clj simple" (read-line))))

(define-lattice-output-format :simple
  [lat file]
  (with-out-writer file
    (binding [*print-length* nil]
      (println "conexp-clj simple")
      (prn {:lattice [(base-set lat)
                      (set-of [x y]
                              [x (base-set lat)
                               y (base-set lat)
                               :when ((order lat) [x y])])]}))))

(define-lattice-input-format :simple
  [file]
  (with-in-reader file
    (let [_        (get-line),
          hash-map (binding [*in* (PushbackReader. *in*)]
                     (read)),
          lattice  (:lattice hash-map)]
      (when-not lattice
        (illegal-argument "File " file " does not contain a lattice."))
      (apply make-lattice lattice))))

;;; ConExp lattice format

'TODO

;;;

nil
