;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.base
  "Common namespace for conexp-clj IO functions."
  (:use conexp.base))

;;;

(defn available-formats
  "Returns for a given type (as string, i.e. \"context\") all
  available output methods."
  [type]
  (let [namespace (str "conexp.io." type "s")]
    (try
      (do
        (require (symbol namespace))
        (let [writer (resolve (symbol namespace (str "write-" type)))]
          (remove #(or (= :default %)
                       (= :conexp.io.util/default-write %))
                  (keys (methods @writer)))))
      (catch java.io.FileNotFoundException _
        (illegal-argument "available-formats: unknown type \"" type "\"")))))

;;;

true
