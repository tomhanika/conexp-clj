;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io
  (:use conexp.base)
  (:require conexp.io.latex
            conexp.io.contexts
	    conexp.io.lattices
	    conexp.io.layouts
            conexp.io.many-valued-contexts))

(ns-doc
 "Common namespace for conexp-clj IO functions.")

;;;

(immigrate 'conexp.io.latex
           'conexp.io.contexts
	   'conexp.io.lattices
	   'conexp.io.layouts
           'conexp.io.many-valued-contexts)

;;;

(defn available-formats
  "Returns for a given type (as string, i.e. \"context\") all
  available output methods."
  [type]
  (let [writer (resolve (symbol "conexp.io" (str "write-" type)))]
    (when (nil? writer)
      (illegal-argument "Unknown type " type " given to available-formats."))
    (remove #(or (= :default %)
                 (= :conexp.io.util/default-write %))
            (keys (methods @writer)))))

;;;

nil
