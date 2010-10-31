;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.doc
  (:use conexp.main)
  (:use [clojure.string :only (split replace)
                        :rename {replace replace-str}]))


;;; API Documentation

(defn- public-api
  "Returns a map of public functions of namespaces to their
  corresponding documentation."
  [ns]
  (let [ns (find-ns ns)]
    (into {}
          (map (fn [[function var]]
                 [function (str (:arglists (meta var))
                                "\n\n"
                                (:doc (meta var)))])
               (filter (fn [[f var]]
                         (and (= (:ns (meta var)) ns)
                              (not (Character/isUpperCase ^Character (first (str f))))))
                       (ns-map ns))))))


;;; Documentation Coverage

(defn conexp-fns-needing-doc
  "Returns function in public conexp-clj api not having documentation."
  []
  (for [ns conexp-namespaces,
        [f _] (public-api ns)
        :when (not (:doc (meta (resolve (symbol (str ns) (str f))))))]
    (symbol (str ns) (str f))))

;;;

nil
