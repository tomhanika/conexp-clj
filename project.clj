;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;;;

(defproject conexp-clj (.trim #=(slurp "VERSION"))
  :description "A ConExp rewrite in clojure"
  :url "http://www.math.tu-dresden.de/~borch/conexp-clj/"
  :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.2.0-SNAPSHOT"]
		 [org.apache.commons/commons-math "2.0"]
		 [jline "0.9.94"]]
  :dev-dependencies [[swank-clojure "1.2.1"]]
  :namespaces [conexp.main])

;;;

nil
