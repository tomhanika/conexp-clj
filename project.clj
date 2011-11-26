;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;;;

(defproject conexp-clj (.trim #=(slurp "VERSION"))
  :min-lein-version "1.3.0"
  :description "A ConExp rewrite in clojure"
  :url "http://daniel.kxpq.de/math/conexp-clj/"
  :dependencies [[org.clojure/clojure             "1.3.0"]
                 [org.clojure/tools.cli           "0.1.0"]
                 [org.clojure/core.incubator      "0.1.0"]
                 [org.clojure/math.combinatorics  "0.0.1"]
                 [org.clojure/math.numeric-tower  "0.0.1"]
                 [org.apache.commons/commons-math "2.1"]
                 [jline                           "0.9.94"]]
  :dev-dependencies [[swank-clojure "1.3.3"]]
  :aot [conexp.main
        conexp.contrib.gui
        conexp.contrib.java]
  :keep-non-project-classes true
  :jvm-opts ["-server", "-Xmx1g"]
  :warn-on-reflection true)

;;;

(use '[clojure.java.io    :only (copy)]
     '[clojure.java.shell :only (sh)]
     '[robert.hooke       :only (add-hook)])

(require 'leiningen.deps)

(defn copy-file [name]
  (let [source (java.io.File. (str "stuff/libs/" name)),
        target (java.io.File. (str "lib/" name))]
    (when-not (.exists target)
      (println (str "Copying " name " to lib"))
      (copy source target))))

(add-hook #'leiningen.deps/deps
          (fn [f & args]
            (apply f args)
            (copy-file "G.jar")
            (copy-file "LatDraw.jar")))

;;;

nil
