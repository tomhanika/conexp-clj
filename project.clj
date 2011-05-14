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
  :url "http://www.math.tu-dresden.de/~borch/conexp-clj/"
  :repositories {"snapshots" "https://oss.sonatype.org/content/repositories/snapshots/",
                 "releases"  "https://oss.sonatype.org/content/repositories/releases/"}
  :dependencies [[org.clojure/clojure               "1.3.0-master-SNAPSHOT"]
                 [org.clojure.contrib/combinatorics "1.3.0-SNAPSHOT"]
                 [org.clojure.contrib/command-line  "1.3.0-SNAPSHOT"]
                 [org.clojure.contrib/core          "1.3.0-SNAPSHOT"]
                 [org.clojure.contrib/def           "1.3.0-SNAPSHOT"]
                 [org.clojure.contrib/except        "1.3.0-SNAPSHOT"]
                 [org.clojure.contrib/graph         "1.3.0-SNAPSHOT"]
                 [org.clojure.contrib/lazy-xml      "1.3.0-SNAPSHOT"]
                 [org.clojure.contrib/math          "1.3.0-SNAPSHOT"]
                 [org.clojure.contrib/profile       "1.3.0-SNAPSHOT"]
                 [org.clojure.contrib/set           "1.3.0-SNAPSHOT"]
                 [org.apache.commons/commons-math   "2.0"]
                 [jline                             "0.9.94"]]
  :dev-dependencies [[swank-clojure "1.3.0"]]
  :aot [conexp.main conexp.contrib.gui conexp.contrib.JavaInterface]
  :keep-non-project-classes true
  :omit-source true
  :jar-name "conexp-clj.jar"
  :jvm-opts ["-server", "-Xmx1g"]
  :warn-on-reflection true)

;;;

(use '[leiningen.core     :only (prepend-tasks)]
     '[clojure.java.io    :only (copy)]
     '[clojure.java.shell :only (sh)]
     '[robert.hooke       :only (add-hook)])

(require 'leiningen.deps
         'leiningen.compile)

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

(prepend-tasks #'leiningen.compile/compile
               (fn [_]
                 (println "Generating Java Interface")
                 (sh "java"
                     "-cp" "./lib/*:./src/"
                     "clojure.main" "-e"
                     "(do (require 'conexp.contrib.java) (conexp.contrib.java/generate-java-interface \"src/conexp/contrib/JavaInterface.clj\"))")))

;;;

nil
