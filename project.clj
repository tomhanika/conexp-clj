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
  :dependencies [[org.clojure/clojure            "1.3.0"]
                 [org.clojure/tools.cli          "0.1.0"]
                 [org.clojure/core.incubator     "0.1.0"]
                 [org.clojure/math.combinatorics "0.0.1"]
                 [org.clojure.contrib/def        "1.3.0-SNAPSHOT"]
                 [org.clojure.contrib/graph      "1.3.0-SNAPSHOT"]
                 [org.clojure.contrib/lazy-xml   "1.3.0-SNAPSHOT"]
                 [org.clojure.contrib/math       "1.3.0-SNAPSHOT"]
                 [org.clojure.contrib/profile    "1.3.0-SNAPSHOT"]
                 [org.apache.commons/commons-math "2.1"]
                 [jline                          "0.9.94"]]
  :dev-dependencies [[swank-clojure "1.3.2"]]
  :aot [conexp.main conexp.contrib.gui]
  :keep-non-project-classes true
  :jar-name "conexp-clj.jar"
  :jvm-opts ["-server", "-Xmx1g"]
  :warn-on-reflection true)

(require 'clojure.java.io
         'robert.hooke
         'leiningen.deps)

(defn copy-file [name]
  (let [source (java.io.File. (str "stuff/libs/" name)),
        target (java.io.File. (str "lib/" name))]
    (when (not (.exists target))
      (println (str "Copying " name " to lib"))
      (clojure.java.io/copy source target))))

(robert.hooke/add-hook #'leiningen.deps/deps
                       (fn [f & args]
                         (apply f args)
                         (copy-file "G.jar")
                         (copy-file "LatDraw.jar")))

;;;

nil
