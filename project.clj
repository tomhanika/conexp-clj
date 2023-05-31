;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(defproject conexp-clj "2.3.1-SNAPSHOT"

  :min-lein-version "2.0.0"
  :description "A ConExp rewrite in clojure -- and so much more ..."
  :url "http://github.com/tomhanika/conexp-clj/"
  :scm {:url "git@github.com:tomhanika/conexp-clj.git"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure             "1.10.1"]
                 [org.clojure/core.async          "1.6.673"]
                 [org.clojure/data.int-map        "1.2.1"]
                 [org.clojure/data.json           "2.4.0"]
                 [org.clojure/data.xml            "0.0.8"]
                 [org.clojure/math.combinatorics  "0.2.0"]
                 [org.clojure/math.numeric-tower  "0.0.5"]
                 [org.clojure/tools.cli           "1.0.219"]
                 [org.apache.commons/commons-math "2.2"]
                 [org.clojure/algo.generic        "0.1.3"]
                 [seesaw                          "1.5.0"]
                 [reply                           "0.5.1"
                  :exclusions [org.clojure/clojure
                               clojure-complete
                               com.cemerick/drawbridge]]
                 [aysylu/loom "1.0.2"]
                 [rolling-stones "1.0.3"
                  :exclusions [org.clojure/clojure]]
                 [clj-http "3.12.3"]
                 [clojure-complete "0.2.5"]
                 [ring/ring-devel "1.10.0"]
                 [ring/ring-core "1.10.0"]
                 [ring/ring-json "0.5.1"]
                 [ring-cors "0.1.13"]
                 [http-kit "2.6.0"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [luposlip/json-schema             "0.4.1"]
                 [org.clojure/data.csv "1.0.1"]]
  :profiles {:uberjar {:main conexp.main
                       :dependencies [[javax.servlet/servlet-api "2.5"]
                                      [ring/ring-mock "0.4.0"]
                                      [nrepl/nrepl "1.0.0"]]
                       :plugins [[lein-aot-order "0.1.0"]]
                       :aot :order}
             :dev {:main conexp.main
                   :dependencies [[javax.servlet/servlet-api "2.5"]
                                  [ring/ring-mock "0.4.0"]
                                  [nrepl/nrepl "1.0.0"]]
                   :plugins [[lein-aot-order "0.1.0"]]
                   :javac-options ["-Xlint:deprecation" "-Xlint:unchecked"]}
             :gorilla {:main conexp.main
                       :plugins [[org.clojars.benfb/lein-gorilla "0.7.0"]]}}
  :keep-non-project-classes true
  :source-paths ["src/main/clojure" "src/test/clojure"]
  :java-source-paths ["src/main/java"]
  :test-paths ["src/test/clojure"]
  :resource-paths ["src/main/resources"]
  :target-path "builds/%s"
  :compile-path "%s/classes/"
  :java-opts ["-Dawt.useSystemAAFontSettings=lcd_hbgr" "-Xmx4G"])
