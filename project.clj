;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(defproject conexp-clj "2.0.3-SNAPSHOT"
  :min-lein-version "2.0.0"
  :description "A ConExp rewrite in clojure"
  :url "http://github.com/tomhanika/conexp-clj/"
  :scm {:url "git@github.com:tomhanika/conexp-clj.git"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure             "1.9.0"]
                 [org.clojure/core.async          "0.4.490"]
                 [org.clojure/data.xml            "0.0.8"]
                 [org.clojure/data.json           "0.2.6"]
                 [org.clojure/math.combinatorics  "0.1.5"]
                 [org.clojure/math.numeric-tower  "0.0.4"]
                 [org.clojure/tools.cli           "0.4.1"]
                 [org.apache.commons/commons-math "2.2"]
                 [seesaw                          "1.5.0"]
                 [reply                           "0.4.3"
                  :exclusions [org.clojure/clojure
                               clojure-complete
                               com.cemerick/drawbridge]]
                 [aysylu/loom "1.0.2"]
                 [rolling-stones                  "1.0.1"
                  :exclusions [org.clojure/clojure]]
                 [clj-http "3.10.0"]
                 [clojure-complete "0.2.5"]]
  :profiles {:uberjar {:main conexp.main
                       :aot :all}}
  :plugins [[org.clojars.benfb/lein-gorilla "0.6.0"]]
  :keep-non-project-classes true
  :source-paths ["src/main/clojure" "src/test/clojure"]
  :java-source-paths ["src/main/java"]
  :test-paths ["src/test/clojure"]
  :resource-paths ["src/main/resources"]
  :target-path "builds/%s"
  :compile-path "%s/classes/"
  :java-opts ["-Dawt.useSystemAAFontSettings=on"])
