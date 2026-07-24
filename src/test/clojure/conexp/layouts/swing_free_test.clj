;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.swing-free-test
  "Regression guard for the GUI/Swing decoupling: the computational core
  (FCA, layouts, I/O, math) and the Java it loads must stay free of Swing/AWT
  so it can be reused headlessly (e.g. by a web/REST frontend). java.awt.geom
  is allowed — it is pure, headless-safe geometry."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;;; The core source trees that must not depend on Swing/AWT.

(def core-clojure-dirs
  ["src/main/clojure/conexp/layouts"
   "src/main/clojure/conexp/io"
   "src/main/clojure/conexp/fca"
   "src/main/clojure/conexp/math"])

;; Java packages reachable from the core Clojure above (the Freese layout loads
;; org.latdraw.*; dim-draw / util.graph load org.dimdraw.*). The GUI-side
;; graphics libraries (no.geosoft.*, formerly org.latdraw.beans) are NOT core
;; and are intentionally excluded.
(def core-java-dirs
  ["src/main/java/org/latdraw"
   "src/main/java/org/dimdraw"])

;; java.awt.geom is permitted (pure geometry); anything else under java.awt,
;; all of javax.swing, and seesaw are forbidden in the core.
(def forbidden-import #"javax\.swing|seesaw|java\.awt(?!\.geom)")

(defn- source-files [dirs exts]
  (->> dirs
       (mapcat #(file-seq (io/file %)))
       (filter (fn [f] (and (.isFile f)
                            (some (fn [e] (.endsWith (.getName f) e)) exts))))))

(defn- offending-lines
  "Returns [path line-number line] for every import-ish line in file that
  references a forbidden Swing/AWT package."
  [file]
  (keep-indexed
   (fn [idx line]
     (when (and (re-find #"import" line)
                (re-find forbidden-import line))
       [(str file) (inc idx) (str/trim line)]))
   (str/split-lines (slurp file))))

(deftest ^:swing-free core-has-no-swing-imports
  (testing "core Clojure namespaces are Swing/AWT-free"
    (let [offenders (mapcat offending-lines
                            (source-files core-clojure-dirs [".clj" ".cljc"]))]
      (is (empty? offenders)
          (str "Swing/AWT import(s) leaked into core Clojure:\n"
               (str/join "\n" (map pr-str offenders))))))
  (testing "Java loaded by the core is Swing/AWT-free (java.awt.geom excepted)"
    (let [offenders (mapcat offending-lines
                            (source-files core-java-dirs [".java"]))]
      (is (empty? offenders)
          (str "Swing/AWT import(s) leaked into core-loaded Java:\n"
               (str/join "\n" (map pr-str offenders)))))))

;;;

nil
