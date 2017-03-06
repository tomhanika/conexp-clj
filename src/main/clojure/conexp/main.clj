;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.main
  "Main namespace for conexp-clj."
  (:require [clojure.tools.cli :as cli])
  (:require [reply.main :as reply])
  (:gen-class))

;;;

(def conexp-clj-namespaces
  "Standard namespaces of conexp-clj."
  '[conexp.base
    conexp.fca.contexts
    conexp.fca.many-valued-contexts
    conexp.fca.implications
    conexp.fca.exploration
    conexp.fca.lattices
    conexp.fca.misc
    conexp.io.latex
    conexp.io.contexts
    conexp.io.lattices
    conexp.io.layouts
    conexp.io.many-valued-contexts
    conexp.layouts])

(apply use conexp-clj-namespaces)

;;;

(def conexp-clj-options
  [["-g" "--gui" "Start the graphical user interface"]
   ["-l" "--load FILE" "Load a given file and exit"]
   ["-h" "--help" "This help"]])

(defn -main [& args]
  (let [{:keys [options summary errors]}
        (cli/parse-opts args conexp-clj-options)]

    (when errors
      (doseq [error errors]
        (println error))
      (System/exit 1))

    (cond
      ;;
      (contains? options :help)
      (println summary)
      ;;
      (contains? options :gui)
      (reply/launch
       {:custom-eval '(do
                        (in-ns 'conexp.main)
                        (use 'clojure.repl)
                        (require '[conexp.contrib.gui.repl-utils :as gui])
                        (require 'conexp.contrib.gui)
                        (alter-var-root
                         (var gui/*main-frame*)
                         (fn [_]
                           (conexp.contrib.gui/gui
                            :default-close-operation :exit))))
        :custom-help ""})
      ;;
      (contains? options :load)
      (do
        (in-ns 'conexp.main)
        (load-file (options :load)))
      ;;
      true
      (reply/launch {:custom-eval '(do (in-ns 'conexp.main)
                                       (use 'clojure.repl))
                     :custom-help ""})))

  (System/exit 0))
