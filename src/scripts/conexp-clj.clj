;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(use 'clojure.tools.cli)

;;

(let [[options trailing doc] (cli *command-line-args*
                                  ["--gui"  "Start the graphical user interface"]
                                  ["--load" "Load a given script"]
                                  ["--help" "This help"])]
  ;;
  (when (contains? options :help)
    (println doc)
    (System/exit 0))
  ;;
  (when (contains? options :gui)
    (clojure.main/repl :init #(do
                                (use 'conexp.main)
                                (use 'conexp.contrib.gui)
                                (@(ns-resolve 'conexp.contrib.gui 'gui)
                                 :default-close-operation :exit))))
  ;;
  (when (contains? options :load)
    (use 'conexp.main)
    (when (not (options :load))
      (println "Error: --load requires a file to load")
      (println doc)
      (System/exit 1))
    (load-file (options :load)))
  ;;
  (when-not (or (options :gui)
                (options :load))
    (clojure.main/repl :init #(do (use 'conexp.main) (use 'clojure.repl)))))

;;

nil
