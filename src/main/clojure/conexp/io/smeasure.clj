;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.smeasure
  "Provides functionality to represent conexp-clj smeasures as latex code."
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.smeasure
        conexp.io.latex))

;;; Smeasure

(declare smeasure->tikz)
(declare smeasure->lattice)

(extend-type conexp.fca.smeasure.Smeasure
  LaTeX
  (latex
   ([this]
     (latex this :tikz))
   ([this choice]
     (case choice
       :tikz    (smeasure->lattice this)
       :lattice (smeasure->tikz this)
       true     (illegal-argument 
                  "Unsupported latex format " choice " for contexts.")))))

(defn- smeasure->tikz
  [smeasure]
  (let [ctx     (context smeasure)
        scale   (scale smeasure)
        mapping (measure smeasure)
        ctx-obj (group-by #(mapping %) (objects ctx))
        sca-obj (objects scale)
        sca-obj (loop [order (vec (difference (objects scale) 
                                              (keys ctx-obj)))
                       next  (keys ctx-obj)
                       place (if (> (count (objects scale))
                                    (count (objects ctx)))
                                 (int (/ (- (count (objects scale))
                                         (count (objects ctx))) 
                                      2))
                                 0)]
                  (if (empty? next)
                      order
                      (let [element   (first next)
                            pos       (+ place 
                                         (int 
                                           (/ (- (count (get ctx-obj element)) 
                                                 1)
                                              2)))
                            new-order (vec (concat (take pos order)
                                                   (list element)
                                                   (drop pos order)))
                            new-place (+ place (count (get ctx-obj element)))]
                        (recur new-order (drop 1 next) new-place))))]
    (with-out-str
      (println "%necessary tikz libraries")
      (println "%\\usetikzlibrary{tikzmark,arrows,positioning}")
      (println "\\begin{figure}")
      (println "  \\centering")
      ;;
      ;; original context 
      (println "  \\begin{minipage}[c]{.5\\textwidth}")
      (println "    \\centering")
      (println "    \\begin{cxt}%")
      (println "      \\cxtName{}%")
      (doseq [m (attributes ctx)]
        (if (>= 2 (count (str m)))
          (println (str "     \\att{\\tikzmarknode{ca" (tex-escape m) 
                        "}{" (tex-escape m) "}}%"))
          (println (str "     \\atr{\\tikzmarknode{ca" (tex-escape m) 
                        "}{" (tex-escape m) "}}%"))))
      (let [inz (incidence ctx)]
        (doall
          (for [[k objs] ctx-obj]
            (doseq [g objs]
              (print "      \\obj{") 
              (doseq [m (attributes ctx)]
                (print (if (inz [g m]) "x" ".")))
              (println (str "}{" (tex-escape g) "}"))))))
      (println "    \\end{cxt}")
      (println "  \\end{minipage}%")
      ;;
      ;; scale context 
      (println "  \\begin{minipage}[c]{.5\\textwidth}")
      (println "    \\centering")
      (println "    \\begin{cxt}%")
      (println "      \\cxtName{}%")
      (doseq [m (attributes scale)]
        (if (>= 2 (count (str m)))
          (println (str "     \\att{" (tex-escape m) "}%"))
          (println (str "     \\atr{" (tex-escape m) "}%"))))
      (doall (let [inz (incidence scale)]
               (doseq [g sca-obj]
                 (print "      \\obj{") 
                 (doseq [m (attributes scale)]
                   (print (if (inz [g m]) "x" ".")))
                 (println (str "}{\\tikzmarknode{so" (tex-escape g) 
                               "}{" (tex-escape g) "}}")))))
      (println "    \\end{cxt}")
      (println "  \\end{minipage}%")
      (println "\\end{figure}")
      ;;
      ;; tikz overlay
      (println "\\begin{tikzpicture}[overlay,remember picture]")
      (let [ctx-obj-list (flatten (map second (vec ctx-obj)))
            anchor       (last (butlast (attributes ctx)))]
        (doall (if (>= 2 (count (str anchor)))
                   (do 
                     (println (str "  \\node[right = 0.45cm of ca" 
                                   (tex-escape anchor)
                                   "] (za) {};"))
                     (println (str "  \\node[below = 0.25cm of za] (co" 
                                   (first ctx-obj-list)  ") {};")))
                   (println (str "  \\node[below = 0.25cm of ca" 
                             (tex-escape anchor)
                             "] (co" (first ctx-obj-list)  ") {};"))))
        (doall
          (map
            (fn[a b] 
              (println (str "  \\node[below = 0.16cm of co" 
                            (tex-escape a)
                            "] (co" (tex-escape b)  ") {};")))
            ctx-obj-list
            (drop 1 ctx-obj-list)))
        (doall
          (map 
            #(println (str "  \\node[left = 0cm of so" % "](lso" % "){};"))
            (objects scale)))
        (doall
          (for [obj ctx-obj-list]
            (println (str "  \\draw[->, >=stealth] (co"
                          (tex-escape obj) ") -- (lso" 
                          (tex-escape (mapping obj)) ");")))))
      (println "\\end{tikzpicture}"))))

;;; Smeasure + Lattice

;;;

nil
