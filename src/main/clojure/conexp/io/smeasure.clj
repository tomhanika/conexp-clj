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
        conexp.fca.lattices
        conexp.fca.smeasure
        [conexp.layouts.base :refer [positions nodes inf-irreducibles 
                                     sup-irreducibles connections annotation]]
        conexp.layouts.dim-draw
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
       :tikz    (smeasure->tikz this)
       :lattice (smeasure->lattice this)
       true     (illegal-argument 
                  "Unsupported latex format " choice " for contexts.")))))

(defn- smeasure->tikz
  [smeasure]
  (let [ctx     (context smeasure)
        scale   (scale smeasure)
        mapping (measure smeasure)
        ctx-obj (group-by #(mapping %) (objects ctx))
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

(defn- smeasure->lattice
  [smeasure]
  (let [ctx     (context smeasure)
        scale   (scale smeasure)
        mapping (measure smeasure)
        ctx-obj (group-by #(mapping %) (objects ctx))]
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
      (let [layout          (dim-draw-layout (concept-lattice scale)),
            vertex-pos      (positions layout),
            sorted-vertices (sort #(let [[x_1 y_1] (vertex-pos %1),
                                         [x_2 y_2] (vertex-pos %2)]
                                     (or (< y_1 y_2)
                                         (and (= y_1 y_2)
                                              (< x_1 x_2))))
                                  (nodes layout)),
            vertex-idx      (into {}
                                  (map-indexed (fn [i v] [v i])
                                               sorted-vertices))]
        (println "    \\colorlet{mivertexcolor}{blue}")
        (println "    \\colorlet{jivertexcolor}{red}")
        (println "    \\colorlet{vertexcolor}{mivertexcolor!50}")
        (println "    \\colorlet{bordercolor}{black!80}")
        (println "    \\colorlet{linecolor}{gray}")
        (println (str "    \\tikzset{vertexbase/.style={semithick, "
                      "shape=circle, inner sep=2pt, outer sep=0pt, "
                      "draw=bordercolor},%"))
        (println "      vertex/.style={vertexbase, fill=vertexcolor!45},%")
        (println "      mivertex/.style={vertexbase, fill=mivertexcolor!45},%")
        (println "      jivertex/.style={vertexbase, fill=jivertexcolor!45},%")
        (println (str "      divertex/.style={vertexbase, "
                      "top color=mivertexcolor!45, "
                      "bottom color=jivertexcolor!45},%"))
        (println "      conn/.style={-, thick, color=linecolor}%")
        (println "    }")
        (println "    \\begin{tikzpicture}")
        (println "      \\begin{scope} %for scaling and the like")
        (println "        \\begin{scope} %draw vertices")
        (println (str "          \\foreach \\nodename/\\nodetype/\\xpos/\\ypos"
                      " in {%"))
        (let [infs         (set (inf-irreducibles layout)),
              sups         (set (sup-irreducibles layout)),
              insu         (intersection infs sups),
              vertex-lines (map (fn [v]
                                  (let [i     (vertex-idx v),
                                        [x y] (vertex-pos v)]
                                    (str "        " i "/"
                                         (cond
                                          (contains? insu v)  "divertex"
                                          (contains? sups v)  "jivertex"
                                          (contains? infs v)  "mivertex"
                                          :else               "vertex")
                                         "/" x "/" y)))
                                sorted-vertices)]
          (doseq [x (interpose ",\n" vertex-lines)]
            (print x))
          (println))
        (println (str "          } \\node[\\nodetype] (\\nodename) at "
                      "(\\xpos, \\ypos) {};"))
        (println "        \\end{scope}")
        (println "        \\begin{scope} %draw connections")
        (doseq [[v w] (connections layout)]
          (println (str "    \\path (" (vertex-idx v) ") edge[conn] (" 
                        (vertex-idx w) ");")))
        (println "        \\end{scope}")
        (println "        \\begin{scope} %add labels")
        (println (str "          \\foreach "
                      "\\nodename/\\labelpos/\\labelopts/\\labelcontent "
                      "in {%"))
        (let [ann       (annotation layout),
              ann-lines (mapcat (fn [v]
                                  (let [[u l] (map tex-escape (ann v)),
                                        lines (if-not (= "" u)
                                                (list (str "            " 
                                                          (vertex-idx v) 
                                                          "/above//{" u "}"))
                                                ()),
                                        lines (if-not (= "" l)
                                                (conj 
                                                  lines
                                                  (str "            " 
                                                       (vertex-idx v) 
                                                       "/below//{" 
                                                       (apply str
                                                       (map 
                                                         #(str "\\tikzmarknode{n" % 
                                                           "}{}")
                                                         (clojure.string/split 
                                                           l 
                                                           #", ")))
                                                       l "}"))
                                                lines)]
                                    lines))
                                sorted-vertices)]
          (doseq [x (interpose ",\n" ann-lines)]
            (print x))
          (println))
        (println (str "          } \\coordinate[label={[\\labelopts]\\labelpos"
                      ":{\\labelcontent}}](c) at (\\nodename);"))
        (println "        \\end{scope}")
        (println "      \\end{scope}")
        (println "    \\end{tikzpicture}"))
      (println "  \\end{minipage}%")
      (println "\\end{figure}")
      ;;
      ;; tikz overlay
      (println "\\begin{tikzpicture}[overlay,remember picture]")
      (let [ctx-obj-list (flatten (map second (vec ctx-obj)))
            anchor       (last (butlast (attributes ctx)))]
        (doall (if (>= 2 (count (str anchor)))
                   (do 
v                     (println (str "  \\node[right = 0.45cm of ca" 
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
            #(println (str "  \\node[above = 0.2cm of n" (tex-escape %) 
                           "] (an" % ") {};"))
            (objects scale)))
        (doall
          (for [obj ctx-obj-list]
            (println (str "  \\draw[->, >=stealth] (co"
                          (tex-escape obj) ") -- (an" 
                          (tex-escape (mapping obj)) ");")))))
      (println "\\end{tikzpicture}"))))

(defn write-smeasure
  "This method dumps the tikz export for smeasures in a file."
  [file smeasure choice]
  (spit file (latex smeasure choice)))

;;;

nil
