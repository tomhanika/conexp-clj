;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.latex
  "Provides functionality to represent conexp-clj datastructures as latex code."
  (:use conexp.base
        [conexp.fca.contexts :only (objects attributes incidence)]
        conexp.fca.lattices
        conexp.fca.many-valued-contexts
        [conexp.layouts.base :only (positions connections nodes inf-irreducibles sup-irreducibles annotation valuations)]))

;;;

(defn tex-escape
  "Escapes all significant characters used by LaTeX."
  [string]
  (clojure.string/escape 
    (str string) 
    {\& "\\&"
     \% "\\%"
     \$ "\\$"
     \# "\\#"
     \_ "\\_"
     \{ "\\{"
     \} "\\}"
     \< "\\textless "
     \> "\\textgreater "
     \~ "\\textasciitilde "
     \^ "\\textasciicircum "
     \\ "\\textbackslash "}))

;;;

(defprotocol LaTeX
  "Implements conversion to latex code."
  (latex [this] [this choice] "Returns a string representation of this."))

;;; Default

(extend-type Object
  LaTeX
  (latex
   ([this]
      (.toString this))
   ([this choice]
      (.toString this))))

;;; Contexts

(extend-type conexp.fca.contexts.Context
  LaTeX
  (latex
   ([this]
      (latex this :plain))
   ([this choice]
      (case choice
        :plain (with-out-str
                 (println "$")
                 (println (str "\\begin{array}{l||*{" (count (attributes this)) "}{c|}}"))
                 (doseq [m (map tex-escape (attributes this))]
                   (print (str "& " m)))
                 (println "\\\\\\hline\\hline")
                 (doseq [g (objects this)]
                   (print (tex-escape (str g)))
                   (doseq [m (attributes this)]
                     (print (str "& " (if ((incidence this) [g m]) "\\times" "\\cdot"))))
                   (println "\\\\\\hline"))
                 (println (str "\\end{array}"))
                 (println "$"))
        :fca   (with-out-str
                 (println "\\begin{cxt}%")
                 (println "  \\cxtName{}%")
                 (doseq [m (attributes this)]
                   (if (>= 2 (count m))
                     (println (str "  \\att{" (tex-escape m) "}%"))
                     (println (str "  \\atr{" (tex-escape m) "}%"))))
                 (let [inz (incidence this)]
                   (doseq [g (objects this)]
                     (print "  \\obj{") 
                     (doseq [m (attributes this)]
                       (print (if (inz [g m]) "x" ".")))
                     (println (str "}{" (tex-escape g) "}"))))
                 (println "\\end{cxt}"))
        true   (illegal-argument 
                 "Unsupported latex format " choice " for contexts.")))))


;;; Layouts

(declare layout->tikz)
(declare layout->fca-style)

(extend-type conexp.layouts.base.Layout
  LaTeX
  (latex
   ([this]
      (latex this :tikz))
   ([this choice]
      (case choice
        :tikz (layout->tikz this)
        :fca-style (layout->fca-style this)
        true  (illegal-argument "Unsupported latex format " choice " for layouts.")))))

(defn- layout->fca-style [layout]
  (let [vertex-pos      (positions layout),
        sorted-vertices (sort #(let [[x_1 y_1] (vertex-pos %1),
                                     [x_2 y_2] (vertex-pos %2)]
                                 (or (< y_1 y_2)
                                     (and (= y_1 y_2)
                                          (< x_1 x_2))))
                              (nodes layout)),
        vertex-idx      (into {}
                              (map-indexed (fn [i v] [v i])
                                           sorted-vertices)),
        value-fn #(if (nil? ((valuations layout) %))
                    "" ((valuations layout) %))]
    (with-out-str
     (println "{\\unitlength 1mm")
      (println "\\tikzset{concept/.style={/tikz/semithick, /tikz/shape=circle, inner sep=1pt, outer sep=0pt, draw=black!80,")
      (println "                         fill=white, radius=1.5mm},%")
      (println "         relation/.style={/tikz/-,/tikz/thick,color=black!80,line width=1.5pt},")
      (println "         valuation/.style={color=red,label distance=3pt}")
      (println "}")
      (println "\\begin{tikzpicture}[scale=1]")
      (println "\\begin{diagram}")
      ;; concepts
      (doseq [v sorted-vertices]
        (let [idx (vertex-idx v)
              [x y] (vertex-pos v)]
          (println (str"\\Node[/tikz/concept](" idx ")("x", "y")"))))
      ;; relation
      (doseq [[v w] (connections layout)]
        (let [vidx (vertex-idx v)
              widx (vertex-idx w)]          
          (println (str "\\Edge[/tikz/relation](" vidx ")("widx")"))))
      ;; attribute labels
      (doseq [v sorted-vertices]
        (let [idx (vertex-idx v)
              ann (annotation layout)
              [u _] (map tex-escape (ann v))]
          (if-not (= "" u)
            (println (str "\\centerAttbox("idx"){" u "}")) ) ))
      ;; object labels
      (doseq [v sorted-vertices]
        (let [idx (vertex-idx v)
              ann (annotation layout)
              [_ l] (map tex-escape (ann v))]
          (if-not (= "" l)
            (println (str "\\centerObjbox("idx"){" l "}")) ) ))
      ;; valuations 
      (doseq [v sorted-vertices]
        (let [val (value-fn v)
              idx (vertex-idx v)]
          (if (not= "" val)
            (println (str"\\node[/tikz/valuation] [right of="idx"] {"val"};")))))
      (println "\\end{diagram}")
      (println "\\end{tikzpicture}}") ) ))

(defn- layout->tikz [layout]
  (let [vertex-pos      (positions layout),
        sorted-vertices (sort #(let [[x_1 y_1] (vertex-pos %1),
                                     [x_2 y_2] (vertex-pos %2)]
                                 (or (< y_1 y_2)
                                     (and (= y_1 y_2)
                                          (< x_1 x_2))))
                              (nodes layout)),
        vertex-idx      (into {}
                              (map-indexed (fn [i v] [v i])
                                           sorted-vertices)),
        value-fn #(if (nil? ((valuations layout) %))
                    "" ((valuations layout) %))]
    (with-out-str
      (println "\\colorlet{mivertexcolor}{blue}")
      (println "\\colorlet{jivertexcolor}{red}")
      (println "\\colorlet{vertexcolor}{mivertexcolor!50}")
      (println "\\colorlet{bordercolor}{black!80}")
      (println "\\colorlet{linecolor}{gray}")
      (println "% parameter corresponds to the used valuation function and can be addressed by #1")
      (println "\\tikzset{vertexbase/.style 2 args={semithick, shape=circle, inner sep=2pt, outer sep=0pt, draw=bordercolor},%")
      (println "  vertex/.style 2 args={vertexbase={#1}{}, fill=vertexcolor!45},%")
      (println "  mivertex/.style 2 args={vertexbase={#1}{}, fill=mivertexcolor!45},%")
      (println "  jivertex/.style 2 args={vertexbase={#1}{}, fill=jivertexcolor!45},%")
      (println "  divertex/.style 2 args={vertexbase={#1}{}, top color=mivertexcolor!45, bottom color=jivertexcolor!45},%")
      (println "  conn/.style={-, thick, color=linecolor}%")
      (println "}")
      (println "\\begin{tikzpicture}")
      (println "  \\begin{scope} %for scaling and the like")
      (println "    \\begin{scope} %draw vertices")
      (println "      \\foreach \\nodename/\\nodetype/\\param/\\xpos/\\ypos in {%")
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
                                       "/" (value-fn v)
                                       "/" x "/" y)))
                              sorted-vertices)]
        (doseq [x (interpose ",\n" vertex-lines)]
          (print x))
        (println))
      (println "      } \\node[\\nodetype={\\param}{}] (\\nodename) at (\\xpos, \\ypos) {};")
      (println "    \\end{scope}")
      (println "    \\begin{scope} %draw connections")
      (doseq [[v w] (connections layout)]
        (println (str "      \\path (" (vertex-idx v) ") edge[conn] (" (vertex-idx w) ");")))
      (println "    \\end{scope}")
      (println "    \\begin{scope} %add labels")
      (println "      \\foreach \\nodename/\\labelpos/\\labelopts/\\labelcontent in {%")
      (let [ann       (annotation layout),
            ann-lines (mapcat (fn [v]
                                (let [[u _] (map tex-escape (ann v)),
                                      lines (if-not (= "" u)
                                              (list (str "        " (vertex-idx v) "/above//{" u "}"))
                                              ())]
                                  lines))
                              sorted-vertices)
            ann-lines (concat ann-lines
                            (mapcat (fn [v]
                                      (let [[_ l] (map tex-escape (ann v)),
                                            val (value-fn v),
                                            lines (if-not (= "" l)
                                                    (list (str "        " (vertex-idx v) "/below//{" l "}"))
                                                    ())]
                                        lines))
                                    sorted-vertices))
            ann-lines (concat ann-lines
                            (mapcat (fn [v]
                                      (let [val (value-fn v),
                                            lines (if-not (= "" val)
                                                    (list (str "        " (vertex-idx v) "/right//{" val "}"))
                                                    ())]
                                        lines))
                                    sorted-vertices))]
        (doseq [x (interpose ",\n" ann-lines)]
          (print x))
        (println))
      (println "      } \\coordinate[label={[\\labelopts]\\labelpos:{\\labelcontent}}](c) at (\\nodename);")
      (println "    \\end{scope}")
      (println "  \\end{scope}")
      (println "\\end{tikzpicture}"))))

;;;

nil
