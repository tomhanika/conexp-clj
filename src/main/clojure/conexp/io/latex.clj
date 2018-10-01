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
        [conexp.layouts.base :only (positions connections nodes inf-irreducibles sup-irreducibles annotation)]))

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
                 (println (str "\\begin{array}{l||*{" (count (attributes this)) "}{c|}}"))
                 (doseq [m (attributes this)]
                   (print (str "& \\text{" m "}")))
                 (println "\\\\\\hline\\hline")
                 (doseq [g (objects this)]
                   (print (str g))
                   (doseq [m (attributes this)]
                     (print (str "& " (if ((incidence this) [g m]) "\\times" "\\cdot"))))
                   (println "\\\\\\hline"))
                 (println (str "\\end{array}")))
        true   (illegal-argument "Unsupported latex format " choice " for contexts.")))))


;;; Layouts

(declare layout->tikz)

(extend-type conexp.layouts.base.Layout
  LaTeX
  (latex
   ([this]
      (latex this :tikz))
   ([this choice]
      (case choice
        :tikz (layout->tikz this)
        true  (illegal-argument "Unsupported latex format " choice " for layouts.")))))

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
                                           sorted-vertices))]
    (with-out-str
      (println "\\colorlet{mivertexcolor}{blue}")
      (println "\\colorlet{jivertexcolor}{red}")
      (println "\\colorlet{vertexcolor}{mivertexcolor!50}")
      (println "\\colorlet{bordercolor}{black!80}")
      (println "\\colorlet{linecolor}{gray}")
      (println "\\tikzset{vertexbase/.style={semithick, shape=circle, inner sep=2pt, outer sep=0pt, draw=bordercolor},%")
      (println "  vertex/.style={vertexbase, fill=vertexcolor!45},%")
      (println "  mivertex/.style={vertexbase, fill=mivertexcolor!45},%")
      (println "  jivertex/.style={vertexbase, fill=jivertexcolor!45},%")
      (println "  divertex/.style={vertexbase, top color=mivertexcolor!45, bottom color=jivertexcolor!45},%")
      (println "  conn/.style={-, thick, color=linecolor}%")
      (println "}")
      (println "\\begin{tikzpicture}")
      (println "  \\begin{scope} %for scaling and the like")
      (println "    \\begin{scope} %draw vertices")
      (println "      \\foreach \\nodename/\\nodetype/\\xpos/\\ypos in {%")
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
      (println "      } \\node[\\nodetype] (\\nodename) at (\\xpos, \\ypos) {};")
      (println "    \\end{scope}")
      (println "    \\begin{scope} %draw connections")
      (doseq [[v w] (connections layout)]
        (println (str "      \\path (" (vertex-idx v) ") edge[conn] (" (vertex-idx w) ");")))
      (println "    \\end{scope}")
      (println "    \\begin{scope} %add labels")
      (println "      \\foreach \\nodename/\\labelpos/\\labelopts/\\labelcontent in {%")
      (let [ann       (annotation layout),
            ann-lines (mapcat (fn [v]
                                (let [[u l] (ann v),
                                      lines (if-not (= "" u)
                                              (list (str "        " (vertex-idx v) "/above//{" u "}"))
                                              ()),
                                      lines (if-not (= "" l)
                                              (conj lines
                                                    (str "        " (vertex-idx v) "/below//{" l "}"))
                                              lines)]
                                  lines))
                              sorted-vertices)]
        (doseq [x (interpose ",\n" ann-lines)]
          (print x))
        (println))
      (println "      } \\coordinate[label={[\\labelopts]\\labelpos:{\\labelcontent}}](c) at (\\nodename);")
      (println "    \\end{scope}")
      (println "  \\end{scope}")
      (println "\\end{tikzpicture}"))))

;;;

nil
