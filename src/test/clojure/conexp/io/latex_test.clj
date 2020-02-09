;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.latex-test
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.lattices
        conexp.layouts.base
        conexp.io.latex)
  (:use clojure.test))

;;;

(deftest test-tex-escape
  (is (= (tex-escape "&%$#_{}~^\\<>")
         (str 
           "\\&\\%\\$\\#\\_\\{\\}\\textasciitilde "
           "\\textasciicircum \\textbackslash "
           "\\textless \\textgreater "))))

(deftest test-context-special-character
  (let [ctx (make-context #{"&" "%" "$" "#" "_" "<"} 
                          #{"{" "}" "~" "^" "\\" ">"} 
                          #{["&" "}"]["$" "\\"]})]
    (is (= (latex ctx :plain)
           (slurp "testing-data/latex/plain-example.tex")))))

(deftest test-layout->tikz-special-character
  (let [lat (make-lattice #{1 2 3 4}
                          #{[1 2][1 3][2 4][3 4][1 4][1 1][2 2][3 3][4 4]})
        pos (hash-map 1 [0 0] 2 [-1 1] 3 [1 1] 4 [0 2])
        new-pos (hash-map 1 [0 0] 2 [-2 1] 3 [1 1] 4 [0 2])
        edge #{[1 2][1 3][2 4][3 4]}
        up (hash-map 1 ["^a&" nil] 2 ["<b" nil] 
										 3 ["_c~" nil] 4 [">d" nil])
        lo (hash-map 1 ["$e%" nil] 2 ["#f" nil] 
                     3 ["{g}" nil] 4 ["\\h" nil])
      	layout (make-layout lat pos edge up lo)]
    (is (= (latex layout :tikz)
           (slurp "testing-data/latex/tikz-example.tex")))))

(deftest test-fca-context-special-character
  (let [ctx (make-context #{"&" "%" "$" "#" "_" "<"} 
                          #{"{" "}" "~sideways" "^" "\\" ">"} 
                          #{["&" "}"]["$" "\\"]})]
    (is (= (latex ctx :fca)
           (slurp "testing-data/latex/fca-example.tex")))))
;;;

nil
