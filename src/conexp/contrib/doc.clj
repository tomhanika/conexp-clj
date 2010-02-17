(ns conexp.contrib.doc
  (:use [conexp :only (*conexp-namespaces*)]
	[clojure.contrib.io :only (with-out-writer)]))


(defn public-api
  "Returns a map of public functions of namespaces to their
  corresponding documentation."
  [ns]
  (let [ns (find-ns ns)]
    (map (fn [[function var]]
	   [function (str (:arglists (meta var))
			  "\n\n"
			  (:doc (meta var)))])
	 (filter (fn [[f var]]
		   (and (= (:ns (meta var)) ns)
			(not (Character/isUpperCase #^Character (first (str f))))))
		 (ns-map ns)))))

(defn tex-escape
  "Escapes special characters for \\TeX."
  [string]
  (let [#^StringBuilder sb (StringBuilder.)
	string (str string)]
    (doseq [c string]
      (cond
	(#{\_,\&,\%,\#} c)
	(.append sb (str \\ c))
	(#{\^} c)
	(.append sb "\\verb+^+")
	:else
	(.append sb c)))
    (str sb)))

(defn public-api-as-tex
  "Returns output suitable for \\TeX describing the public apis of the
  given namespaces with their corresponding documentations."
  [& namespaces]
  (with-out-str
    (doseq [ns namespaces]
      (let [api (sort (public-api ns))]
	(println (str "\\section{" (tex-escape ns) "}"))
	(println "\\begin{description}")
	(doseq [[fn doc] api]
	  (println (str "  \\item[" (tex-escape fn) "]"))
	  (println (tex-escape doc))
	  (println))
	(println "\\end{description}"))
      (println))))

(defn public-api-to-file
  "Prints out public api to file for external usage."
  [file & namespaces]
  (with-out-writer file
    (print (apply public-api-as-tex namespaces))))

(defn conexp-api
  "Prints conexp-clj api to file."
  [file]
  (apply public-api-to-file file *conexp-namespaces*))

(defn conexp-fns-needing-doc
  "Returns function in public conexp-clj api not having documentation."
  []
  (for [ns *conexp-namespaces*
	[f var] (public-api ns)
	:when (not (:doc (meta var)))]
    (symbol (str ns) (str f))))

nil
