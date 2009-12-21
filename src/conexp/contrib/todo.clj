(ns #^{:doc "Provides information on all tasks planned for conexp-clj."}
  conexp.contrib.todo
  (:use clojure.contrib.def)
  (:use [clojure.contrib.str-utils :only (re-split)])
  (:use [clojure.contrib.pprint :only (pprint)]))

(defvar- *conexp-todo-list*
  { "conexp" { "layout" { "util"     [ "Implement function for inf-irr-addivite layout." ],
			  "force"    [ "Implement force layout as described by C. Zschalig." ],
			  "zschalig" [ "Implement initial positioning and force distribution layout as described by C. Zschalig."] },

	       "fca"    { "rules"    [ "Think about and write some code." ],
			  "retracts" [ "Implement first thoughts of F. Kästner." ],
			  "association-rules" [ "More?" ],
			  "exploration" [ "Add background knowledge (implications or clauses)." ],
			  "contexts" [ "Better implementation for context-for-clop." ] },

	       "graphics" { "draw"   [ "Implement full lattice editor" ],
			    "base"   [ "Get current lattice layout from a GScene." ] },

	       "gui"    { "plugins"  [ "Design and implement.",
				       { "contexteditor" [ "Design and implement." ],
					 "latticeeditor" [ "Design and implement." ],
					 "codeeditor"    [ "Design and implement." ] } ] },

	       "contrib" { "fuzzy"   [ "Design and implement something for FuzzyFCA (Cynthia)." ],
			   "rough"   [ "Design and implement something for RoughSets (Christian)." ],
			   "algorithms" { "bitwise" [ "Add In-Close and fast Calculations for Luxenburger Bases.",
						      "Add preconditioned NextClosure." ],
					  "parallel" [ "Implement ParallelNextClosure and Krajca-Outrata-Vychodil." ] } },

	       "tests"  ["Add tests for everything.",
			 { "util"     [ "Add more tests." ],
			   "base"     [ "Check whether everything has been tested." ],
			   "fca"      { "contexts" [ "Finish tests." ],
					"implications" [ "Finish tests." ] }, } ] },
    "doc"    [ "Add docstrings for namespaces.",
	       "Add docstrings for all functions.",
	       "Extend LaTeX-Documentation." ] })

(defn- access-todo-list
  "Returns all tasks remaining for given ns, given as sequence of strings."
  [ns-as-seq]
  (if (empty? ns-as-seq)
    *conexp-todo-list*
    (let [runner (fn runner [todo-list keys]
		   (cond
		     (or (not todo-list)
			 (empty? keys))
		     todo-list,
		     (vector? todo-list)
		     (assoc todo-list
		       (dec (count todo-list))
		       {(first keys) (runner ((last todo-list)
					      (first keys))
					     (rest keys))}),
		     (map? todo-list)
		     {(first keys) (runner (todo-list (first keys))
					   (rest keys))}))]
      (runner *conexp-todo-list* ns-as-seq))))

(defn- format-output
  "Formats structure given as human-readable output.

  This implementation is ugly."
  ([object]
     (format-output object 0))
  ([object indent]
     (cond
       (string? object)
       (str (apply str (repeat indent \space)) object "\n"),
       (vector? object)
       (apply str
	      (map #(format-output % indent) object)),
       (map? object)
       (apply str
	      (for [key (keys object)]
		(apply str
		       (apply str (repeat indent \space))
		       key
		       ":\n"
		       (format-output (object key) (+ indent 2))))),
       :else
       nil)))

(defn what-is-left?
  "Returns, given a namespace (as Symbol, String or Namespace), the
  task to be done there."
  ([]
     (print (format-output *conexp-todo-list*)))
  ([ns]
     (let [ns-as-seq (re-split #"\." (str ns))
	   tasks (access-todo-list ns-as-seq)]
       (print (format-output tasks)))))

nil
