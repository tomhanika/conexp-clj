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
			  "exploration" [ "Add background knowledge (implications or clauses)." ] },

	       "graphics" { "draw"   [ "Implement full lattice editor" ],
			    "base"   [ "Get current lattice layout from a GScene." ] },

	       "gui"    { "plugins"  [ "Design and implement.",
				       { "contexteditor" [ "Design and implement." ],
					 "latticeeditor" [ "Design and implement." ],
					 "codeeditor"    [ "Design and implement." ] } ] },

	       "contrib" { "fuzzy"   [ "Design and implement something for FuzzyFCA (Cynthia)." ],
			   "rough"   [ "Design and implement something for RoughSets (Christian)." ],
			   "algorithms" { "bitwise" [ "Add In-Close and fast Calculations for Luxenburger Bases." ],
					  "parallel" [ "Implement ParallelNextClosure and Krajca-Outrata-Vychodil." ] } },

	       "tests"  ["Add tests for everything.",
			 { "util"     [ "Add more tests." ],
			   "base"     [ "Check whether everything has been tested." ],
			   "fca"      { "contexts" [ "Finish tests." ],
					"implications " [ "Finish tests." ] }, } ] },
    "doc"    [ "Add docstrings for namespaces.",
	       "Add docstrings for all functions.",
	       "Extend LaTeX-Documentation." ] })

(defn- access-todo-list
  "Returns all tasks remaining for given ns, given as sequence of strings."
  [ns-as-seq]
  (if (empty? ns-as-seq)
    nil
    (let [runner (fn runner [todo-list keys]
		   (if (or (not todo-list)
			   (empty? keys))
		     todo-list
		     (recur ((if (vector? todo-list)
			       (last todo-list)
			       todo-list)
			     (first keys))
			    (rest keys))))]
      (runner *conexp-todo-list* ns-as-seq))))

(defn what-is-left?
  "Returns, given a namespace (as Symbol, String or Namespace), the
  task to be done there."
  ([]
     (what-is-left? "conexp"))
  ([ns]
     (let [ns-as-seq (re-split #"\." (str ns))
	   tasks (access-todo-list ns-as-seq)]
       (pprint tasks))))

nil
