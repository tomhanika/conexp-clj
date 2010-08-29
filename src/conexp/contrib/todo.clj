;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.todo
  (:use conexp.main)
  (:use [clojure.string :only (split)]
	[clojure.pprint :only (pprint)]))

(ns-doc "Provides information on all tasks planned for conexp-clj.")

;;;

(defvar- *conexp-todo-list*
  {"conexp" ["Add better error reporting and constraint checking.",
	     "Add docstrings for namespaces.",
	     "Add docstrings for all functions.",
	     "Extend LaTeX-Documentation.",

	     {"layout" {"force"    ["Implement force layout with derivations?"],
			"zschalig" ["Implement initial positioning as described by C. Zschalig."]},
	      "fca"    {"rules"    ["Think about and write some code."],
			"association-rules" ["More?"],
			"exploration" ["Add background knowledge (clauses)."],
			"contexts" ["Better implementation for context-for-clop."]},
	      "gui"    {"plugins"  {"contexteditor" ["Design and implement."],
				    "latticeeditor" ["Design and implement."],
				    "codeeditor"    ["Design and implement."],
				    "browser"       ["Design and implement graphical plugin browser."]}},
	      "contrib" {"fuzzy"   ["Design and implement something for FuzzyFCA (Cynthia)."],
			 "rough"   ["Design and implement something for RoughSets (Christian)."],
			 "algorithms" {"concepts"
				       ["Check In-Close and add fast Calculations for Luxenburger Bases.",
					"Add preconditioned NextClosure.",
					"Implement ParallelNextClosure and Krajca-Outrata-Vychodil."]}},
	      "tests"  ["Add tests for everything.",
			{"fca"  {"contexts" ["Finish tests."],
				 "implications" ["Finish tests."]}}]}]})

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
		     (let [todo-for-key ((last todo-list) (first keys))]
		       (if todo-for-key
			 (assoc todo-list
			   (dec (count todo-list))
			   {(first keys) todo-for-key})
			 (subvec todo-list 0 (dec (count todo-list))))),

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
     (let [ns-as-seq (split (str ns) #"\.")
	   tasks (access-todo-list ns-as-seq)]
       (print (format-output tasks)))))

;;;

nil
