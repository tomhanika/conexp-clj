(ns helper)

;;;
;;; This file contains stuff which I think are great
;;;

(defmacro one-by-one
  "Calls a function on each element of a collection.
   Parameters:
   coll    _collection of arguments
   f       _function body that takes one parameter
   " 
  [coll & f]
  `(loop [args# ~coll]
     (if (empty? args#) nil (do (~@f (first args#))
                                (recur (rest args#))))))
   
(defmacro lambda
  "Takes the free argument as first parameter and the function value as
    following parameters as function body, i.e.


     ( lambda (x y) (do (print x) (* x y)) ) 

    will produce a function that takes 2 parameters, prints the first one
    and returns the product of them.
   " 
  [parameter & body]
  `(fn [~@parameter] ~@body ))

(defmacro rho
  "Takes an block that defines a function by omitting its last parameter
    and turns it into a normal function that takes one parameter"
  [ & body ]
  `(fn [x#] (~@body x#)))

(defmacro .*.
  "Takes two functions and returns the covariant function concatenation
    of them"
  [ f g ] 
  `(lambda (x#) (~g (~f x#))))

(defmacro *..
  "Takes some functions and returns the covariant function concatenation
    of them (n-ary .*.-Operator)"
  [ f g & more]
  (if (empty? more) `(.*. ~f ~g)
      `(f*g ~f (chain ~g ~@more ))))

(defn do-map-tree
  "Maps a named list tree using the given functions to a new tree structure,
   utilizing side effects heavily. (This is Java inspired madness....)
   (A list tree is a tree which nodes are of the form (name child1 child2)
    (where the childs are optional) such that the whole tree is represented
    by its root node.)
 
   Parameters:
     mk        _function that maps a node name to the new node data structure
     add       _function that takes as arguments the root and the child node
                (as side effect!!)
     tree      _the tree that is mapped"
  [mk add tree]
  (let [root (mk (first tree))
        childs (rest tree)]
    (do
      (one-by-one childs (lambda (x) (add root (do-map-tree mk add x)))) 
      root )))
