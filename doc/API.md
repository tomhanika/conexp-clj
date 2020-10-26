
# API Documentation

 - Namespace [`conexp.base`](#conexp.base)

 - Namespace [`conexp.fca.contexts`](#conexp.fca.contexts)

 - Namespace [`conexp.fca.many-valued-contexts`](#conexp.fca.many-valued-contexts)

 - Namespace [`conexp.fca.implications`](#conexp.fca.implications)

 - Namespace [`conexp.fca.exploration`](#conexp.fca.exploration)

 - Namespace [`conexp.fca.lattices`](#conexp.fca.lattices)

 - Namespace [`conexp.fca.more`](#conexp.fca.more)

 - Namespace [`conexp.io.latex`](#conexp.io.latex)

 - Namespace [`conexp.io.contexts`](#conexp.io.contexts)

 - Namespace [`conexp.io.lattices`](#conexp.io.lattices)

 - Namespace [`conexp.io.layouts`](#conexp.io.layouts)

 - Namespace [`conexp.io.many-valued-contexts`](#conexp.io.many-valued-contexts)

 - Namespace [`conexp.layouts`](#conexp.layouts)

## <a name="conexp.base"> Public API of conexp.base 


  Basic definitions for conexp-clj.

### Available Functions 

- [`<=>`](#<=>)
- [`=>`](#=>)
- [`abs`](#abs)
- [`all-closed-sets`](#all-closed-sets)
- [`all-closed-sets-in-family`](#all-closed-sets-in-family)
- [`ask`](#ask)
- [`ceil`](#ceil)
- [`clojure-coll`](#clojure-coll)
- [`clojure-fn`](#clojure-fn)
- [`clojure-map`](#clojure-map)
- [`clojure-seq`](#clojure-seq)
- [`clojure-set`](#clojure-set)
- [`clojure-type`](#clojure-type)
- [`clojure-vec`](#clojure-vec)
- [`compare-order`](#compare-order)
- [`conexp-version`](#conexp-version)
- [`conexp-version-map`](#conexp-version-map)
- [`covers?`](#covers?)
- [`cross-product`](#cross-product)
- [`def-`](#def-)
- [`defalias`](#defalias)
- [`defmacro-`](#defmacro-)
- [`die-with-error`](#die-with-error)
- [`difference`](#difference)
- [`disjoint-union`](#disjoint-union)
- [`distinct-by-key`](#distinct-by-key)
- [`dopar`](#dopar)
- [`ensure-length`](#ensure-length)
- [`ensure-seq`](#ensure-seq)
- [`exact-integer-sqrt`](#exact-integer-sqrt)
- [`exists`](#exists)
- [`expand-bindings`](#expand-bindings)
- [`expt`](#expt)
- [`first-non-nil`](#first-non-nil)
- [`first-position-if`](#first-position-if)
- [`floor`](#floor)
- [`forall`](#forall)
- [`gcd`](#gcd)
- [`generic-equals`](#generic-equals)
- [`graph-of-function?`](#graph-of-function?)
- [`has-version?`](#has-version?)
- [`hash-combine-hash`](#hash-combine-hash)
- [`illegal-argument`](#illegal-argument)
- [`illegal-state`](#illegal-state)
- [`immigrate`](#immigrate)
- [`improve-basic-order`](#improve-basic-order)
- [`index`](#index)
- [`inits`](#inits)
- [`integer-length`](#integer-length)
- [`internal-version-string`](#internal-version-string)
- [`intersection`](#intersection)
- [`intersection-set?`](#intersection-set?)
- [`join`](#join)
- [`lcm`](#lcm)
- [`lectic-<`](#lectic-<)
- [`lectic-<_i`](#lectic-<_i)
- [`map-by-fn`](#map-by-fn)
- [`map-invert`](#map-invert)
- [`memo-fn`](#memo-fn)
- [`minimal-generating-subsets`](#minimal-generating-subsets)
- [`minimal-hypergraph-transversals`](#minimal-hypergraph-transversals)
- [`minimum-set-covers`](#minimum-set-covers)
- [`next-closed-set`](#next-closed-set)
- [`next-closed-set-in-family`](#next-closed-set-in-family)
- [`not-yet-implemented`](#not-yet-implemented)
- [`now`](#now)
- [`order-by`](#order-by)
- [`parallel-closures`](#parallel-closures)
- [`partial-max`](#partial-max)
- [`partial-min`](#partial-min)
- [`prod`](#prod)
- [`project`](#project)
- [`proper-subset?`](#proper-subset?)
- [`proper-superset?`](#proper-superset?)
- [`quit`](#quit)
- [`reduce!`](#reduce!)
- [`redundant?`](#redundant?)
- [`reflexive-transitive-closure`](#reflexive-transitive-closure)
- [`rename`](#rename)
- [`rename-keys`](#rename-keys)
- [`round`](#round)
- [`select`](#select)
- [`set-of`](#set-of)
- [`set-of-range`](#set-of-range)
- [`singleton?`](#singleton?)
- [`sort-by-first`](#sort-by-first)
- [`sort-by-second`](#sort-by-second)
- [`split-at-first`](#split-at-first)
- [`split-at-last`](#split-at-last)
- [`sqrt`](#sqrt)
- [`subset?`](#subset?)
- [`subsets`](#subsets)
- [`sum`](#sum)
- [`superset?`](#superset?)
- [`tails`](#tails)
- [`to-set`](#to-set)
- [`topological-sort`](#topological-sort)
- [`transitive-closure`](#transitive-closure)
- [`transitive-reduction`](#transitive-reduction)
- [`union`](#union)
- [`unsupported-operation`](#unsupported-operation)
- [`warn`](#warn)
- [`when-available`](#when-available)
- [`with-altered-vars`](#with-altered-vars)
- [`with-memoized-fns`](#with-memoized-fns)
- [`with-printed-result`](#with-printed-result)
- [`with-str-out`](#with-str-out)
- [`with-testing-data`](#with-testing-data)
- [`with-var-bindings`](#with-var-bindings)
- [`yes-or-no?`](#yes-or-no?)
- [`zip`](#zip)

### Function Documentation 

#### <a name="<=&gt;"> Function `<=>` 

Argument List: <tt>([&zwj;a b])</tt> 

Documentation: 
>   Implements equivalence. 

#### <a name="=&gt;"> Function `=>` 

Argument List: <tt>([&zwj;a b])</tt> 

Documentation: 
>   Implements implication. 

#### <a name="abs"> Function `abs` 

Argument List: <tt>([&zwj;n])</tt> 

Documentation: 
>   (abs n) is the absolute value of n 

#### <a name="all-closed-sets"> Function `all-closed-sets` 

Argument List: <tt>([&zwj;base clop] [&zwj;base clop initial])</tt> 

Documentation: 
>   Computes all closed sets of a given closure operator on a given
>   set. Uses initial as first closed set if supplied. 

#### <a name="all-closed-sets-in-family"> Function `all-closed-sets-in-family` 

Argument List: <tt>([&zwj;predicate base clop] [&zwj;predicate base clop initial])</tt> 

Documentation: 
>   Computes all closed sets of a given closure operator on a given set
>   base contained in the family described by predicate. See
>   documentation of next-closed-set-in-family for more details. Uses
>   initial as first closed set if supplied. 

#### <a name="ask"> Function `ask` 

Argument List: <tt>([&zwj;prompt read & preds-and-fail-messages])</tt> 

Documentation: 
>   Performs simple quering. prompt is printed first and then the user
>   is asked for an answer (via read). The other arguments are
>   predicates with corresponding error messages. If a given answer does
>   not satisfy some predicate pred, it's associated error message is
>   printed (if it is a string) or it is assumed to be a function of one
>   argument, whereupon it is called with the invalid answer and the
>   result is printed. In any case, the user is asked again, until the
>   given answer fulfills all given predicates. 

#### <a name="ceil"> Function `ceil` 

Argument List: <tt>([&zwj;n])</tt> 

Documentation: 
>   (ceil n) returns the least integer greater than or equal to n.
>   If n is an exact number, ceil returns an integer, otherwise a double. 

#### <a name="clojure-type"> Function `clojure-type` 

Argument List: <tt>([&zwj;thing])</tt> 

Documentation: 
>   Dispatch function for multimethods. 

#### <a name="compare-order"> Function `compare-order` 

Argument List: <tt>([&zwj;x y])</tt> 

Documentation: 
>   Orders things for proper output of formal contexts. 

#### <a name="conexp-version"> Function `conexp-version` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns the version of conexp as a string. 

#### <a name="covers?"> Function `covers?` 

Argument List: <tt>([&zwj;sets base-set])</tt> 

Documentation: 
>   Technical Helper. Tests wheterh all elements in base-set are contained at
>   least one set in sets. 

#### <a name="cross-product"> Function `cross-product` 

Argument List: <tt>([&zwj;& sets])</tt> 

Documentation: 
>   Returns cross product of set-1 and set-2. 

#### <a name="def-"> Function `def-` 

Argument List: <tt>([&zwj;name & decls])</tt> 

Documentation: 
>   Same as def, but yields a private definition 

#### <a name="defalias"> Function `defalias` 

Argument List: <tt>([&zwj;name orig] [&zwj;name orig doc])</tt> 

Documentation: 
>   Defines an alias for a var: a new var with the same root binding (if
>   any) and similar metadata. The metadata of the alias is its initial
>   metadata (as provided by def) merged into the metadata of the original. 

#### <a name="defmacro-"> Function `defmacro-` 

Argument List: <tt>([&zwj;name & decls])</tt> 

Documentation: 
>   Same as defmacro but yields a private definition 

#### <a name="die-with-error"> Function `die-with-error` 

Argument List: <tt>([&zwj;error strings])</tt> 

Documentation: 
>   Stops program by raising the given error with strings as message. 

#### <a name="difference"> Function `difference` 

Argument List: <tt>([&zwj;s1] [&zwj;s1 s2] [&zwj;s1 s2 & sets])</tt> 

Documentation: 
>   Return a set that is the first set without elements of the remaining sets 

#### <a name="disjoint-union"> Function `disjoint-union` 

Argument List: <tt>([&zwj;& sets])</tt> 

Documentation: 
>   Computes the disjoint union of sets by joining the cross-products of the
>   sets with natural numbers. 

#### <a name="distinct-by-key"> Function `distinct-by-key` 

Argument List: <tt>([&zwj;sequence key])</tt> 

Documentation: 
>   Returns a sequence of all elements of the given sequence with distinct key
>   values, where key is a function from the elements of the given sequence. If
>   two elements correspond to the same key, the one is chosen which appeared
>   earlier in the sequence.
>   
>   This function is copied from clojure.core/distinct and adapted for using a key
>   function. 

#### <a name="dopar"> Function `dopar` 

Argument List: <tt>([&zwj;[&zwj;k v] & body])</tt> 

Documentation: 
>   Executes body binding k to each value in v.
>   Execution is done in parallel.  Code adapted from
>   http://www.acooke.org/cute/Clojuremac0.html 

#### <a name="ensure-length"> Function `ensure-length` 

Argument List: <tt>([&zwj;string length] [&zwj;string length padding])</tt> 

Documentation: 
>   Fills given string with padding to have at least the given length. 

#### <a name="ensure-seq"> Function `ensure-seq` 

Argument List: <tt>([&zwj;x])</tt> 

Documentation: 
>   Given anything that can be made a sequence from, returns that
>   sequence. If given a number x, returns (range x). 

#### <a name="exact-integer-sqrt"> Function `exact-integer-sqrt` 

Argument List: <tt>([&zwj;n])</tt> 

Documentation: 
>   (exact-integer-sqrt n) expects a non-negative integer n, and returns [s r] where n = s^2+r and n < (s+1)^2.  In other words, it returns the floor of the square root and the 'remainder'.
>   For example, (exact-integer-sqrt 15) is [3 6] because 15 = 3^2+6. 

#### <a name="exists"> Function `exists` 

Argument List: <tt>([&zwj;bindings condition])</tt> 

Documentation: 
>   Implements logical exists quantor. Bindings is of the form [var-1
>   seq-1 var-2 seq-2 ...]. Returns boolean value. 

#### <a name="expand-bindings"> Function `expand-bindings` 

Argument List: <tt>([&zwj;bindings body])</tt> 

Documentation: 
>   Expands bindings used by forall and exists. 

#### <a name="expt"> Function `expt` 

Argument List: <tt>([&zwj;a b])</tt> 

Documentation: 
>   Exponentiation of arguments. Is exact if given arguments are exact
>   and returns double otherwise. 

#### <a name="first-non-nil"> Function `first-non-nil` 

Argument List: <tt>([&zwj;seq])</tt> 

Documentation: 
>   Returns first non-nil element in seq, or nil if there is none. 

#### <a name="first-position-if"> Function `first-position-if` 

Argument List: <tt>([&zwj;predicate sequence])</tt> 

Documentation: 
>   Return the index of the first element in sequence for which predicate returns
>   true. 

#### <a name="floor"> Function `floor` 

Argument List: <tt>([&zwj;n])</tt> 

Documentation: 
>   (floor n) returns the greatest integer less than or equal to n.
>   If n is an exact number, floor returns an integer, otherwise a double. 

#### <a name="forall"> Function `forall` 

Argument List: <tt>([&zwj;bindings condition])</tt> 

Documentation: 
>   Implements logical forall quantor. Bindings is of the form [var-1
>   seq-1 var-2 seq-2 ...]. Returns boolean value. 

#### <a name="gcd"> Function `gcd` 

Argument List: <tt>([&zwj;a b])</tt> 

Documentation: 
>   (gcd a b) returns the greatest common divisor of a and b 

#### <a name="generic-equals"> Function `generic-equals` 

Argument List: <tt>([&zwj;[&zwj;this other] class fields])</tt> 

Documentation: 
>   Implements a generic equals for class on fields. 

#### <a name="graph-of-function?"> Function `graph-of-function?` 

Argument List: <tt>([&zwj;relation source target])</tt> 

Documentation: 
>   Returns true iff relation is the graph of a function from source to target. 

#### <a name="has-version?"> Function `has-version?` 

Argument List: <tt>([&zwj;{my-major :major, my-minor :minor, my-patch :patch}])</tt> 

Documentation: 
>   Compares given version of conexp and returns true if and only if
>   the current version of conexp is higher or equal than the given one 

#### <a name="hash-combine-hash"> Function `hash-combine-hash` 

Argument List: <tt>([&zwj;& args])</tt> 

Documentation: 
>   Combines the hashes of all things given. 

#### <a name="illegal-argument"> Function `illegal-argument` 

Argument List: <tt>([&zwj;& strings])</tt> 

Documentation: 
>   Throws IllegalArgumentException with given strings as message. 

#### <a name="illegal-state"> Function `illegal-state` 

Argument List: <tt>([&zwj;& strings])</tt> 

Documentation: 
>   Throws IllegalStateException with given strings as message. 

#### <a name="immigrate"> Function `immigrate` 

Argument List: <tt>([&zwj;& ns-names])</tt> 

Documentation: 
>   Create a public var in this namespace for each public var in the
>   namespaces named by ns-names. The created vars have the same name, root
>   binding, and metadata as the original except that their :ns metadata
>   value is this namespace.
>   
>   This function is literally copied from the clojure.contrib.ns-utils library. 

#### <a name="improve-basic-order"> Function `improve-basic-order` 

Argument List: <tt>([&zwj;base clop])</tt> 

Documentation: 
>   Improves basic order on the sequence base, where the closure operator
>   clop operates on. 

#### <a name="index"> Function `index` 

Argument List: <tt>([&zwj;xrel ks])</tt> 

Documentation: 
>   Returns a map of the distinct values of ks in the xrel mapped to a
>   set of the maps in xrel with the corresponding values of ks. 

#### <a name="inits"> Function `inits` 

Argument List: <tt>([&zwj;sqn])</tt> 

Documentation: 
>   Returns a lazy sequence of the beginnings of sqn. 

#### <a name="integer-length"> Function `integer-length` 

Argument List: <tt>([&zwj;n])</tt> 

Documentation: 
>   Length of integer in binary 

#### <a name="intersection"> Function `intersection` 

Argument List: <tt>([&zwj;s1] [&zwj;s1 s2] [&zwj;s1 s2 & sets])</tt> 

Documentation: 
>   Return a set that is the intersection of the input sets 

#### <a name="intersection-set?"> Function `intersection-set?` 

Argument List: <tt>([&zwj;set sets])</tt> 

Documentation: 
>   Tests whether set has non-empty intersection with every set in sets. 

#### <a name="join"> Function `join` 

Argument List: <tt>([&zwj;xrel yrel] [&zwj;xrel yrel km])</tt> 

Documentation: 
>   When passed 2 rels, returns the rel corresponding to the natural
>   join. When passed an additional keymap, joins on the corresponding
>   keys. 

#### <a name="lcm"> Function `lcm` 

Argument List: <tt>([&zwj;a b])</tt> 

Documentation: 
>   (lcm a b) returns the least common multiple of a and b 

#### <a name="lectic-<"> Function `lectic-<` 

Argument List: <tt>([&zwj;base A B])</tt> 

Documentation: 
>   Implements lectic ordering. The basic order is given by the ordering of base
>   which is interpreted as increasing order. 

#### <a name="lectic-<_i"> Function `lectic-<_i` 

Argument List: <tt>([&zwj;base i A B])</tt> 

Documentation: 
>   Implements lectic < at position i. The basic order is given by the ordering
>   of base which is interpreted as increasing order.
>   
>   A and B have to be sets. 

#### <a name="map-by-fn"> Function `map-by-fn` 

Argument List: <tt>([&zwj;function keys])</tt> 

Documentation: 
>   Returns a hash map with the values of keys as keys and their values
>   under function as values. 

#### <a name="map-invert"> Function `map-invert` 

Argument List: <tt>([&zwj;m])</tt> 

Documentation: 
>   Returns the map with the vals mapped to the keys. 

#### <a name="memo-fn"> Function `memo-fn` 

Argument List: <tt>([&zwj;name args & body])</tt> 

Documentation: 
>   Defines memoized, anonymous function. 

#### <a name="minimal-generating-subsets"> Function `minimal-generating-subsets` 

Argument List: <tt>([&zwj;clop A])</tt> 

Documentation: 
>   Given a set A and a closure operator clop returns all subsets B of
>   A such that (= (clop A) (clop B)). 

#### <a name="minimal-hypergraph-transversals"> Function `minimal-hypergraph-transversals` 

Argument List: <tt>([&zwj;vertices edges])</tt> 

Documentation: 
>   Returns all minimal hypergraph transversals of the hypergraph defined by
>   «edges» on the vertex sets «vertices». 

#### <a name="minimum-set-covers"> Function `minimum-set-covers` 

Argument List: <tt>([&zwj;base-set sets])</tt> 

Documentation: 
>   For a given set base-set and a collection of sets returns all subcollections
>   of sets such that the union of the contained sets cover base-set and that are
>   minimal with that property. 

#### <a name="next-closed-set"> Function `next-closed-set` 

Argument List: <tt>([&zwj;base clop A])</tt> 

Documentation: 
>   Computes next closed set of the closure operator clop after A with
>   the Next Closure algorithm. The order of elements in base,
>   interpreted as increasing, is taken to be the basic order of the
>   elements. 

#### <a name="next-closed-set-in-family"> Function `next-closed-set-in-family` 

Argument List: <tt>([&zwj;predicate base clop A])</tt> 

Documentation: 
>   Computes next closed set as with next-closed-set, which is in the
>   family F of all closed sets satisfing predicate. predicate has to
>   satisfy the condition
>   
>     A in F and i in base ==> clop(A union {1, ..., i-1}) in F.
>    

#### <a name="not-yet-implemented"> Function `not-yet-implemented` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Throws UnsupportedOperationException with "Not yet implemented"
>   message. 

#### <a name="now"> Function `now` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns the current time in a human readable format. 

#### <a name="order-by"> Function `order-by` 

Argument List: <tt>([&zwj;sequence])</tt> 

Documentation: 
>   Returns a function on two arguments a and b that returns true if and only if
>   b occurs not after a in the given sequence (ascending order.) 

#### <a name="parallel-closures"> Function `parallel-closures` 

Argument List: <tt>([&zwj;base clop])</tt> 

Documentation: 
>   Returns the set of all closures of the closure operator on the given base set.
>   Computes the closures in parallel, to the extent possible. 

#### <a name="partial-max"> Function `partial-max` 

Argument List: <tt>([&zwj;<= xs])</tt> 

Documentation: 
>   For a given partial order <= and given elements returns the maximal
>   among them. 

#### <a name="partial-min"> Function `partial-min` 

Argument List: <tt>([&zwj;<= xs])</tt> 

Documentation: 
>   For a given partial order <= and given elements returns the minimal
>   among them. 

#### <a name="prod"> Function `prod` 

Argument List: <tt>([&zwj;index start end & expr])</tt> 

Documentation: 
>   Computes the product of expr for indices from start to end, named
>   as index. 

#### <a name="project"> Function `project` 

Argument List: <tt>([&zwj;xrel ks])</tt> 

Documentation: 
>   Returns a rel of the elements of xrel with only the keys in ks 

#### <a name="proper-subset?"> Function `proper-subset?` 

Argument List: <tt>([&zwj;set-1 set-2])</tt> 

Documentation: 
>   Returns true iff set-1 is a proper subset of set-2. 

#### <a name="proper-superset?"> Function `proper-superset?` 

Argument List: <tt>([&zwj;set-1 set-2])</tt> 

Documentation: 
>   Returns true iff set-1 is a proper superset of set-2. 

#### <a name="quit"> Function `quit` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Quits conexp-clj. 

#### <a name="reduce!"> Function `reduce!` 

Argument List: <tt>([&zwj;fn initial-value coll])</tt> 

Documentation: 
>   Does the same as reduce, but calls transient on the initial value
>   and persistent! on the result. 

#### <a name="redundant?"> Function `redundant?` 

Argument List: <tt>([&zwj;base-set cover count])</tt> 

Documentation: 
>   Technical Helper. For a given set base-set, a collection cover of sets and a
>   map mapping elements from base-set to the number of times they occur in sets
>   in cover, tests whether the cover is redundant or not, i.e. if a proper
>   subcollection of cover is already a cover or not. 

#### <a name="reflexive-transitive-closure"> Function `reflexive-transitive-closure` 

Argument List: <tt>([&zwj;base-set pairs])</tt> 

Documentation: 
>   Computes the reflexive, transitive closure of a given set of pairs
>   on base-set. 

#### <a name="rename"> Function `rename` 

Argument List: <tt>([&zwj;xrel kmap])</tt> 

Documentation: 
>   Returns a rel of the maps in xrel with the keys in kmap renamed to the vals in kmap 

#### <a name="rename-keys"> Function `rename-keys` 

Argument List: <tt>([&zwj;map kmap])</tt> 

Documentation: 
>   Returns the map with the keys in kmap renamed to the vals in kmap 

#### <a name="round"> Function `round` 

Argument List: <tt>([&zwj;n])</tt> 

Documentation: 
>   (round n) rounds to the nearest integer.
>   round always returns an integer.  Rounds up for values exactly in between two integers. 

#### <a name="select"> Function `select` 

Argument List: <tt>([&zwj;pred xset])</tt> 

Documentation: 
>   Returns a set of the elements for which pred is true 

#### <a name="set-of"> Function `set-of` 

Argument List: <tt>([&zwj;thing & condition])</tt> 

Documentation: 
>   Macro for writing sets as mathematicians do (at least similar to
>   it.) The following syntax constructions are supported:
>   
>     (set-of x [x [1 2 3]])
>       for the set of all x with x in [1 2 3]
>   
>     (set-of x | x [1 2 3])
>       for the same set
>   
>     (set-of [x y] [x [1 2 3], y [4 5 6] :when (= 1 (gcd x y))])
>       for the set of all pairs [x y] with x in [1 2 3], y in [4 5 6]
>       and x and y are coprime.
>   
>   In general, the condition vector (or the sequence after |) must be
>   suitable for doseq. 

#### <a name="set-of-range"> Function `set-of-range` 

Argument List: <tt>([&zwj;end] [&zwj;start end] [&zwj;start end step])</tt> 

Documentation: 
>   Returns a set of all numbers from start upto (but not including) end,
>   by step if provided. 

#### <a name="singleton?"> Function `singleton?` 

Argument List: <tt>([&zwj;x])</tt> 

Documentation: 
>   Returns true iff given thing is a singleton sequence or set. 

#### <a name="sort-by-first"> Function `sort-by-first` 

Argument List: <tt>([&zwj;x y])</tt> 

Documentation: 
>   Ensures that pairs are ordered by first entry first. 

#### <a name="sort-by-second"> Function `sort-by-second` 

Argument List: <tt>([&zwj;x y])</tt> 

Documentation: 
>   Ensures that pairs are ordered by second entry first. This gives
>   better output for context sums, products, ... 

#### <a name="split-at-first"> Function `split-at-first` 

Argument List: <tt>([&zwj;predicate sequence])</tt> 

Documentation: 
>   Splits given sequence at first element satisfing predicate.
>   The first element satisfing predicate will be in the second sequence. 

#### <a name="split-at-last"> Function `split-at-last` 

Argument List: <tt>([&zwj;predicate sequence])</tt> 

Documentation: 
>   Splits given sequence at last element satisfing predicate.
>   The last element satisfing predicate will be in the first sequence. 

#### <a name="sqrt"> Function `sqrt` 

Argument List: <tt>([&zwj;n])</tt> 

Documentation: 
>   Square root, but returns exact number if possible. 

#### <a name="subset?"> Function `subset?` 

Argument List: <tt>([&zwj;set1 set2])</tt> 

Documentation: 
>   Is set1 a subset of set2? 

#### <a name="subsets"> Function `subsets` 

Argument List: <tt>([&zwj;base-set])</tt> 

Documentation: 
>   Returns all subsets of the given base-set. 

#### <a name="sum"> Function `sum` 

Argument List: <tt>([&zwj;index start end & expr])</tt> 

Documentation: 
>   Computes the sum of expr for indices from start to end, named as
>   index. 

#### <a name="superset?"> Function `superset?` 

Argument List: <tt>([&zwj;set1 set2])</tt> 

Documentation: 
>   Is set1 a superset of set2? 

#### <a name="tails"> Function `tails` 

Argument List: <tt>([&zwj;sqn])</tt> 

Documentation: 
>   Returns a lazy sequence of the tails of sqn. 

#### <a name="to-set"> Function `to-set` 

Argument List: <tt>([&zwj;thing])</tt> 

Documentation: 
>   Converts given argument «thing» to a set. If it is a number,
>   returns the set {0, ..., thing-1}. If it is a collection,
>   returns (set thing). Otherwise raises an error. 

#### <a name="topological-sort"> Function `topological-sort` 

Argument List: <tt>([&zwj;comp coll])</tt> 

Documentation: 
>   Returns a linear extension of the given collection coll and the
>   supplied comparator comp. 

#### <a name="transitive-closure"> Function `transitive-closure` 

Argument List: <tt>([&zwj;pairs])</tt> 

Documentation: 
>   Computes transitive closure of a given set of pairs. 

#### <a name="transitive-reduction"> Function `transitive-reduction` 

Argument List: <tt>([&zwj;pairs] [&zwj;base pred])</tt> 

Documentation: 
>   Returns for a set of pairs its transitive reduction. Alternatively,
>   the relation can be given as a base set and a predicate p which
>   returns true in (p x y) iff [x y] is in the relation in question.
>   
>   Note that if the relation given is not acyclic, the transitive
>   closure of the reduction may not yield the transitive closure of the
>   original relation anymore, since the reduction itself can be empty. 

#### <a name="union"> Function `union` 

Argument List: <tt>([&zwj;] [&zwj;s1] [&zwj;s1 s2] [&zwj;s1 s2 & sets])</tt> 

Documentation: 
>   Return a set that is the union of the input sets 

#### <a name="unsupported-operation"> Function `unsupported-operation` 

Argument List: <tt>([&zwj;& strings])</tt> 

Documentation: 
>   Throws UnsupportedOperationException with given strings as message. 

#### <a name="warn"> Function `warn` 

Argument List: <tt>([&zwj;message])</tt> 

Documentation: 
>   Emits a warning message on *out*. 

#### <a name="with-altered-vars"> Function `with-altered-vars` 

Argument List: <tt>([&zwj;bindings & body])</tt> 

Documentation: 
>   Executes the code given in a dynamic environment where the var
>   roots of the given names are altered according to the given
>   bindings. The bindings have the form [name_1 f_1 name_2 f_2 ...]
>   where f_i is applied to the original value of the var associated
>   with name_i to give the new value which will be in place during
>   execution of body. The old value will be restored after execution
>   has been finished. 

#### <a name="with-memoized-fns"> Function `with-memoized-fns` 

Argument List: <tt>([&zwj;functions & body])</tt> 

Documentation: 
>   Runs code in body with all given functions memoized. 

#### <a name="with-printed-result"> Function `with-printed-result` 

Argument List: <tt>([&zwj;string & body])</tt> 

Documentation: 
>   Prints string followed by result, returning it. 

#### <a name="with-str-out"> Function `with-str-out` 

Argument List: <tt>([&zwj;& body])</tt> 

Documentation: 
>   Returns string of all output being made in (flatten body). 

#### <a name="with-testing-data"> Function `with-testing-data` 

Argument List: <tt>([&zwj;bindings & body])</tt> 

Documentation: 
>   Expects for all bindings the body to be evaluated to true. bindings
>   must be those of doseq. 

#### <a name="with-var-bindings"> Function `with-var-bindings` 

Argument List: <tt>([&zwj;bindings & body])</tt> 

Documentation: 
>   Executes body with the vars in bindings set to the corresponding
>   values. 

#### <a name="yes-or-no?"> Function `yes-or-no?` 

Argument List: <tt>([&zwj;question])</tt> 

Documentation: 
>   Asks string, expecting 'yes' or 'no'. Returns true when answered
>   'yes' and false otherwise. 

#### <a name="zip"> Function `zip` 

Argument List: <tt>([&zwj;seq-1 seq-2])</tt> 

Documentation: 
>   Returns sequence of pairs [x,y] where x runs through seq-1 and
>   y runs through seq-2 simultaneously. This is the same as
>   (map #(vector %1 %2) seq-1 seq-2). 


## <a name="conexp.fca.contexts"> Public API of conexp.fca.contexts 


  Provides the implementation of formal contexts and functions on them.

### Available Functions 

- [`->Formal-Context`](#->Formal-Context)
- [`adiag-context`](#adiag-context)
- [`adprime`](#adprime)
- [`aprime`](#aprime)
- [`attribute-clarified?`](#attribute-clarified?)
- [`attribute-concept`](#attribute-concept)
- [`attribute-derivation`](#attribute-derivation)
- [`attribute-reduced?`](#attribute-reduced?)
- [`attributes`](#attributes)
- [`cbo-test`](#cbo-test)
- [`clarify-attributes`](#clarify-attributes)
- [`clarify-context`](#clarify-context)
- [`clarify-objects`](#clarify-objects)
- [`compatible-subcontext?`](#compatible-subcontext?)
- [`compatible-subcontexts`](#compatible-subcontexts)
- [`concept?`](#concept?)
- [`concepts`](#concepts)
- [`context-apposition`](#context-apposition)
- [`context-attribute-closure`](#context-attribute-closure)
- [`context-clarified?`](#context-clarified?)
- [`context-composition`](#context-composition)
- [`context-disjoint-union`](#context-disjoint-union)
- [`context-intersection`](#context-intersection)
- [`context-object-closure`](#context-object-closure)
- [`context-product`](#context-product)
- [`context-reduced?`](#context-reduced?)
- [`context-semiproduct`](#context-semiproduct)
- [`context-size`](#context-size)
- [`context-subposition`](#context-subposition)
- [`context-sum`](#context-sum)
- [`context-to-string`](#context-to-string)
- [`context-transitive-closure`](#context-transitive-closure)
- [`context-union`](#context-union)
- [`context-xia-product`](#context-xia-product)
- [`context?`](#context?)
- [`diag-context`](#diag-context)
- [`direct-lower-concepts`](#direct-lower-concepts)
- [`direct-upper-concepts`](#direct-upper-concepts)
- [`down-arrows`](#down-arrows)
- [`dual-context`](#dual-context)
- [`extent?`](#extent?)
- [`extents`](#extents)
- [`incidence`](#incidence)
- [`incidence-relation`](#incidence-relation)
- [`incident?`](#incident?)
- [`intent?`](#intent?)
- [`intents`](#intents)
- [`invert-context`](#invert-context)
- [`make-context`](#make-context)
- [`make-context-from-matrix`](#make-context-from-matrix)
- [`make-context-nc`](#make-context-nc)
- [`null-context`](#null-context)
- [`object-clarified?`](#object-clarified?)
- [`object-concept`](#object-concept)
- [`object-derivation`](#object-derivation)
- [`object-reduced?`](#object-reduced?)
- [`objects`](#objects)
- [`odprime`](#odprime)
- [`one-context`](#one-context)
- [`oprime`](#oprime)
- [`print-context`](#print-context)
- [`rand-context`](#rand-context)
- [`random-context`](#random-context)
- [`random-contexts`](#random-contexts)
- [`reduce-attributes`](#reduce-attributes)
- [`reduce-context`](#reduce-context)
- [`reduce-objects`](#reduce-objects)
- [`rename-attributes`](#rename-attributes)
- [`rename-objects`](#rename-objects)
- [`restrict-concept`](#restrict-concept)
- [`subconcept?`](#subconcept?)
- [`subconceptneq?`](#subconceptneq?)
- [`subcontext?`](#subcontext?)
- [`up-arrows`](#up-arrows)
- [`up-down-arrows`](#up-down-arrows)

### Function Documentation 

#### <a name="-&gt;Formal-Context"> Function `->Formal-Context` 

Argument List: <tt>([&zwj;objects attributes incidence])</tt> 

Documentation: 
>   Positional factory function for class conexp.fca.contexts.Formal-Context. 

#### <a name="adiag-context"> Function `adiag-context` 

Argument List: <tt>([&zwj;base-set])</tt> 

Documentation: 
>   Returns not= on base-set as context. 

#### <a name="adprime"> Function `adprime` 

Argument List: <tt>([&zwj;ctx set-of-attributes])</tt> 

Documentation: 
>   Computes double prime in context ctx for the given set-of-attributes. 

#### <a name="aprime"> Function `aprime` 

Argument List: <tt>([&zwj;ctx attributes])</tt> 

Documentation: 
>   Computes set of objects common to all attributes in context. 

#### <a name="attribute-clarified?"> Function `attribute-clarified?` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Tests whether given context ctx is attribute clarified. 

#### <a name="attribute-concept"> Function `attribute-concept` 

Argument List: <tt>([&zwj;ctx m])</tt> 

Documentation: 
>   Returns the attribute concept of the given attribute m in context
>   ctx. 

#### <a name="attribute-derivation"> Function `attribute-derivation` 

Argument List: <tt>([&zwj;ctx attributes])</tt> 

Documentation: 
>   Computes set of objects common to all attributes in context. 

#### <a name="attribute-reduced?"> Function `attribute-reduced?` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Tests whether given context ctx is attribute-reduced or not. 

#### <a name="attributes"> Function `attributes` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns the attributes of a context. 

#### <a name="cbo-test"> Function `cbo-test` 

Argument List: <tt>([&zwj;attribute j B D])</tt> 

Documentation: 
>   Simple implementation of the test used by the «Close by One»
>   algorithm. 

#### <a name="clarify-attributes"> Function `clarify-attributes` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Clarifies attributes in context ctx. 

#### <a name="clarify-context"> Function `clarify-context` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Clarifies context ctx. 

#### <a name="clarify-objects"> Function `clarify-objects` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Clarifies objects in context ctx. 

#### <a name="compatible-subcontext?"> Function `compatible-subcontext?` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   Tests whether ctx-1 is a compatible subcontext of ctx-2. 

#### <a name="compatible-subcontexts"> Function `compatible-subcontexts` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns all compatible subcontexts of ctx. ctx has to be reduced. 

#### <a name="concept?"> Function `concept?` 

Argument List: <tt>([&zwj;ctx [&zwj;set-of-obj set-of-att]])</tt> 

Documentation: 
>   Tests whether given pair is a concept in context ctx. 

#### <a name="concepts"> Function `concepts` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns a sequence of all concepts of ctx. 

#### <a name="context-apposition"> Function `context-apposition` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   Returns context apposition of ctx-1 and ctx-2, that is
>   
>    (G_1,M_1,I_1) | (G_1,M_2,I_2) := (G_1,M_1 dunion M_2,I_1 dunion I_2).
>    

#### <a name="context-attribute-closure"> Function `context-attribute-closure` 

Argument List: <tt>([&zwj;ctx set-of-attributes])</tt> 

Documentation: 
>   Computes double prime in context ctx for the given set-of-attributes. 

#### <a name="context-clarified?"> Function `context-clarified?` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Tests whether given context ctx is clarified. 

#### <a name="context-composition"> Function `context-composition` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   Returns context composition of ctx-1 and ctx-2, that is
>   
>     (G_1,M_1,I_1) o (G_2,M_2,I_2) := (G_1,M_2,I_1 o I_2).
>    

#### <a name="context-disjoint-union"> Function `context-disjoint-union` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   Returns the disjoint union of ctx-1 and ctx-2. 

#### <a name="context-intersection"> Function `context-intersection` 

Argument List: <tt>([&zwj;ctx1 ctx2])</tt> 

Documentation: 
>   Returns context intersection of ctx1 and ctx2. 

#### <a name="context-object-closure"> Function `context-object-closure` 

Argument List: <tt>([&zwj;ctx set-of-objects])</tt> 

Documentation: 
>   Computes double prime in context ctx for the given set-of-objects. 

#### <a name="context-product"> Function `context-product` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   Computes the context product of ctx-1 and ctx-2, that is
>   
>     (G_1,M_1,I_1) x (G_2,M_2,I_2) :=
>       (G_1 x G_2, M_1 x M_2, nabla)
>   
>   where
>   
>     (g_1,g_2) nabla (m_1,m_2) <=> g_1I_1m_1 or g_2I_2m_2.
>    

#### <a name="context-reduced?"> Function `context-reduced?` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Tests whether given context ctx is reduced or not. 

#### <a name="context-semiproduct"> Function `context-semiproduct` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   Computes the context semiproduct of ctx-1 and ctx-2, where for
>   contexts (G_1,M_1,I_1) and (G_2,M_2,I_2) their semidirect product is
>   defined as
>   
>     (G_1 x G_2, M_1 dunion M_2, nabla)
>   
>   where
>   
>     (g_1,g_2) nabla (j,m) <=> g_jI_jm for j in {1,2}.
>    

#### <a name="context-size"> Function `context-size` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns tuple of number of objects, number of attributes, and fill rate. 

#### <a name="context-subposition"> Function `context-subposition` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   Returns context subposition of ctx-1 and ctx-2, that is
>   
>      (G_1,M_1,I_1)
>     --------------- := (G_1 dunion G_2, M_1, I_1 union I_2).
>      (G_2,M_1,I_2)
>    

#### <a name="context-sum"> Function `context-sum` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   Computes the context sum of ctx-1 and ctx-2, that is
>   
>     (G_1,M_1,I_1) + (G_2,M_2,I_2) :=
>       (G_1 dunion G_2, M_1 dunion M_2, I_1 dunion I_2
>        dunion (G_1 x M_2) dunion (G_2 x M_1))
>   
>   where all set unions are disjoint set unions. 

#### <a name="context-to-string"> Function `context-to-string` 

Argument List: <tt>([&zwj;ctx] [&zwj;ctx order-on-objects order-on-attributes])</tt> 

Documentation: 
>   Prints contexts in a human readable form. Orderings can be given as
>   sequences or as functions. If given as sequence, the corresponding
>   elements will be ordered that way, with all remaining elements at
>   the end. If given as function, sort the corresponding elements with
>   that function. If no ordering is given for objects and attributes,
>   sort-by-second is used.  

#### <a name="context-transitive-closure"> Function `context-transitive-closure` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Transitively closes incidence relation of ctx and returns corresponding context. 

#### <a name="context-union"> Function `context-union` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   Returns the union of ctx-1 and ctx-2. Note that this union is
>   inclusive; use context-disjoint-union if this is not what you want. 

#### <a name="context-xia-product"> Function `context-xia-product` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   Computes Xia's product of ctx-1 and ctx-2, where for two contexts
>   (G_1,M_1,I_1) and (G_2,M_2,I_2) their Xia product is defined as
>   
>     (G_1 x G_2, M_1 x M_2, ~)
>   
>   where
>   
>     (g_1,g_2) ~ (m_1,m_2) <=> (g_1I_1m_1 <=> g_2I_2m_2).
>    

#### <a name="context?"> Function `context?` 

Argument List: <tt>([&zwj;thing])</tt> 

Documentation: 
>   Returns true iff thing is a formal context. 

#### <a name="diag-context"> Function `diag-context` 

Argument List: <tt>([&zwj;base-set])</tt> 

Documentation: 
>   Returns = on base-set as context. 

#### <a name="direct-lower-concepts"> Function `direct-lower-concepts` 

Argument List: <tt>([&zwj;ctx [&zwj;A B]])</tt> 

Documentation: 
>   Computes the set of direct upper neighbours of the concept [A B] in
>   the concept lattice of ctx. Uses Lindig's Algorithm for that. 

#### <a name="direct-upper-concepts"> Function `direct-upper-concepts` 

Argument List: <tt>([&zwj;ctx [&zwj;A B]])</tt> 

Documentation: 
>   Computes the set of direct upper neighbours of the concept [A B] in
>   the concept lattice of ctx. Uses Lindig's Algorithm for that. 

#### <a name="down-arrows"> Function `down-arrows` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Computes the down arrow relation of ctx. 

#### <a name="dual-context"> Function `dual-context` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Dualizes context ctx, that is (G,M,I) gets (M,G,I^{-1}). 

#### <a name="extent?"> Function `extent?` 

Argument List: <tt>([&zwj;ctx thing])</tt> 

Documentation: 
>   Test whether `thing' is an extent of the formal context `ctx.' 

#### <a name="extents"> Function `extents` 

Argument List: <tt>([&zwj;ctx] [&zwj;ctx pred])</tt> 

Documentation: 
>   Computes a sequence of all extents of «ctx», in a lectic order.  Optionally, one can
>   specify a predicate function «pred» that acts as a filter on all extents of «ctx».
>   «pred» should specify the same conditions as the predicate function to
>   «next-closed-set-in-family». 

#### <a name="incidence"> Function `incidence` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns a function that, given a pair [a b], returns true if and only
>   if a and b are incident in the context ctx. 

#### <a name="incidence-relation"> Function `incidence-relation` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns the incidence-relation of the given context, as a set of pairs of objects and
>   attributes. 

#### <a name="incident?"> Function `incident?` 

Argument List: <tt>([&zwj;ctx g m])</tt> 

Documentation: 
>   Returns true if and only if in context ctx, the object g is
>   incident with the attribute m. 

#### <a name="intent?"> Function `intent?` 

Argument List: <tt>([&zwj;ctx thing])</tt> 

Documentation: 
>   Test whether `thing' is an intent of the formal context `ctx.' 

#### <a name="intents"> Function `intents` 

Argument List: <tt>([&zwj;ctx] [&zwj;ctx pred])</tt> 

Documentation: 
>   Computes a sequence of all intents of «ctx», in a lectic order.  Optionally, one can
>   specify a predicate function «pred» that acts as a filter on all intents of «ctx».
>   «pred» should specify the same conditions as the predicate function to
>   «next-closed-set-in-family». 

#### <a name="invert-context"> Function `invert-context` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Inverts context ctx, that is (G,M,I) gets (G,M,(G x M) \ I). 

#### <a name="make-context"> Function `make-context` 

Argument List: <tt>([&zwj;objects attributes incidence])</tt> 

Documentation: 
>   Standard constructor for contexts. Takes a sequence of objects,
>   a sequence of attributes and either a set of pairs or function of two arguments being
>   true iff its arguments are incident. Note that the object and attribute sequences are
>   converted to sets and therefore have to not contain any duplicate elements. If the
>   incidence relation is given as a sequence, it is automatically restricted to the
>   cartesian product of the object an the attribute set. 

#### <a name="make-context-from-matrix"> Function `make-context-from-matrix` 

Argument List: <tt>([&zwj;G M bits])</tt> 

Documentation: 
>   Given objects G and attribute M and an incidence matrix constructs
>   the corresponding context. G and M may also be numbers where they
>   represent (range G) and (range M) respectively. 

#### <a name="make-context-nc"> Function `make-context-nc` 

Argument List: <tt>([&zwj;objects attributes incidence])</tt> 

Documentation: 
>   Context constructor similar to make-context, but does not do any
>   safety checking and is therefore faster. Use with care.
>   
>   This function is useful if you want to construct new contexts from old ones.  In
>   particular, if you want to use the incidence relation if a given context as the
>   incidence relation for a new one, you can just pass it to this function without any
>   modifications. 

#### <a name="null-context"> Function `null-context` 

Argument List: <tt>([&zwj;base-set])</tt> 

Documentation: 
>   Returns context with no crosses. 

#### <a name="object-clarified?"> Function `object-clarified?` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Tests whether given context ctx is object clarified. 

#### <a name="object-concept"> Function `object-concept` 

Argument List: <tt>([&zwj;ctx g])</tt> 

Documentation: 
>   Returns the object concept of the given object g in context ctx. 

#### <a name="object-derivation"> Function `object-derivation` 

Argument List: <tt>([&zwj;ctx objects])</tt> 

Documentation: 
>   Computes set of attributes common to all objects in context. 

#### <a name="object-reduced?"> Function `object-reduced?` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Tests whether given context ctx is object-reduced or not. 

#### <a name="objects"> Function `objects` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns the objects of a context. 

#### <a name="odprime"> Function `odprime` 

Argument List: <tt>([&zwj;ctx set-of-objects])</tt> 

Documentation: 
>   Computes double prime in context ctx for the given set-of-objects. 

#### <a name="one-context"> Function `one-context` 

Argument List: <tt>([&zwj;base-set])</tt> 

Documentation: 
>   Returns context full of crosses. 

#### <a name="oprime"> Function `oprime` 

Argument List: <tt>([&zwj;ctx objects])</tt> 

Documentation: 
>   Computes set of attributes common to all objects in context. 

#### <a name="print-context"> Function `print-context` 

Argument List: <tt>([&zwj;ctx & args])</tt> 

Documentation: 
>   Prints the result of applying context-to-string to the given
>   arguments. 

#### <a name="rand-context"> Function `rand-context` 

Argument List: <tt>([&zwj;base-set fill-rate] [&zwj;objects attributes fill-rate])</tt> 

Documentation: 
>   Randomly fills context on base-set (or on objects and attributes)
>   with crosses and propability fill-rate. If given numbers instead of
>   collections uses (range base-set) (and likewise for objects and
>   attributes) instead. 

#### <a name="random-context"> Function `random-context` 

Argument List: <tt>([&zwj;base-set fill-rate] [&zwj;objects attributes fill-rate])</tt> 

Documentation: 
>   Randomly fills context on base-set (or on objects and attributes)
>   with crosses and propability fill-rate. If given numbers instead of
>   collections uses (range base-set) (and likewise for objects and
>   attributes) instead. 

#### <a name="random-contexts"> Function `random-contexts` 

Argument List: <tt>([&zwj;number upper-limit])</tt> 

Documentation: 
>   Returns a sequence of number contexts, with random fill rate and random
>   size between 0 and upper-limit. 

#### <a name="reduce-attributes"> Function `reduce-attributes` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Attribute reduction for ctx. 

#### <a name="reduce-context"> Function `reduce-context` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Reduces context ctx. 

#### <a name="reduce-objects"> Function `reduce-objects` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Object reduction for ctx. 

#### <a name="rename-attributes"> Function `rename-attributes` 

Argument List: <tt>([&zwj;ctx old-to-new])</tt> 

Documentation: 
>   Rename attributes in ctx by given function old-to-new. 

#### <a name="rename-objects"> Function `rename-objects` 

Argument List: <tt>([&zwj;ctx old-to-new])</tt> 

Documentation: 
>   Rename objects in ctx by given function old-to-new. 

#### <a name="restrict-concept"> Function `restrict-concept` 

Argument List: <tt>([&zwj;concept subcontext])</tt> 

Documentation: 
>   Restricts the given concept to the given subcontext. 

#### <a name="subconcept?"> Function `subconcept?` 

Argument List: <tt>([&zwj;a b])</tt> 

Documentation: 
>   Tests if 'a is a subset of 'b, but not equal. 

#### <a name="subconceptneq?"> Function `subconceptneq?` 

Argument List: <tt>([&zwj;a b])</tt> 

Documentation: 
>   Tests if 'a is a subset of 'b, but not equal. 

#### <a name="subcontext?"> Function `subcontext?` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   Tests whether ctx-1 is a subcontext ctx-2 or not. 

#### <a name="up-arrows"> Function `up-arrows` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Computes the up arrow relation of ctx. 

#### <a name="up-down-arrows"> Function `up-down-arrows` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns up-down-arrow relation of ctx. 


## <a name="conexp.fca.many-valued-contexts"> Public API of conexp.fca.many-valued-contexts 


  Many-Valued-Contexts and some functions for scaling.

### Available Functions 

- [`->Many-Valued-Context`](#->Many-Valued-Context)
- [`biordinal-scale`](#biordinal-scale)
- [`dichotomic-scale`](#dichotomic-scale)
- [`incidences-of-object`](#incidences-of-object)
- [`interordinal-scale`](#interordinal-scale)
- [`interval-scale`](#interval-scale)
- [`make-mv-context`](#make-mv-context)
- [`make-mv-context-from-matrix`](#make-mv-context-from-matrix)
- [`make-mv-context-nc`](#make-mv-context-nc)
- [`make-mv-subcontext`](#make-mv-subcontext)
- [`mv-context-to-string`](#mv-context-to-string)
- [`nominal-scale`](#nominal-scale)
- [`object-by-incidence`](#object-by-incidence)
- [`ordinal-scale`](#ordinal-scale)
- [`scale-mv-context`](#scale-mv-context)
- [`scale-mv-context-with`](#scale-mv-context-with)
- [`values-of-attribute`](#values-of-attribute)
- [`values-of-object`](#values-of-object)

### Function Documentation 

#### <a name="-&gt;Many-Valued-Context"> Function `->Many-Valued-Context` 

Argument List: <tt>([&zwj;objects attributes incidence])</tt> 

Documentation: 
>   Positional factory function for class conexp.fca.many_valued_contexts.Many-Valued-Context. 

#### <a name="biordinal-scale"> Function `biordinal-scale` 

Argument List: <tt>([&zwj;values n] [&zwj;values others n <= >=])</tt> 

Documentation: 
>   Returns the biordinal scale on the sequence values, optionally given
>   two order relations <= and >=. Note that values (and others) must be
>   ordered (e.g. vector or list), because otherwise the result will be
>   arbitrary. 

#### <a name="dichotomic-scale"> Function `dichotomic-scale` 

Argument List: <tt>([&zwj;values])</tt> 

Documentation: 
>   Returns the dichotimic scale on the set values. Note that base must
>   have exactly two arguments. 

#### <a name="incidences-of-object"> Function `incidences-of-object` 

Argument List: <tt>([&zwj;mv-ctx g])</tt> 

Documentation: 
>   For a given many-valued context mv-ctx and a given object g,
>   returns the set of all values of g in mv-ctx. 

#### <a name="interordinal-scale"> Function `interordinal-scale` 

Argument List: <tt>([&zwj;values] [&zwj;values <= >=] [&zwj;values others <= >=])</tt> 

Documentation: 
>   Returns the interordinal scale on the set base, optionally given
>   two order relations <= and >=. 

#### <a name="interval-scale"> Function `interval-scale` 

Argument List: <tt>([&zwj;values] [&zwj;values others] [&zwj;values others < >=])</tt> 

Documentation: 
>   Returns the interval scale on the set values.
>   Note that values must be ordered (e.g. vector or list), because otherwise the result
>   will be arbitrary.  Also note that the intervales will be left-open. 

#### <a name="make-mv-context"> Function `make-mv-context` 

Argument List: <tt>([&zwj;objects attributes incidence])</tt> 

Documentation: 
>   Constructs a many-valued context from a set of objects, a set of
>   attributes and an incidence relation, given as set of triples [g m w]
>   or as a function from two arguments g and m to values w. 

#### <a name="make-mv-context-from-matrix"> Function `make-mv-context-from-matrix` 

Argument List: <tt>([&zwj;objects attributes values])</tt> 

Documentation: 
>   Creates a many-valued context from a given matrix of
>   values. objects and attributes may either be given as numbers
>   representing the corresponding number of objects and attributes
>   respectively, or as collections. The number of entries in values
>   must match the number of objects times the number of attributes. 

#### <a name="make-mv-context-nc"> Function `make-mv-context-nc` 

Argument List: <tt>([&zwj;objects attributes incidence])</tt> 

Documentation: 
>   Just creates a many-valued context from a set of objects, a set of
>   attributes and a hashmap from pairs of objects and attributes to
>   values. Does no checking, use with care. 

#### <a name="make-mv-subcontext"> Function `make-mv-subcontext` 

Argument List: <tt>([&zwj;mv-ctx objs attrs])</tt> 

Documentation: 
>   For a given many-valued context, returns the induced subcontext given
>   by the object and attribute set. 

#### <a name="mv-context-to-string"> Function `mv-context-to-string` 

Argument List: <tt>([&zwj;mv-ctx] [&zwj;mv-ctx order-on-objects order-on-attributes])</tt> 

Documentation: 
>   Returns a string representing the given many-valued context mv-ctx
>   as a value-table. 

#### <a name="nominal-scale"> Function `nominal-scale` 

Argument List: <tt>([&zwj;values] [&zwj;values others])</tt> 

Documentation: 
>   Returns the nominal scale on the set base. 

#### <a name="object-by-incidence"> Function `object-by-incidence` 

Argument List: <tt>([&zwj;mv-ctx values])</tt> 

Documentation: 
>   For a given many-valued context mv-ctx and an attribute-value map,
>   returns all objects having these values. 

#### <a name="ordinal-scale"> Function `ordinal-scale` 

Argument List: <tt>([&zwj;values] [&zwj;values <=] [&zwj;values others <=])</tt> 

Documentation: 
>   Returns the ordinal scale on the set values, optionally given an
>   order relation <=. 

#### <a name="scale-mv-context"> Function `scale-mv-context` 

Argument List: <tt>([&zwj;mv-ctx scales] [&zwj;mv-ctx scales default])</tt> 

Documentation: 
>   Scales given many-valued context mv-ctx with given scales. scales must be a map from attributes m
>   to contexts K, where all possible values of m in mv-ctx are among the objects in K. If a scale for
>   an attribute is given, the default scale is used, where default should be a function returning a
>   scale for the supplied attribute. If no default scale is given, an error is thrown if an attribute
>   is missing. 

#### <a name="scale-mv-context-with"> Function `scale-mv-context-with` 

Argument List: <tt>([&zwj;ctx & scales])</tt> 

Documentation: 
>   Scales the given many-valued context ctx with the given scales. These are of the form
>   
>     [att_1 att_2 ...] scale,
>   
>   where att_i is an attribute of the given context and scale determines a call to a known
>   scale. The variable «values» will be bound to the corresponding values of each attribute
>   and may be used when constructing the scale. For example, you may use this macro with
>   
>     (scale-mv-context-with ctx
>       [a b c]  (nominal-scale values)
>       [d]      (ordinal-scale values <=)
>       (nominal-scale values))
>   
>   where the last entry (without any associated attribute) is the default scale.  Note that
>   attributes of ctx always have to be given in a sequence, even if there is only one. 

#### <a name="values-of-attribute"> Function `values-of-attribute` 

Argument List: <tt>([&zwj;mv-ctx m])</tt> 

Documentation: 
>   For a given many-valued context mv-ctx and a given attribute m,
>   returns the set of all values of m in mv-ctx. 

#### <a name="values-of-object"> Function `values-of-object` 

Argument List: <tt>([&zwj;mv-ctx g])</tt> 

Documentation: 
>   For a given many-valued context mv-ctx and a given object g,
>   returns the set of all values of g in mv-ctx. 


## <a name="conexp.fca.implications"> Public API of conexp.fca.implications 


  Implications for Formal Concept Analysis.

### Available Functions 

- [`->Implication`](#->Implication)
- [`add-immediate-elements`](#add-immediate-elements)
- [`approx-canonical-base`](#approx-canonical-base)
- [`association-rules`](#association-rules)
- [`canonical-base`](#canonical-base)
- [`canonical-base-from-base`](#canonical-base-from-base)
- [`canonical-base-from-clop`](#canonical-base-from-clop)
- [`clop-by-implications`](#clop-by-implications)
- [`close-under-implications`](#close-under-implications)
- [`close-with-downing-gallier`](#close-with-downing-gallier)
- [`complete-implication-set?`](#complete-implication-set?)
- [`conclusion`](#conclusion)
- [`confidence`](#confidence)
- [`cover`](#cover)
- [`equivalence-oracle-by-implications`](#equivalence-oracle-by-implications)
- [`equivalent-implications?`](#equivalent-implications?)
- [`follows-semantically?`](#follows-semantically?)
- [`follows?`](#follows?)
- [`frequent-closed-itemsets`](#frequent-closed-itemsets)
- [`frequent-itemsets`](#frequent-itemsets)
- [`holds?`](#holds?)
- [`horn1-reduce-implication`](#horn1-reduce-implication)
- [`horn1-refine-implication`](#horn1-refine-implication)
- [`impl`](#impl)
- [`implication-graph`](#implication-graph)
- [`implication?`](#implication?)
- [`intersect-implicational-theories`](#intersect-implicational-theories)
- [`irredundant-subset`](#irredundant-subset)
- [`learn-implications-by-queries`](#learn-implications-by-queries)
- [`luxenburger-base`](#luxenburger-base)
- [`luxenburger-basis`](#luxenburger-basis)
- [`make-implication`](#make-implication)
- [`membership-oracle-by-implications`](#membership-oracle-by-implications)
- [`minimal-implication-set?`](#minimal-implication-set?)
- [`parallel-canonical-base`](#parallel-canonical-base)
- [`parallel-canonical-base-from-clop`](#parallel-canonical-base-from-clop)
- [`premise`](#premise)
- [`proper-conclusion`](#proper-conclusion)
- [`proper-premise-implications`](#proper-premise-implications)
- [`proper-premise?`](#proper-premise?)
- [`proper-premises`](#proper-premises)
- [`proper-premises-by-hypertrans`](#proper-premises-by-hypertrans)
- [`proper-premises-for-attribute`](#proper-premises-for-attribute)
- [`pseudo-clop-by-implications`](#pseudo-clop-by-implications)
- [`pseudo-close-under-implications`](#pseudo-close-under-implications)
- [`pseudo-intents`](#pseudo-intents)
- [`respects?`](#respects?)
- [`ryssel-base`](#ryssel-base)
- [`sound-implication-set?`](#sound-implication-set?)
- [`stem-base`](#stem-base)
- [`stem-base-from-base`](#stem-base-from-base)
- [`support`](#support)
- [`tautology?`](#tautology?)

### Function Documentation 

#### <a name="-&gt;Implication"> Function `->Implication` 

Argument List: <tt>([&zwj;premise conclusion])</tt> 

Documentation: 
>   Positional factory function for class conexp.fca.implications.Implication. 

#### <a name="add-immediate-elements"> Function `add-immediate-elements` 

Argument List: <tt>([&zwj;implications initial-set subset-test])</tt> 

Documentation: 
>   Iterating through the sequence of implications, tries to apply as many
>   implications as possible.  Uses subset-test to determine whether a given
>   implication can be used to extend a given set, i.e. an implication impl can be
>   used to extend a set s if and only if
>   
>     (subset-test (premise impl) s)
>   
>   is true. Note that if (conclusion impl) is already a subset of s, then s is
>   effectively not extended. 

#### <a name="approx-canonical-base"> Function `approx-canonical-base` 

Argument List: <tt>([&zwj;ctx ε δ])</tt> 

Documentation: 
>   Compute a set L of implications that is an approximation to the canonical
>   base of the formal context `ctx'.  More precisely, if H is the canonical base
>   of ctx, then
>   
>     |Mod(L) Δ Mod(H)|/2^{|M|} ≤ ε
>   
>   with probability at least 1-δ.  The computation is done in polynomial time
>   with respect to |M|, |L|, 1/ε, and 1/δ.  

#### <a name="association-rules"> Function `association-rules` 

Argument List: <tt>([&zwj;context minsupp minconf])</tt> 

Documentation: 
>   Returns all association rules of context with the parameters minsupp as
>   minimal support and minconf as minimal confidence. The result returned is a
>   lazy sequence. 

#### <a name="canonical-base"> Function `canonical-base` 

Argument List: <tt>([&zwj;ctx] [&zwj;ctx background-knowledge] [&zwj;ctx background-knowledge predicate])</tt> 

Documentation: 
>   Returns the canonical base of given context, as a lazy sequence.  Uses
>   «background-knowledge» as starting set of implications, which will not appear
>   in the result.  If «predicate» is given (a function), computes only those
>   implications from the canonical base whose premise satisfy this predicate,
>   i.e. «predicate» returns true on these premises.  Note that «predicate» has to
>   satisfy the same conditions as the predicate to «next-closed-set-in-family». 

#### <a name="canonical-base-from-base"> Function `canonical-base-from-base` 

Argument List: <tt>([&zwj;implications])</tt> 

Documentation: 
>   For a given set of implications returns its stem-base. 

#### <a name="canonical-base-from-clop"> Function `canonical-base-from-clop` 

Argument List: <tt>([&zwj;clop base] [&zwj;clop base background-knowledge] [&zwj;clop base background-knowledge predicate])</tt> 

Documentation: 
>   Given a closure operator «clop» on the set «base», computes its canonical base,
>    optionally using the set «background-knowledge» of implications on «base-set»
>   as background knowledge.  The result will be a lazy sequence.  If «predicate»
>   is given as third argument, computes only those implications whose premise
>   satisfy this predicate.  Note that «predicate» has to satisfy the same
>   conditions as the one of «next-closed-set-in-family». 

#### <a name="clop-by-implications"> Function `clop-by-implications` 

Argument List: <tt>([&zwj;implications])</tt> 

Documentation: 
>   Returns closure operator given by implications. 

#### <a name="close-under-implications"> Function `close-under-implications` 

Argument List: <tt>([&zwj;implications input-set])</tt> 

Documentation: 
>   Computes smallest superset of set being closed under given implications. 

#### <a name="close-with-downing-gallier"> Function `close-with-downing-gallier` 

Argument List: <tt>([&zwj;[&zwj;implications in-premise numargs] input-set])</tt> 

Documentation: 
>   Downing-Gallier 

#### <a name="complete-implication-set?"> Function `complete-implication-set?` 

Argument List: <tt>([&zwj;ctx impl-set])</tt> 

Documentation: 
>   Checks wheter given set of implications is complete in context ctx. This is a
>   very costly computation. 

#### <a name="conclusion"> Function `conclusion` 

Argument List: <tt>([&zwj;thing])</tt> 

Documentation: 
>   Returns conclusion of given object. 

#### <a name="confidence"> Function `confidence` 

Argument List: <tt>([&zwj;implication context])</tt> 

Documentation: 
>   Computes the confidence of the given implication in the given context. 

#### <a name="equivalence-oracle-by-implications"> Function `equivalence-oracle-by-implications` 

Argument List: <tt>([&zwj;background-implications])</tt> 

Documentation: 
>   Return a function that can serve as an equivalence oracle for query learning.
>   
>   The returned oracle will return true if a given set S of implications is
>   equivalent to background-implications.  Otherwise, it will return a
>   counterexample, i.e., model of S that is not a model ov
>   background-implications or vice versa. 

#### <a name="equivalent-implications?"> Function `equivalent-implications?` 

Argument List: <tt>([&zwj;impls-1 impls-2])</tt> 

Documentation: 
>   Returns true iff the two seqs of implications are equivalent. 

#### <a name="follows-semantically?"> Function `follows-semantically?` 

Argument List: <tt>([&zwj;implication implications])</tt> 

Documentation: 
>   Returns true iff implication follows semantically from given
>   implications. 

#### <a name="follows?"> Function `follows?` 

Argument List: <tt>([&zwj;implication implications])</tt> 

Documentation: 
>   Returns true iff implication follows semantically from given
>   implications. 

#### <a name="frequent-closed-itemsets"> Function `frequent-closed-itemsets` 

Argument List: <tt>([&zwj;context minsupp])</tt> 

Documentation: 
>   Computes for context a lazy sequence of all frequent and closed itemsets,
>   given minsupp as minimal support. 

#### <a name="frequent-itemsets"> Function `frequent-itemsets` 

Argument List: <tt>([&zwj;context minsupp])</tt> 

Documentation: 
>   Returns all frequent itemsets of context, given minsupp as minimal support. 

#### <a name="holds?"> Function `holds?` 

Argument List: <tt>([&zwj;impl ctx])</tt> 

Documentation: 
>   Returns true iff impl holds in given context ctx. 

#### <a name="impl"> Function `impl` 

Argument List: <tt>([&zwj;& elements])</tt> 

Documentation: 
>   Convenience interface for creating implications.  Write implications just as
>   
>     user=> (impl 1 2 3 ==> 4 5 6)
>     (#{1 2 3} ==> #{4 5 6}) 

#### <a name="implication-graph"> Function `implication-graph` 

Argument List: <tt>([&zwj;implications])</tt> 

Documentation: 
>   Compute setup for Downing-Gallier 

#### <a name="implication?"> Function `implication?` 

Argument List: <tt>([&zwj;thing])</tt> 

Documentation: 
>   Returns true iff thing is an implication. 

#### <a name="intersect-implicational-theories"> Function `intersect-implicational-theories` 

Argument List: <tt>([&zwj;base-set & implication-sets])</tt> 

Documentation: 
>   Given a set «base-set» and collections «implication-sets» of implications,
>   returns the canonical base of the intersection of the corresponding closure
>   theories. 

#### <a name="irredundant-subset"> Function `irredundant-subset` 

Argument List: <tt>([&zwj;impls])</tt> 

Documentation: 
>   Given a set impls of implications, returns an irredundant subset of impls.
>   Note that this set does not need to be of minimal cardinality. 

#### <a name="learn-implications-by-queries"> Function `learn-implications-by-queries` 

Argument List: <tt>([&zwj;base-set member? equivalent?])</tt> 

Documentation: 
>   Learn an implicational theory on base-set with access to membership oracle
>   `member?' and equivalence oracle `equivalent?'.
>   
>   The membership oracle has to decide for a given set S whether S is a model of
>   the background theory to be learned.  The equivalence oracle has to decide
>   whether a given set of implications is equivalent to the background theory.
>   For this it needs to return true if the theories are equivalent, and a
>   counterexample otherwise, i.e., a subset of base-set that is a model of the
>   current hypothesis and not a model of the background theory, or vice versa.
>   
>   This function implements the HORN1 algorithm of Angluin, Frazier, and Pitt:
>   “Learning Conjunctions of Horn Clauses”, 1992. 

#### <a name="luxenburger-base"> Function `luxenburger-base` 

Argument List: <tt>([&zwj;context minsupp-or-predicate minconf])</tt> 

Documentation: 
>   Computes the luxenburger-base of a given context «context», returning the
>   result as a lazy sequence.  Uses «minconf» as minimal confidence.  If
>   «minsupp-or-predicate» is a number, uses that as a minimal support threshold.
>   In this case, «minsupp» ∈ [0,1] must hold.  If «minsupp-or-predicate» is a
>   function, uses this as a predicate to filter all candidate itemsets.  In this
>   case, the predicate should be valid predicate value for «intents». 

#### <a name="luxenburger-basis"> Function `luxenburger-basis` 

Argument List: <tt>([&zwj;context minsupp-or-predicate minconf])</tt> 

Documentation: 
>   Computes the luxenburger-base of a given context «context», returning the
>   result as a lazy sequence.  Uses «minconf» as minimal confidence.  If
>   «minsupp-or-predicate» is a number, uses that as a minimal support threshold.
>   In this case, «minsupp» ∈ [0,1] must hold.  If «minsupp-or-predicate» is a
>   function, uses this as a predicate to filter all candidate itemsets.  In this
>   case, the predicate should be valid predicate value for «intents». 

#### <a name="make-implication"> Function `make-implication` 

Argument List: <tt>([&zwj;premise conclusion])</tt> 

Documentation: 
>   Creates an implication (premise => conclusion \ premise). 

#### <a name="membership-oracle-by-implications"> Function `membership-oracle-by-implications` 

Argument List: <tt>([&zwj;implications])</tt> 

Documentation: 
>   Return a function that can serve as a membership oracle for query learning.
>   
>   The returned oracle will return true if a given set S of elements is a model
>   of implications, and false otherwise. 

#### <a name="minimal-implication-set?"> Function `minimal-implication-set?` 

Argument List: <tt>([&zwj;impl-set])</tt> 

Documentation: 
>   Checks whether given set of implications is minimal, i.e. no
>   implication in this set follows from the others. 

#### <a name="parallel-canonical-base"> Function `parallel-canonical-base` 

Argument List: <tt>([&zwj;ctx] [&zwj;ctx background-knowledge])</tt> 

Documentation: 
>   Computes the canonical base of the given formal context.
>   Background knowledge can be provided as a set of implications on the attribute
>   set of the given context.  Computation is eager and is done in parallel. 

#### <a name="parallel-canonical-base-from-clop"> Function `parallel-canonical-base-from-clop` 

Argument List: <tt>([&zwj;clop base] [&zwj;clop base background-knowledge])</tt> 

Documentation: 
>   Computes the canonical base of the given closure operator in parallel.
>   Accepts the same parameters as «canonical-base-from-clop», except for the
>   predicate. 

#### <a name="premise"> Function `premise` 

Argument List: <tt>([&zwj;thing])</tt> 

Documentation: 
>   Returns premise of given object. 

#### <a name="proper-conclusion"> Function `proper-conclusion` 

Argument List: <tt>([&zwj;ctx A])</tt> 

Documentation: 
>   Returns all elements which are implied in context ctx by A but are neither
>   contained in A or follow from a strict subsets of A. 

#### <a name="proper-premise-implications"> Function `proper-premise-implications` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns all implications based on the proper premises of the
>   context ctx. 

#### <a name="proper-premise?"> Function `proper-premise?` 

Argument List: <tt>([&zwj;ctx A])</tt> 

Documentation: 
>   Returns true iff set A is a subset of the attributes of context ctx
>   and is a proper premise in ctx. 

#### <a name="proper-premises"> Function `proper-premises` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns the proper premises of the given context ctx as a lazy sequence. 

#### <a name="proper-premises-by-hypertrans"> Function `proper-premises-by-hypertrans` 

Argument List: <tt>([&zwj;ctx m objs])</tt> 

Documentation: 
>   Returns all proper premises for the attribute «m» in the formal context
>   «ctx».  The set «objs» should contain all objects from ctx which are in
>   down-arrow relation to m. 

#### <a name="proper-premises-for-attribute"> Function `proper-premises-for-attribute` 

Argument List: <tt>([&zwj;ctx m])</tt> 

Documentation: 
>   Returns all proper premises for the attribute «m» in the formal context «ctx». 

#### <a name="pseudo-clop-by-implications"> Function `pseudo-clop-by-implications` 

Argument List: <tt>([&zwj;implications])</tt> 

Documentation: 
>   Returns for a given set of implications the corresponding closure
>   operator whose closures are all closed and pseudo-closed sets. 

#### <a name="pseudo-close-under-implications"> Function `pseudo-close-under-implications` 

Argument List: <tt>([&zwj;implications set])</tt> 

Documentation: 
>   Computes smallest superset of set being pseudo-closed under given
>   implications. 

#### <a name="pseudo-intents"> Function `pseudo-intents` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns the pseudo intents of the given context ctx. 

#### <a name="respects?"> Function `respects?` 

Argument List: <tt>([&zwj;set impl])</tt> 

Documentation: 
>   Returns true iff set respects given implication impl. 

#### <a name="ryssel-base"> Function `ryssel-base` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns the implications computed by Ryssels Algorithm, as a lazy sequence. 

#### <a name="sound-implication-set?"> Function `sound-implication-set?` 

Argument List: <tt>([&zwj;ctx impl-set])</tt> 

Documentation: 
>   Checks whether given set of implications is sound, i.e. every
>   implication holds in the given context. 

#### <a name="stem-base"> Function `stem-base` 

Argument List: <tt>([&zwj;ctx] [&zwj;ctx background-knowledge] [&zwj;ctx background-knowledge predicate])</tt> 

Documentation: 
>   Returns the canonical base of given context, as a lazy sequence.  Uses
>   «background-knowledge» as starting set of implications, which will not appear
>   in the result.  If «predicate» is given (a function), computes only those
>   implications from the canonical base whose premise satisfy this predicate,
>   i.e. «predicate» returns true on these premises.  Note that «predicate» has to
>   satisfy the same conditions as the predicate to «next-closed-set-in-family». 

#### <a name="stem-base-from-base"> Function `stem-base-from-base` 

Argument List: <tt>([&zwj;implications])</tt> 

Documentation: 
>   For a given set of implications returns its stem-base. 

#### <a name="support"> Function `support` 

Argument List: <tt>([&zwj;thing ctx])</tt> 

Documentation: 
>   Computes the support of the set of attributes B in context ctx. If an
>   implications is given, returns the support of this implication in the given
>   context. 

#### <a name="tautology?"> Function `tautology?` 

Argument List: <tt>([&zwj;impl])</tt> 

Documentation: 
>   Returns true iff impl has empty conclusion. 


## <a name="conexp.fca.exploration"> Public API of conexp.fca.exploration 


  Provides function for exploration and computing proper premises.

### Available Functions 

- [`abortion-sentinal`](#abortion-sentinal)
- [`counterexample-from-expert`](#counterexample-from-expert)
- [`counterexamples-via-repl`](#counterexamples-via-repl)
- [`default-handler-for-complete-counterexamples`](#default-handler-for-complete-counterexamples)
- [`default-handler-for-incomplete-counterexamples`](#default-handler-for-incomplete-counterexamples)
- [`define-repl-fn`](#define-repl-fn)
- [`eval-command`](#eval-command)
- [`examples-by-automorphisms`](#examples-by-automorphisms)
- [`expert-interaction`](#expert-interaction)
- [`explore-attributes`](#explore-attributes)
- [`explore-attributes-with-complete-counterexamples`](#explore-attributes-with-complete-counterexamples)
- [`explore-attributes-with-incomplete-counterexamples`](#explore-attributes-with-incomplete-counterexamples)
- [`falsifies-implication?`](#falsifies-implication?)
- [`help-repl-command`](#help-repl-command)
- [`incomplete-counterexamples-via-repl`](#incomplete-counterexamples-via-repl)
- [`make-handler`](#make-handler)
- [`print-when-there`](#print-when-there)
- [`run-repl-command`](#run-repl-command)
- [`saturate-partial-example`](#saturate-partial-example)
- [`suitable-repl-commands`](#suitable-repl-commands)
- [`valid-counterexample?`](#valid-counterexample?)

### Function Documentation 

#### <a name="counterexamples-via-repl"> Function `counterexamples-via-repl` 

Argument List: <tt>([&zwj;ctx knowledge impl])</tt> 

Documentation: 
>   Starts a repl for counterexamples, which must be specified completely. 

#### <a name="default-handler-for-complete-counterexamples"> Function `default-handler-for-complete-counterexamples` 

Argument List: <tt>([&zwj;ctx known impl])</tt> 

Documentation: 
>   Default handler for attribute exploration. Does it's interaction on the console. 

#### <a name="default-handler-for-incomplete-counterexamples"> Function `default-handler-for-incomplete-counterexamples` 

Argument List: <tt>([&zwj;possible-ctx certain-ctx known impl])</tt> 

Documentation: 
>   Default handler for attribute exploration with incomplete counterexamples. Does it's
>     interaction on the console. 

#### <a name="eval-command"> Function `eval-command` 

Argument List: <tt>([&zwj;query state])</tt> 

Documentation: 
>   Runs the given REPL command query with state, in the case the query uniquely
>   determines a command.  If not, an error message is printed and state is
>   returned. 

#### <a name="examples-by-automorphisms"> Function `examples-by-automorphisms` 

Argument List: <tt>([&zwj;ctx [&zwj;g atts] auts])</tt> 

Documentation: 
>   Generates for the given context ctx and a given example row [g
>   atts] a sequence of new examples (as rows of the same form) by
>   applying the context automorphism in auts. The context automorphisms
>   are applied to the attributes in atts only, the corresponding object
>   will be a newly generated. 

#### <a name="explore-attributes"> Function `explore-attributes` 

Argument List: <tt>([&zwj;& {:keys [&zwj;possible-context certain-context context background-knowledge handler incomplete-counterexamples]}])</tt> 

Documentation: 
>   Performs attribute exploration on the given context(s).  Returns a hashmap of
>   implications computed and the final context, stored with keys :implications
>   and :context (in the case of complete counterexamples)
>   or :possible-context/:certain-context (in the case of incomplete counterexamples),
>   respectively.
>   
>   Arguments are passed as keyword arguments like so
>   
>     (explore-attributes
>       :context ctx-1
>       :handler my-handler)
>   
>     (explore-attributes
>       :incomplete-counterexamples true
>       :possible-context ctx-1
>       :certain-context  ctx-2
>       :handler          my-other-handler
>       :background-knowledge #{})
>   
>   Either a value for :context or values for :possible-context and :certain-context must be
>   given, but not both.  The second option is only possible if :incomplete-counterexamples
>   is set to «true».
>   
>   Optional keyword arguments are:
>   
>   - :handler «fn»
>   
>     Interaction is accomplished via the given handler fn.  Depending on whether incomplete
>     counterexamples are allowed or not, this handler is called with different arguments
>     and is supposed to return different things:
>   
>     - if incomplete counterexamples are not allowed, «fn» is called (in this order) with
>       the current working context, the set of known implications and the current
>       implication to be asked to the expert.  It is supposed to return either «nil» (in
>       which case the implication is accepted), or a sequence of counterexamples which are
>       supposed to be of the form
>   
>         [g attributes]
>   
>       where «g» is the name of a new object and «attributes» is the set of attributes the
>       new object should possess.
>   
>     - if incomplete counterexamples are allowed, «fn» is called with the context of
>       possible incidences, the context of certain incidence, the set of known implications
>       and the current implication to be asked to the expert.  It is supposed to return
>       either «nil» (in which case the implication is accepted), or a sequence of
>       counterexamples of the form
>   
>         [g positive-attributes negative-attributes],
>   
>       where «g» is a new object, «positive-attributes» is a sequence of attributes the new
>       object has, and «negative-attributes» is a sequence of attributes the new object
>       does not have.  Note that «positive-attributes» and «negative-attributes» must be
>       disjoint.
>   
>     Note that it is the responsibility of the handler to ensure that the counterexample is
>     correct.  If this is not the case, the exploration algorithm will just ask the same
>     question in the next iteration again.
>   
>     It is possible to abort the exploration from within a handler.  For this, the handler
>     just has to return :abort.  In this case, the current working context as well as the
>     currently known implications are returned as if the exploration would have been
>     finished in that iteration.
>   
>   - :background-knowledge «set of implications»
>   
>     background-knowledge denotes a set of implications used as background knowledge, which
>     will be subtracted from the computed result.
>   
>   - :incomplete-counterexamples «true or false»
>   
>     Specifies whether incomplete counterexamples are allowed or not.  Default is false.
>     Mandatory to be set to true if context is given via :possible-context
>     and :certain-context.
>   
>   If you want to use automorphisms of the underlying context, you have to construct a
>   special handler using the «make-handler» function. See the corresponding documentation
>   of «make-handler». 

#### <a name="explore-attributes-with-complete-counterexamples"> Function `explore-attributes-with-complete-counterexamples` 

Argument List: <tt>([&zwj;ctx background-knowledge handler])</tt> 

Documentation: 
>   Performs attribute exploration with complete background knowledge 

#### <a name="explore-attributes-with-incomplete-counterexamples"> Function `explore-attributes-with-incomplete-counterexamples` 

Argument List: <tt>([&zwj;possible-ctx certain-ctx background-knowledge handler])</tt> 

Documentation: 
>   Performs attribute exploration allowing for incomplete counterexamples 

#### <a name="falsifies-implication?"> Function `falsifies-implication?` 

Argument List: <tt>([&zwj;new-atts impl])</tt> 

Documentation: 
>   Returns true iff set of new attributes does not respect implication impl. 

#### <a name="help-repl-command"> Function `help-repl-command` 

Argument List: <tt></tt> 

Documentation: 
>   Returns the help string of the given command. 

#### <a name="incomplete-counterexamples-via-repl"> Function `incomplete-counterexamples-via-repl` 

Argument List: <tt>([&zwj;possible-ctx certain-ctx knowledge impl])</tt> 

Documentation: 
>   Starts a repl for counterexamples, which may be incomplete. 

#### <a name="make-handler"> Function `make-handler` 

Argument List: <tt>([&zwj;& {:keys [&zwj;automorphisms incomplete-counterexamples?], :or {automorphisms #{}, incomplete-counterexamples? false}}])</tt> 

Documentation: 
>   Creates a handler for attribute exploration. Valid keys are
>   
>   - automorphisms: A sequence of automorphisms of the overall context,
>     used to construct more examples from a given one.
>   
>     Currently, this has only an effect if the counterexamples are complete.
>   
>   - incomplete-counterexamples?: If true, allows for incomplete counterexamples.  In
>     contrast to the case of complete counterexamples, the function returned takes four
>     arguments (instead of 3), namely the context of the possible incidence, the context of
>     the certain incidence, the known implications as well as the current implication to
>     be asked to the expert. 

#### <a name="run-repl-command"> Function `run-repl-command` 

Argument List: <tt></tt> 

Documentation: 
>   Runs a command for the counterexample REPL. 

#### <a name="saturate-partial-example"> Function `saturate-partial-example` 

Argument List: <tt>([&zwj;impls positives negatives unknown])</tt> 

Documentation: 
>   Saturates the partial example given by positives, negatives and
>   unknown. Uses the set impls of implications for saturation. 

#### <a name="suitable-repl-commands"> Function `suitable-repl-commands` 

Argument List: <tt>([&zwj;query])</tt> 

Documentation: 
>   Returns all known repl commands for query, which can be a symbol or
>   a string. 

#### <a name="valid-counterexample?"> Function `valid-counterexample?` 

Argument List: <tt>([&zwj;state attributes impl])</tt> 

Documentation: 
>   Checks the given example for being valid. 


## <a name="conexp.fca.lattices"> Public API of conexp.fca.lattices 


  Basis datastructure and definitions for abstract lattices.

### Available Functions 

- [`->Lattice`](#->Lattice)
- [`base-set`](#base-set)
- [`concept-lattice`](#concept-lattice)
- [`directly-neighboured?`](#directly-neighboured?)
- [`distributive?`](#distributive?)
- [`dual-lattice`](#dual-lattice)
- [`has-lattice-order?`](#has-lattice-order?)
- [`inf`](#inf)
- [`lattice-atoms`](#lattice-atoms)
- [`lattice-coatoms`](#lattice-coatoms)
- [`lattice-doubly-irreducibles`](#lattice-doubly-irreducibles)
- [`lattice-inf-irreducibles`](#lattice-inf-irreducibles)
- [`lattice-lower-neighbours`](#lattice-lower-neighbours)
- [`lattice-one`](#lattice-one)
- [`lattice-sup-irreducibles`](#lattice-sup-irreducibles)
- [`lattice-upper-neighbours`](#lattice-upper-neighbours)
- [`lattice-zero`](#lattice-zero)
- [`make-lattice`](#make-lattice)
- [`make-lattice-nc`](#make-lattice-nc)
- [`modular?`](#modular?)
- [`order`](#order)
- [`standard-context`](#standard-context)
- [`sup`](#sup)

### Function Documentation 

#### <a name="-&gt;Lattice"> Function `->Lattice` 

Argument List: <tt>([&zwj;base-set order-function inf sup])</tt> 

Documentation: 
>   Positional factory function for class conexp.fca.lattices.Lattice. 

#### <a name="base-set"> Function `base-set` 

Argument List: <tt>([&zwj;lattice])</tt> 

Documentation: 
>   Returns the base set of lattice. 

#### <a name="concept-lattice"> Function `concept-lattice` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns for a given context ctx its concept lattice. 

#### <a name="directly-neighboured?"> Function `directly-neighboured?` 

Argument List: <tt>([&zwj;lat x y])</tt> 

Documentation: 
>   Checks whether x is direct lower neighbour of y in lattice lat. 

#### <a name="distributive?"> Function `distributive?` 

Argument List: <tt>([&zwj;lat])</tt> 

Documentation: 
>   Checks (primitively) whether given lattice lat is distributive or not. 

#### <a name="dual-lattice"> Function `dual-lattice` 

Argument List: <tt>([&zwj;lat])</tt> 

Documentation: 
>   Dualizes given lattice lat. 

#### <a name="has-lattice-order?"> Function `has-lattice-order?` 

Argument List: <tt>([&zwj;lat])</tt> 

Documentation: 
>   Given a lattice checks if its order is indeed a lattice order. 

#### <a name="inf"> Function `inf` 

Argument List: <tt>([&zwj;lattice])</tt> 

Documentation: 
>   Returns a function computing the infimum in lattice. 

#### <a name="lattice-atoms"> Function `lattice-atoms` 

Argument List: <tt>([&zwj;lat])</tt> 

Documentation: 
>   Returns the lattice atoms of lat. 

#### <a name="lattice-coatoms"> Function `lattice-coatoms` 

Argument List: <tt>([&zwj;lat])</tt> 

Documentation: 
>   Returns the lattice coatoms of lat. 

#### <a name="lattice-doubly-irreducibles"> Function `lattice-doubly-irreducibles` 

Argument List: <tt>([&zwj;lat])</tt> 

Documentation: 
>   Returns all (i.e. sup and inf) irreducible elements of lattice lat. 

#### <a name="lattice-inf-irreducibles"> Function `lattice-inf-irreducibles` 

Argument List: <tt>([&zwj;lat])</tt> 

Documentation: 
>   Returns the inf-irreducible elements of lattice lat. 

#### <a name="lattice-lower-neighbours"> Function `lattice-lower-neighbours` 

Argument List: <tt>([&zwj;lat y])</tt> 

Documentation: 
>   Returns all direct lower neighbours of y in lattice lat. 

#### <a name="lattice-one"> Function `lattice-one` 

Argument List: <tt>([&zwj;lat])</tt> 

Documentation: 
>   Returns the one element of lattice lat. 

#### <a name="lattice-sup-irreducibles"> Function `lattice-sup-irreducibles` 

Argument List: <tt>([&zwj;lat])</tt> 

Documentation: 
>   Returns the sup-irreducible elements of lattice lat. 

#### <a name="lattice-upper-neighbours"> Function `lattice-upper-neighbours` 

Argument List: <tt>([&zwj;lat x])</tt> 

Documentation: 
>   Returns all direct upper neighbours of x in lattice lat. 

#### <a name="lattice-zero"> Function `lattice-zero` 

Argument List: <tt>([&zwj;lat])</tt> 

Documentation: 
>   Returns the zero element of lattice lat. 

#### <a name="make-lattice"> Function `make-lattice` 

Argument List: <tt>([&zwj;& args])</tt> 

Documentation: 
>   Standard constructor for makeing lattice. Call with two arguments
>   [base-set order] to construct the lattice by its order
>   relation (given as a set of pairs or as a function of two
>   arguments). Call with three arguments [base-set inf sup] to
>   construct the lattice by its algebraic operations.
>   
>   Note: This function will test the resulting lattice for being one,
>   which may take some time. If you don't want this, use
>   make-lattice-nc. 

#### <a name="make-lattice-nc"> Function `make-lattice-nc` 

Argument List: <tt>([&zwj;base-set order-function] [&zwj;base-set inf sup] [&zwj;base-set order-function inf sup])</tt> 

Documentation: 
>   Creates a new lattice from the given arguments, without any
>   checks. Use with care. 

#### <a name="modular?"> Function `modular?` 

Argument List: <tt>([&zwj;lat])</tt> 

Documentation: 
>   Checks (primitively) whether given lattice lat is modular or not. 

#### <a name="order"> Function `order` 

Argument List: <tt>([&zwj;lattice])</tt> 

Documentation: 
>   Returns a function of one or two arguments representing the order
>   relation. If called with one argument it is assumed that this
>   argument is a pair of elements. 

#### <a name="standard-context"> Function `standard-context` 

Argument List: <tt>([&zwj;lat])</tt> 

Documentation: 
>   Returns the standard context of lattice lat. 

#### <a name="sup"> Function `sup` 

Argument List: <tt>([&zwj;lattice])</tt> 

Documentation: 
>   Returns a function computing the supremum in lattice. 


## <a name="conexp.fca.more"> Public API of conexp.fca.more 


  More on FCA.

### Available Functions 

- [`*fast-computation*`](#*fast-computation*)
- [`all-bonds`](#all-bonds)
- [`all-bonds-by-shared-intents`](#all-bonds-by-shared-intents)
- [`all-shared-intents`](#all-shared-intents)
- [`average-concept-robustness`](#average-concept-robustness)
- [`bond?`](#bond?)
- [`concept-probability`](#concept-probability)
- [`concept-robustness`](#concept-robustness)
- [`concept-robustness-add-next-entry`](#concept-robustness-add-next-entry)
- [`concept-robustness-polynomial`](#concept-robustness-polynomial)
- [`concept-stability`](#concept-stability)
- [`context-from-clop`](#context-from-clop)
- [`implication-context`](#implication-context)
- [`jaccard-index`](#jaccard-index)
- [`maximal-counterexample`](#maximal-counterexample)
- [`next-shared-intent`](#next-shared-intent)
- [`smallest-bond`](#smallest-bond)
- [`sorensen-coefficient`](#sorensen-coefficient)
- [`weighted-concept-similarity`](#weighted-concept-similarity)

### Function Documentation 

#### <a name="*fast-computation*"> Function `*fast-computation*` 

Argument List: <tt></tt> 

Documentation: 
>   Enable computation of concept probability with floating point arithmetic
>   instead of rationals 

#### <a name="all-bonds"> Function `all-bonds` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   Returns all bonds between ctx-1 and ctx-2. 

#### <a name="all-bonds-by-shared-intents"> Function `all-bonds-by-shared-intents` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   All bonds between ctx-1 and ctx-2, computed using shared intents. 

#### <a name="all-shared-intents"> Function `all-shared-intents` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   All intents shared between contexts ctx-1 and ctx-2. ctx-1 and
>   ctx-2 must have the same attribute set. 

#### <a name="average-concept-robustness"> Function `average-concept-robustness` 

Argument List: <tt>([&zwj;concepts alpha])</tt> 

Documentation: 
>   Takes the seq `concepts' consisting of all concepts of a context and computes
>   the average concept robustness with parmater `alpha'. 

#### <a name="bond?"> Function `bond?` 

Argument List: <tt>([&zwj;ctx-1 ctx-2 ctx])</tt> 

Documentation: 
>   Checks whether context ctx is a bond between ctx-1 and ctx-2. 

#### <a name="concept-probability"> Function `concept-probability` 

Argument List: <tt>([&zwj;context concept])</tt> 

Documentation: 
>   Compute the probability of a `concept' in `context' 𝕂 in the following manner.
>   Let pₘ be the relative frequence of attribute m in context.  The
>   probability of a subset B ⊆ M in 𝕂 is the product of all pₘ for m ∈ B.
>   Then the probability of a concept is defined by pr(A,B) := pr(B=B'')
>   which is ∑_{k=0}^n {n choose k}·p_Bᵏ·(1-p_B)ⁿ⁻ᵏ·∏_{m ∈ M ∖ B}(1-p_mᵏ). 

#### <a name="concept-robustness"> Function `concept-robustness` 

Argument List: <tt>([&zwj;concept concepts alpha sorted?] [&zwj;concept concepts alpha])</tt> 

Documentation: 
>   Computes the robustness of a `concept' in a context with parameter `alpha' by
>   using the seq `concepts' consisting of all concepts of the context.  The
>   optional boolean parameter `sorted?' allows to declare the seq of concepts as
>   beeing already sorted by increasing size of the attribute set.  This function
>   uses the function concept-robustness-polynomial. 

#### <a name="concept-robustness-add-next-entry"> Function `concept-robustness-add-next-entry` 

Argument List: <tt>([&zwj;concept cache])</tt> 

Documentation: 
>   Helper-function for `concept-robustness-polynomial'.
>   
>   This function computes the value e(Y,`concept'), based on the already computed
>   values e(Z,`concept'), which are given in the second parameter `cache' in the
>   form [Z, e(Z, `concept')].
>   
>   This function is needed in the algorithm on page 19 in "Finding Robust
>   Itemsets under Subsampling" from Tatti, Moerchen, and Calders. 

#### <a name="concept-robustness-polynomial"> Function `concept-robustness-polynomial` 

Argument List: <tt>([&zwj;concept concepts sorted?] [&zwj;concept concepts])</tt> 

Documentation: 
>   Return the coefficients of the robustness polynomial of `concept'.
>   
>   For the given `concept' of a context, the coefficients of the polynomial p
>   corresponding to the robustness is computed by using the seq `concepts' of
>   all concepts of the context.  The optional boolean parameter `sorted?' allows
>   to declare the seq of concepts as being already sorted by increasing
>   attribute-set.  Thus if v is the result of (robustness-polynomial concept
>   concepts), then (eval-polynomial v (- 1 alpha)) computes the robustness with
>   parameter alpha.
>   
>   For details see "Finding Robust Itemsets under Subsampling" from Tatti,
>   Moerchen, and Calders, pages 17–19. 

#### <a name="concept-stability"> Function `concept-stability` 

Argument List: <tt>([&zwj;context concept])</tt> 

Documentation: 
>   Compute the concept stability of `concept' in `context'. 

#### <a name="context-from-clop"> Function `context-from-clop` 

Argument List: <tt>([&zwj;base-set clop])</tt> 

Documentation: 
>   Returns a context whose intents are exactly the closed sets of the
>   given closure operator on the given base-set. 

#### <a name="implication-context"> Function `implication-context` 

Argument List: <tt>([&zwj;n])</tt> 

Documentation: 
>   Returns a context for a non-negative number n which has as it's
>   extents the closure systems on the set {0 .. (n-1)} and as it's
>   intents the corresponding implicational theory. 

#### <a name="jaccard-index"> Function `jaccard-index` 

Argument List: <tt>([&zwj;x y])</tt> 

Documentation: 
>   Computes the Jaccard index of two sets. This is |x ∩ y| / |x ∪ y|.
>   Returns 1 if both sets are empty. 

#### <a name="maximal-counterexample"> Function `maximal-counterexample` 

Argument List: <tt>([&zwj;c base-set B C])</tt> 

Documentation: 
>   For a given closure operator c on a set base-set, maximizes the set
>   C (i.e. returns a superset of C) that is not a superset of
>   B (i.e. there exists an element m in B without C that is also not in
>   the result. 

#### <a name="next-shared-intent"> Function `next-shared-intent` 

Argument List: <tt>([&zwj;ctx-1 ctx-2 b])</tt> 

Documentation: 
>   The smallest shared intent of contexts ctx-1 and ctx-2 containing b. 

#### <a name="smallest-bond"> Function `smallest-bond` 

Argument List: <tt>([&zwj;ctx-1 ctx-2 rel])</tt> 

Documentation: 
>   Returns the smallest bond between ctx-1 and ctx-2 that has the
>   elements of rel as crosses. 

#### <a name="sorensen-coefficient"> Function `sorensen-coefficient` 

Argument List: <tt>([&zwj;x y])</tt> 

Documentation: 
>   Computes the Sorensen coefficient of two sets.
>   This is 2 * |x ∩ y| / (|x| + |y|).
>   Returns 1 if both sets are empty. 

#### <a name="weighted-concept-similarity"> Function `weighted-concept-similarity` 

Argument List: <tt>([&zwj;sim [&zwj;c1 c2]] [&zwj;sim [&zwj;c1 c2] w])</tt> 

Documentation: 
>   Computes a weighted concept similarity for a given similatity measure `sim',
>   two concepts [`c1' `c2'] and an optional weight `w' (default is 1/2).
>   
>   That is the weighted average of the similarity of the extents/object sets
>   (weight `w') and the intents/attribute sets (weight 1-`w').
>   
>   This is from Alqadah, F. & Bhatnagar, R. (2011), 'Similarity measures in
>   formal concept analysis.', Ann. Math. Artif. Intell. 61 (3), 249,
>   https://doi.org/10.1007/s10472-011-9257-7 


## <a name="conexp.io.latex"> Public API of conexp.io.latex 


  Provides functionality to represent conexp-clj datastructures as latex code.

### Available Functions 

- [`latex`](#latex)
- [`layout->tikz`](#layout->tikz)
- [`tex-escape`](#tex-escape)

### Function Documentation 

#### <a name="latex"> Function `latex` 

Argument List: <tt>([&zwj;this] [&zwj;this choice])</tt> 

Documentation: 
>   Returns a string representation of this. 

#### <a name="tex-escape"> Function `tex-escape` 

Argument List: <tt>([&zwj;string])</tt> 

Documentation: 
>   Escapes all significant characters used by LaTeX. 


## <a name="conexp.io.contexts"> Public API of conexp.io.contexts 


### Available Functions 

- [`add-context-input-format`](#add-context-input-format)
- [`ctx->xml-vector`](#ctx->xml-vector)
- [`define-context-input-format`](#define-context-input-format)
- [`define-context-output-format`](#define-context-output-format)
- [`find-context-input-format`](#find-context-input-format)
- [`find-tag`](#find-tag)
- [`find-tags`](#find-tags)
- [`get-default-context-format`](#get-default-context-format)
- [`get-known-context-input-formats`](#get-known-context-input-formats)
- [`hash-from-pairs`](#hash-from-pairs)
- [`list-context-formats`](#list-context-formats)
- [`list-context-input-formats`](#list-context-input-formats)
- [`list-context-output-formats`](#list-context-output-formats)
- [`read-context`](#read-context)
- [`set-default-context-format!`](#set-default-context-format!)
- [`trim`](#trim)
- [`write-context`](#write-context)

### Function Documentation 

#### <a name="define-context-input-format"> Function `define-context-input-format` 

Argument List: <tt>([&zwj;input-format [&zwj;file] & body])</tt> 

Documentation: 
>   Defines input format for contexts. 

#### <a name="define-context-output-format"> Function `define-context-output-format` 

Argument List: <tt>([&zwj;input-format [&zwj;thing file] & body])</tt> 

Documentation: 
>   Defines output format for contexts. 

#### <a name="find-context-input-format"> Function `find-context-input-format` 

Argument List: <tt>([&zwj;file__7379__auto__] [&zwj;file__7379__auto__ format__7380__auto__])</tt> 

Documentation: 
>   Tries to determine the format used in file. 

#### <a name="get-default-context-format"> Function `get-default-context-format` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns default write format for contexts. 

#### <a name="list-context-formats"> Function `list-context-formats` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns a list of known context IO formats, i.e. formats for which reading and writing is defined. 

#### <a name="list-context-input-formats"> Function `list-context-input-formats` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns a list of known context input formats 

#### <a name="list-context-output-formats"> Function `list-context-output-formats` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns a list of known context input formats 

#### <a name="read-context"> Function `read-context` 

Argument List: <tt>([&zwj;file] [&zwj;file explicit-format])</tt> 

Documentation: 
>   Reads context from file, automatically determining the format used. 

#### <a name="set-default-context-format!"> Function `set-default-context-format!` 

Argument List: <tt>([&zwj;format__7380__auto__])</tt> 

Documentation: 
>   Sets default write format for contexts to format. 

#### <a name="write-context"> Function `write-context` 

Argument List: <tt>([&zwj;format context file] [&zwj;context file])</tt> 

Documentation: 
>   Writes context to file using format. 


## <a name="conexp.io.lattices"> Public API of conexp.io.lattices 


### Available Functions 

- [`add-lattice-input-format`](#add-lattice-input-format)
- [`define-lattice-input-format`](#define-lattice-input-format)
- [`define-lattice-output-format`](#define-lattice-output-format)
- [`find-lattice-input-format`](#find-lattice-input-format)
- [`get-default-lattice-format`](#get-default-lattice-format)
- [`get-known-lattice-input-formats`](#get-known-lattice-input-formats)
- [`list-lattice-formats`](#list-lattice-formats)
- [`list-lattice-input-formats`](#list-lattice-input-formats)
- [`list-lattice-output-formats`](#list-lattice-output-formats)
- [`read-lattice`](#read-lattice)
- [`set-default-lattice-format!`](#set-default-lattice-format!)
- [`write-lattice`](#write-lattice)

### Function Documentation 

#### <a name="define-lattice-input-format"> Function `define-lattice-input-format` 

Argument List: <tt>([&zwj;input-format [&zwj;file] & body])</tt> 

Documentation: 
>   Defines input format for lattices. 

#### <a name="define-lattice-output-format"> Function `define-lattice-output-format` 

Argument List: <tt>([&zwj;input-format [&zwj;thing file] & body])</tt> 

Documentation: 
>   Defines output format for lattices. 

#### <a name="find-lattice-input-format"> Function `find-lattice-input-format` 

Argument List: <tt>([&zwj;file__7379__auto__] [&zwj;file__7379__auto__ format__7380__auto__])</tt> 

Documentation: 
>   Tries to determine the format used in file. 

#### <a name="get-default-lattice-format"> Function `get-default-lattice-format` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns default write format for lattices. 

#### <a name="list-lattice-formats"> Function `list-lattice-formats` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns a list of known lattice IO formats, i.e. formats for which reading and writing is defined. 

#### <a name="list-lattice-input-formats"> Function `list-lattice-input-formats` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns a list of known lattice input formats 

#### <a name="list-lattice-output-formats"> Function `list-lattice-output-formats` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns a list of known lattice input formats 

#### <a name="read-lattice"> Function `read-lattice` 

Argument List: <tt>([&zwj;file] [&zwj;file explicit-format])</tt> 

Documentation: 
>   Reads lattice from file, automatically determining the format used. 

#### <a name="set-default-lattice-format!"> Function `set-default-lattice-format!` 

Argument List: <tt>([&zwj;format__7380__auto__])</tt> 

Documentation: 
>   Sets default write format for lattices to format. 

#### <a name="write-lattice"> Function `write-lattice` 

Argument List: <tt>([&zwj;format lattice file] [&zwj;lattice file])</tt> 

Documentation: 
>   Writes lattice to file using format. 


## <a name="conexp.io.layouts"> Public API of conexp.io.layouts 


  Implements IO for layouts.

### Available Functions 

- [`add-layout-input-format`](#add-layout-input-format)
- [`define-layout-input-format`](#define-layout-input-format)
- [`define-layout-output-format`](#define-layout-output-format)
- [`find-layout-input-format`](#find-layout-input-format)
- [`get-arguments`](#get-arguments)
- [`get-default-layout-format`](#get-default-layout-format)
- [`get-known-layout-input-formats`](#get-known-layout-input-formats)
- [`list-layout-formats`](#list-layout-formats)
- [`list-layout-input-formats`](#list-layout-input-formats)
- [`list-layout-output-formats`](#list-layout-output-formats)
- [`read-layout`](#read-layout)
- [`seq-positions`](#seq-positions)
- [`set-default-layout-format!`](#set-default-layout-format!)
- [`write-layout`](#write-layout)

### Function Documentation 

#### <a name="define-layout-input-format"> Function `define-layout-input-format` 

Argument List: <tt>([&zwj;input-format [&zwj;file] & body])</tt> 

Documentation: 
>   Defines input format for layouts. 

#### <a name="define-layout-output-format"> Function `define-layout-output-format` 

Argument List: <tt>([&zwj;input-format [&zwj;thing file] & body])</tt> 

Documentation: 
>   Defines output format for layouts. 

#### <a name="find-layout-input-format"> Function `find-layout-input-format` 

Argument List: <tt>([&zwj;file__7379__auto__] [&zwj;file__7379__auto__ format__7380__auto__])</tt> 

Documentation: 
>   Tries to determine the format used in file. 

#### <a name="get-default-layout-format"> Function `get-default-layout-format` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns default write format for layouts. 

#### <a name="list-layout-formats"> Function `list-layout-formats` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns a list of known layout IO formats, i.e. formats for which reading and writing is defined. 

#### <a name="list-layout-input-formats"> Function `list-layout-input-formats` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns a list of known layout input formats 

#### <a name="list-layout-output-formats"> Function `list-layout-output-formats` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns a list of known layout input formats 

#### <a name="read-layout"> Function `read-layout` 

Argument List: <tt>([&zwj;file] [&zwj;file explicit-format])</tt> 

Documentation: 
>   Reads layout from file, automatically determining the format used. 

#### <a name="seq-positions"> Function `seq-positions` 

Argument List: <tt>([&zwj;seq])</tt> 

Documentation: 
>   Returns a map from elements of seq to their positions. 

#### <a name="set-default-layout-format!"> Function `set-default-layout-format!` 

Argument List: <tt>([&zwj;format__7380__auto__])</tt> 

Documentation: 
>   Sets default write format for layouts to format. 

#### <a name="write-layout"> Function `write-layout` 

Argument List: <tt>([&zwj;format layout file] [&zwj;layout file])</tt> 

Documentation: 
>   Writes layout to file using format. 


## <a name="conexp.io.many-valued-contexts"> Public API of conexp.io.many-valued-contexts 


  Implements IO for Many-Valued Contexts.

### Available Functions 

- [`add-mv-context-input-format`](#add-mv-context-input-format)
- [`define-mv-context-input-format`](#define-mv-context-input-format)
- [`define-mv-context-output-format`](#define-mv-context-output-format)
- [`find-mv-context-input-format`](#find-mv-context-input-format)
- [`get-default-mv-context-format`](#get-default-mv-context-format)
- [`get-known-mv-context-input-formats`](#get-known-mv-context-input-formats)
- [`list-mv-context-formats`](#list-mv-context-formats)
- [`list-mv-context-input-formats`](#list-mv-context-input-formats)
- [`list-mv-context-output-formats`](#list-mv-context-output-formats)
- [`read-many-valued-context`](#read-many-valued-context)
- [`read-mv-context`](#read-mv-context)
- [`set-default-mv-context-format!`](#set-default-mv-context-format!)
- [`write-many-valued-context`](#write-many-valued-context)
- [`write-mv-context`](#write-mv-context)

### Function Documentation 

#### <a name="define-mv-context-input-format"> Function `define-mv-context-input-format` 

Argument List: <tt>([&zwj;input-format [&zwj;file] & body])</tt> 

Documentation: 
>   Defines input format for mv-contexts. 

#### <a name="define-mv-context-output-format"> Function `define-mv-context-output-format` 

Argument List: <tt>([&zwj;input-format [&zwj;thing file] & body])</tt> 

Documentation: 
>   Defines output format for mv-contexts. 

#### <a name="find-mv-context-input-format"> Function `find-mv-context-input-format` 

Argument List: <tt>([&zwj;file__7379__auto__] [&zwj;file__7379__auto__ format__7380__auto__])</tt> 

Documentation: 
>   Tries to determine the format used in file. 

#### <a name="get-default-mv-context-format"> Function `get-default-mv-context-format` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns default write format for mv-contexts. 

#### <a name="list-mv-context-formats"> Function `list-mv-context-formats` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns a list of known mv-context IO formats, i.e. formats for which reading and writing is defined. 

#### <a name="list-mv-context-input-formats"> Function `list-mv-context-input-formats` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns a list of known mv-context input formats 

#### <a name="list-mv-context-output-formats"> Function `list-mv-context-output-formats` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns a list of known mv-context input formats 

#### <a name="read-many-valued-context"> Function `read-many-valued-context` 

Argument List: <tt>([&zwj;file] [&zwj;file explicit-format])</tt> 

Documentation: 
>   Reads mv-context from file, automatically determining the format used. 

#### <a name="read-mv-context"> Function `read-mv-context` 

Argument List: <tt>([&zwj;file] [&zwj;file explicit-format])</tt> 

Documentation: 
>   Reads mv-context from file, automatically determining the format used. 

#### <a name="set-default-mv-context-format!"> Function `set-default-mv-context-format!` 

Argument List: <tt>([&zwj;format__7380__auto__])</tt> 

Documentation: 
>   Sets default write format for mv-contexts to format. 

#### <a name="write-many-valued-context"> Function `write-many-valued-context` 

Argument List: <tt>([&zwj;format mv-context file] [&zwj;mv-context file])</tt> 

Documentation: 
>   Writes mv-context to file using format. 

#### <a name="write-mv-context"> Function `write-mv-context` 

Argument List: <tt>([&zwj;format mv-context file] [&zwj;mv-context file])</tt> 

Documentation: 
>   Writes mv-context to file using format. 


## <a name="conexp.layouts"> Public API of conexp.layouts 


### Available Functions 

- [`inf-additive-layout`](#inf-additive-layout)
- [`standard-layout`](#standard-layout)

### Function Documentation 

#### <a name="inf-additive-layout"> Function `inf-additive-layout` 

Argument List: <tt>([&zwj;lattice])</tt> 

Documentation: 
>   Returns an infimum additive layout for lattice. 

#### <a name="standard-layout"> Function `standard-layout` 

Argument List: <tt></tt> 

Documentation: 
>   Standard layout function. Call on a lattice to get a layout. 
