
# API Documentation

 - Namespace [`conexp.main`](#conexp.main)

 - Namespace [`conexp.layouts`](#conexp.layouts)

 - Namespace [`conexp.layouts.base`](#conexp.layouts.base)

 - Namespace [`conexp.layouts.common`](#conexp.layouts.common)

 - Namespace [`conexp.layouts.force`](#conexp.layouts.force)

 - Namespace [`conexp.layouts.freese`](#conexp.layouts.freese)

 - Namespace [`conexp.layouts.layered`](#conexp.layouts.layered)

 - Namespace [`conexp.layouts.util`](#conexp.layouts.util)

 - Namespace [`conexp.contrib.concept-approximation`](#conexp.contrib.concept-approximation)

 - Namespace [`conexp.contrib.doc`](#conexp.contrib.doc)

 - Namespace [`conexp.contrib.fuzzy.fca`](#conexp.contrib.fuzzy.fca)

 - Namespace [`conexp.contrib.fuzzy.logics`](#conexp.contrib.fuzzy.logics)

 - Namespace [`conexp.contrib.fuzzy.sets`](#conexp.contrib.fuzzy.sets)

 - Namespace [`conexp.contrib.draw`](#conexp.contrib.draw)

 - Namespace [`conexp.contrib.exec`](#conexp.contrib.exec)

 - Namespace [`conexp.contrib.factor-analysis`](#conexp.contrib.factor-analysis)

 - Namespace [`conexp.contrib.gui`](#conexp.contrib.gui)

 - Namespace [`conexp.contrib.java`](#conexp.contrib.java)

 - Namespace [`conexp.contrib.nonsense`](#conexp.contrib.nonsense)

 - Namespace [`conexp.contrib.profiler`](#conexp.contrib.profiler)

 - Namespace [`conexp.contrib.retracts`](#conexp.contrib.retracts)

## <a name="conexp.main"> Public API of conexp.main 


  Main namespace for conexp-clj. Immigrates all needed namespaces.

### Available Functions 

- [`->Formal-Context`](#->Formal-Context)
- [`->Implication`](#->Implication)
- [`->Lattice`](#->Lattice)
- [`->Many-Valued-Context`](#->Many-Valued-Context)
- [`-?>`](#-?>)
- [`-?>>`](#-?>>)
- [`.?.`](#.?.)
- [`<=>`](#<=>)
- [`=>`](#=>)
- [`abs`](#abs)
- [`adiag-context`](#adiag-context)
- [`adprime`](#adprime)
- [`all-bonds`](#all-bonds)
- [`all-bonds-by-shared-intents`](#all-bonds-by-shared-intents)
- [`all-closed-sets`](#all-closed-sets)
- [`all-closed-sets-in-family`](#all-closed-sets-in-family)
- [`all-shared-intents`](#all-shared-intents)
- [`aprime`](#aprime)
- [`ask`](#ask)
- [`attribute-clarified?`](#attribute-clarified?)
- [`attribute-concept`](#attribute-concept)
- [`attribute-derivation`](#attribute-derivation)
- [`attribute-reduced?`](#attribute-reduced?)
- [`attributes`](#attributes)
- [`available-formats`](#available-formats)
- [`base-set`](#base-set)
- [`biordinal-scale`](#biordinal-scale)
- [`bond?`](#bond?)
- [`canonical-base`](#canonical-base)
- [`canonical-base-from-base`](#canonical-base-from-base)
- [`canonical-base-from-clop`](#canonical-base-from-clop)
- [`ceil`](#ceil)
- [`clarify-attributes`](#clarify-attributes)
- [`clarify-context`](#clarify-context)
- [`clarify-objects`](#clarify-objects)
- [`clojure-coll`](#clojure-coll)
- [`clojure-fn`](#clojure-fn)
- [`clojure-map`](#clojure-map)
- [`clojure-seq`](#clojure-seq)
- [`clojure-set`](#clojure-set)
- [`clojure-type`](#clojure-type)
- [`clojure-vec`](#clojure-vec)
- [`clop-by-implications`](#clop-by-implications)
- [`close-under-implications`](#close-under-implications)
- [`close-under-implications-1`](#close-under-implications-1)
- [`compatible-subcontext?`](#compatible-subcontext?)
- [`compatible-subcontexts`](#compatible-subcontexts)
- [`complete-implication-set?`](#complete-implication-set?)
- [`concept-lattice`](#concept-lattice)
- [`concept?`](#concept?)
- [`concepts`](#concepts)
- [`conclusion`](#conclusion)
- [`conexp-version`](#conexp-version)
- [`confidence`](#confidence)
- [`context-apposition`](#context-apposition)
- [`context-attribute-closure`](#context-attribute-closure)
- [`context-clarified?`](#context-clarified?)
- [`context-composition`](#context-composition)
- [`context-disjoint-union`](#context-disjoint-union)
- [`context-from-clop`](#context-from-clop)
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
- [`cross-product`](#cross-product)
- [`def-`](#def-)
- [`defalias`](#defalias)
- [`default-handler-for-complete-counterexamples`](#default-handler-for-complete-counterexamples)
- [`default-handler-for-incomplete-counterexamples`](#default-handler-for-incomplete-counterexamples)
- [`define-context-input-format`](#define-context-input-format)
- [`define-context-output-format`](#define-context-output-format)
- [`define-lattice-input-format`](#define-lattice-input-format)
- [`define-lattice-output-format`](#define-lattice-output-format)
- [`define-layout-input-format`](#define-layout-input-format)
- [`define-layout-output-format`](#define-layout-output-format)
- [`define-mv-context-input-format`](#define-mv-context-input-format)
- [`define-mv-context-output-format`](#define-mv-context-output-format)
- [`defmacro-`](#defmacro-)
- [`diag-context`](#diag-context)
- [`dichotomic-scale`](#dichotomic-scale)
- [`die-with-error`](#die-with-error)
- [`difference`](#difference)
- [`direct-lower-concepts`](#direct-lower-concepts)
- [`direct-upper-concepts`](#direct-upper-concepts)
- [`directly-neighboured?`](#directly-neighboured?)
- [`disjoint-union`](#disjoint-union)
- [`dissoc-in`](#dissoc-in)
- [`distinct-by-key`](#distinct-by-key)
- [`distributive?`](#distributive?)
- [`div`](#div)
- [`down-arrows`](#down-arrows)
- [`dual-context`](#dual-context)
- [`dual-lattice`](#dual-lattice)
- [`ensure-length`](#ensure-length)
- [`ensure-seq`](#ensure-seq)
- [`equivalent-implications?`](#equivalent-implications?)
- [`exact-integer-sqrt`](#exact-integer-sqrt)
- [`exists`](#exists)
- [`explore-attributes`](#explore-attributes)
- [`expt`](#expt)
- [`extents`](#extents)
- [`first-non-nil`](#first-non-nil)
- [`floor`](#floor)
- [`follows-semantically?`](#follows-semantically?)
- [`follows?`](#follows?)
- [`forall`](#forall)
- [`frequent-closed-itemsets`](#frequent-closed-itemsets)
- [`gcd`](#gcd)
- [`generic-equals`](#generic-equals)
- [`get-default-context-format`](#get-default-context-format)
- [`get-default-lattice-format`](#get-default-lattice-format)
- [`get-default-layout-format`](#get-default-layout-format)
- [`get-default-mv-context-format`](#get-default-mv-context-format)
- [`graph-of-function?`](#graph-of-function?)
- [`has-lattice-order?`](#has-lattice-order?)
- [`has-version?`](#has-version?)
- [`hash-combine-hash`](#hash-combine-hash)
- [`holds?`](#holds?)
- [`illegal-argument`](#illegal-argument)
- [`illegal-state`](#illegal-state)
- [`immigrate`](#immigrate)
- [`impl`](#impl)
- [`implication-context`](#implication-context)
- [`implication?`](#implication?)
- [`improve-basic-order`](#improve-basic-order)
- [`incidence`](#incidence)
- [`incident?`](#incident?)
- [`index`](#index)
- [`inf`](#inf)
- [`inf-additive-layout`](#inf-additive-layout)
- [`inits`](#inits)
- [`integer-length`](#integer-length)
- [`intents`](#intents)
- [`interordinal-scale`](#interordinal-scale)
- [`intersect-implicational-theories`](#intersect-implicational-theories)
- [`intersection`](#intersection)
- [`interval-scale`](#interval-scale)
- [`invert-context`](#invert-context)
- [`irredundant-subset`](#irredundant-subset)
- [`join`](#join)
- [`latex`](#latex)
- [`lattice-atoms`](#lattice-atoms)
- [`lattice-coatoms`](#lattice-coatoms)
- [`lattice-doubly-irreducibles`](#lattice-doubly-irreducibles)
- [`lattice-inf-irreducibles`](#lattice-inf-irreducibles)
- [`lattice-lower-neighbours`](#lattice-lower-neighbours)
- [`lattice-one`](#lattice-one)
- [`lattice-sup-irreducibles`](#lattice-sup-irreducibles)
- [`lattice-upper-neighbours`](#lattice-upper-neighbours)
- [`lattice-zero`](#lattice-zero)
- [`lcm`](#lcm)
- [`lectic-<`](#lectic-<)
- [`lectic-<_i`](#lectic-<_i)
- [`list-context-formats`](#list-context-formats)
- [`list-context-input-formats`](#list-context-input-formats)
- [`list-context-output-formats`](#list-context-output-formats)
- [`list-lattice-formats`](#list-lattice-formats)
- [`list-lattice-input-formats`](#list-lattice-input-formats)
- [`list-lattice-output-formats`](#list-lattice-output-formats)
- [`list-layout-formats`](#list-layout-formats)
- [`list-layout-input-formats`](#list-layout-input-formats)
- [`list-layout-output-formats`](#list-layout-output-formats)
- [`list-mv-context-formats`](#list-mv-context-formats)
- [`list-mv-context-input-formats`](#list-mv-context-input-formats)
- [`list-mv-context-output-formats`](#list-mv-context-output-formats)
- [`luxenburger-base`](#luxenburger-base)
- [`luxenburger-basis`](#luxenburger-basis)
- [`make-context`](#make-context)
- [`make-context-from-matrix`](#make-context-from-matrix)
- [`make-context-nc`](#make-context-nc)
- [`make-handler`](#make-handler)
- [`make-implication`](#make-implication)
- [`make-lattice`](#make-lattice)
- [`make-lattice-nc`](#make-lattice-nc)
- [`make-mv-context`](#make-mv-context)
- [`make-mv-context-from-matrix`](#make-mv-context-from-matrix)
- [`make-mv-context-nc`](#make-mv-context-nc)
- [`map-by-fn`](#map-by-fn)
- [`map-invert`](#map-invert)
- [`memo-fn`](#memo-fn)
- [`minimal-generating-subsets`](#minimal-generating-subsets)
- [`minimal-hypergraph-transversals`](#minimal-hypergraph-transversals)
- [`minimal-implication-set?`](#minimal-implication-set?)
- [`minimum-set-covers`](#minimum-set-covers)
- [`modular?`](#modular?)
- [`mv-context-to-string`](#mv-context-to-string)
- [`new-by-name`](#new-by-name)
- [`next-closed-set`](#next-closed-set)
- [`next-closed-set-in-family`](#next-closed-set-in-family)
- [`nominal-scale`](#nominal-scale)
- [`not-yet-implemented`](#not-yet-implemented)
- [`now`](#now)
- [`null-context`](#null-context)
- [`object-clarified?`](#object-clarified?)
- [`object-concept`](#object-concept)
- [`object-derivation`](#object-derivation)
- [`object-reduced?`](#object-reduced?)
- [`objects`](#objects)
- [`odprime`](#odprime)
- [`one-context`](#one-context)
- [`oprime`](#oprime)
- [`order`](#order)
- [`order-by`](#order-by)
- [`ordinal-scale`](#ordinal-scale)
- [`partial-max`](#partial-max)
- [`partial-min`](#partial-min)
- [`premise`](#premise)
- [`print-context`](#print-context)
- [`prod`](#prod)
- [`project`](#project)
- [`proper-conclusion`](#proper-conclusion)
- [`proper-premise-implications`](#proper-premise-implications)
- [`proper-premise?`](#proper-premise?)
- [`proper-premises`](#proper-premises)
- [`proper-premises-for-attribute`](#proper-premises-for-attribute)
- [`proper-subset?`](#proper-subset?)
- [`proper-superset?`](#proper-superset?)
- [`pseudo-clop-by-implications`](#pseudo-clop-by-implications)
- [`pseudo-close-under-implications`](#pseudo-close-under-implications)
- [`pseudo-intents`](#pseudo-intents)
- [`quit`](#quit)
- [`rand-context`](#rand-context)
- [`random-context`](#random-context)
- [`random-contexts`](#random-contexts)
- [`read-context`](#read-context)
- [`read-lattice`](#read-lattice)
- [`read-layout`](#read-layout)
- [`read-mv-context`](#read-mv-context)
- [`reduce!`](#reduce!)
- [`reduce-attributes`](#reduce-attributes)
- [`reduce-context`](#reduce-context)
- [`reduce-objects`](#reduce-objects)
- [`reflexive-transitive-closure`](#reflexive-transitive-closure)
- [`rename`](#rename)
- [`rename-attributes`](#rename-attributes)
- [`rename-keys`](#rename-keys)
- [`rename-objects`](#rename-objects)
- [`respects?`](#respects?)
- [`restrict-concept`](#restrict-concept)
- [`round`](#round)
- [`ryssel-base`](#ryssel-base)
- [`scale-mv-context`](#scale-mv-context)
- [`scale-mv-context-with`](#scale-mv-context-with)
- [`select`](#select)
- [`seqable?`](#seqable?)
- [`set-default-context-format!`](#set-default-context-format!)
- [`set-default-lattice-format!`](#set-default-lattice-format!)
- [`set-default-layout-format!`](#set-default-layout-format!)
- [`set-default-mv-context-format!`](#set-default-mv-context-format!)
- [`set-of`](#set-of)
- [`set-of-range`](#set-of-range)
- [`singleton?`](#singleton?)
- [`smallest-bond`](#smallest-bond)
- [`sort-by-first`](#sort-by-first)
- [`sort-by-second`](#sort-by-second)
- [`sound-implication-set?`](#sound-implication-set?)
- [`split-at-first`](#split-at-first)
- [`split-at-last`](#split-at-last)
- [`sqrt`](#sqrt)
- [`standard-context`](#standard-context)
- [`standard-layout`](#standard-layout)
- [`stem-base`](#stem-base)
- [`stem-base-from-base`](#stem-base-from-base)
- [`subcontext?`](#subcontext?)
- [`subset?`](#subset?)
- [`subsets`](#subsets)
- [`sum`](#sum)
- [`sup`](#sup)
- [`superset?`](#superset?)
- [`support`](#support)
- [`tails`](#tails)
- [`test-conexp`](#test-conexp)
- [`tests-to-run`](#tests-to-run)
- [`to-set`](#to-set)
- [`topological-sort`](#topological-sort)
- [`transitive-closure`](#transitive-closure)
- [`transitive-reduction`](#transitive-reduction)
- [`union`](#union)
- [`unsupported-operation`](#unsupported-operation)
- [`up-arrows`](#up-arrows)
- [`up-down-arrows`](#up-down-arrows)
- [`values-of-attribute`](#values-of-attribute)
- [`values-of-object`](#values-of-object)
- [`warn`](#warn)
- [`when-available`](#when-available)
- [`with-altered-vars`](#with-altered-vars)
- [`with-memoized-fns`](#with-memoized-fns)
- [`with-printed-result`](#with-printed-result)
- [`with-str-out`](#with-str-out)
- [`with-testing-data`](#with-testing-data)
- [`with-var-bindings`](#with-var-bindings)
- [`write-context`](#write-context)
- [`write-lattice`](#write-lattice)
- [`write-layout`](#write-layout)
- [`write-mv-context`](#write-mv-context)
- [`yes-or-no?`](#yes-or-no?)
- [`zip`](#zip)

### Function Documentation 

#### <a name="-&gt;Formal-Context"> Function `->Formal-Context` 

Argument List: <tt>([&zwj;objects attributes incidence])</tt> 

Documentation: 
>   Positional factory function for class conexp.fca.contexts.Formal-Context. 

#### <a name="-&gt;Implication"> Function `->Implication` 

Argument List: <tt>([&zwj;premise conclusion])</tt> 

Documentation: 
>   Positional factory function for class conexp.fca.implications.Implication. 

#### <a name="-&gt;Lattice"> Function `->Lattice` 

Argument List: <tt>([&zwj;base-set order-function inf sup])</tt> 

Documentation: 
>   Positional factory function for class conexp.fca.lattices.Lattice. 

#### <a name="-&gt;Many-Valued-Context"> Function `->Many-Valued-Context` 

Argument List: <tt>([&zwj;objects attributes incidence])</tt> 

Documentation: 
>   Positional factory function for class conexp.fca.many_valued_contexts.Many-Valued-Context. 

#### <a name="-?&gt;"> Function `-?>` 

Argument List: <tt>([&zwj;x form] [&zwj;x form & forms])</tt> 

Documentation: 
>   DEPRECATED: use clojure.core/some-> instead.
>    
>    Same as clojure.core/-> but returns nil as soon as the threaded value is nil itself (thus short-circuiting any pending computation).
>    Examples :
>    (-?> "foo" .toUpperCase (.substring 1)) returns "OO"
>    (-?> nil .toUpperCase (.substring 1)) returns nil
>     

#### <a name="-?&gt;&gt;"> Function `-?>>` 

Argument List: <tt>([&zwj;x form] [&zwj;x form & forms])</tt> 

Documentation: 
>   DEPRECATED: use clojure.core/some->> instead.
>   
>    Same as clojure.core/->> but returns nil as soon as the threaded value is nil itself (thus short-circuiting any pending computation).
>    Examples :
>    (-?>> (range 5) (map inc)) returns (1 2 3 4 5)
>    (-?>> [] seq (map inc)) returns nil
>     

#### <a name=".?."> Function `.?.` 

Argument List: <tt>([&zwj;x form] [&zwj;x form & forms])</tt> 

Documentation: 
>   Same as clojure.core/.. but returns nil as soon as the threaded value is nil itself (thus short-circuiting any pending computation).
>    Examples :
>    (.?. "foo" .toUpperCase (.substring 1)) returns "OO"
>    (.?. nil .toUpperCase (.substring 1)) returns nil
>     

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

#### <a name="adiag-context"> Function `adiag-context` 

Argument List: <tt>([&zwj;base-set])</tt> 

Documentation: 
>   Returns not= on base-set as context. 

#### <a name="adprime"> Function `adprime` 

Argument List: <tt>([&zwj;ctx set-of-attributes])</tt> 

Documentation: 
>   Computes double prime in context ctx for the given set-of-attributes. 

#### <a name="all-bonds"> Function `all-bonds` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   Returns all bonds between ctx-1 and ctx-2. 

#### <a name="all-bonds-by-shared-intents"> Function `all-bonds-by-shared-intents` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   All bonds between ctx-1 and ctx-2, computed using shared intents. 

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

#### <a name="all-shared-intents"> Function `all-shared-intents` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   All intents shared between contexts ctx-1 and ctx-2. ctx-1 and
>   ctx-2 must have the same attribute set. 

#### <a name="aprime"> Function `aprime` 

Argument List: <tt>([&zwj;ctx attributes])</tt> 

Documentation: 
>   Computes set of objects common to all attributes in context. 

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

#### <a name="available-formats"> Function `available-formats` 

Argument List: <tt>([&zwj;type])</tt> 

Documentation: 
>   Returns for a given type (as string, i.e. "context") all
>   available output methods. 

#### <a name="base-set"> Function `base-set` 

Argument List: <tt>([&zwj;lattice])</tt> 

Documentation: 
>   Returns the base set of lattice. 

#### <a name="biordinal-scale"> Function `biordinal-scale` 

Argument List: <tt>([&zwj;values n] [&zwj;values others n <= >=])</tt> 

Documentation: 
>   Returns the biordinal scale on the sequence values, optionally given
>   two order relations <= and >=. Note that values (and others) must be
>   ordered (e.g. vector or list), because otherwise the result will be
>   arbitrary. 

#### <a name="bond?"> Function `bond?` 

Argument List: <tt>([&zwj;ctx-1 ctx-2 ctx])</tt> 

Documentation: 
>   Checks whether context ctx is a context between ctx-1 and ctx-2. 

#### <a name="canonical-base"> Function `canonical-base` 

Argument List: <tt>([&zwj;ctx] [&zwj;ctx background-knowledge] [&zwj;ctx background-knowledge predicate])</tt> 

Documentation: 
>   Returns the canonical base of given context, as a lazy sequence.  Uses
>   «background-knowledge» as starting set of implications, which will not appear in the
>   result.  If «predicate» is given (a function), computes only those implications from the
>   canonical base whose premise satisfy this predicate, i.e. «predicate» returns true on
>   these premises.  Note that «predicate» has to satisfy the same conditions as the
>   predicate to «next-closed-set-in-family». 

#### <a name="canonical-base-from-base"> Function `canonical-base-from-base` 

Argument List: <tt>([&zwj;implications])</tt> 

Documentation: 
>   For a given set of implications returns its stem-base. 

#### <a name="canonical-base-from-clop"> Function `canonical-base-from-clop` 

Argument List: <tt>([&zwj;clop base] [&zwj;clop base background-knowledge] [&zwj;clop base background-knowledge predicate])</tt> 

Documentation: 
>   Given a closure operator «clop» on the set «base», computes its canonical base,
>    optionally using the set «background-knowledge» of implications on «base-set» as
>    background knowledge.  The result will be a lazy sequence.  If «predicate» is given as
>    third argument, computes only those implications whose premise satisfy this predicate.
>    Note that «predicate» has to satisfy the same conditions as the one of
>    «next-closed-set-in-family». 

#### <a name="ceil"> Function `ceil` 

Argument List: <tt>([&zwj;n])</tt> 

Documentation: 
>   (ceil n) returns the least integer greater than or equal to n.
>   If n is an exact number, ceil returns an integer, otherwise a double. 

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

#### <a name="clojure-type"> Function `clojure-type` 

Argument List: <tt>([&zwj;thing])</tt> 

Documentation: 
>   Dispatch function for multimethods. 

#### <a name="clop-by-implications"> Function `clop-by-implications` 

Argument List: <tt>([&zwj;implications])</tt> 

Documentation: 
>   Returns closure operator given by implications. 

#### <a name="close-under-implications"> Function `close-under-implications` 

Argument List: <tt>([&zwj;implications set])</tt> 

Documentation: 
>   Computes smallest superset of set being closed under given implications. 

#### <a name="close-under-implications-1"> Function `close-under-implications-1` 

Argument List: <tt>([&zwj;implications set])</tt> 

Documentation: 
>   Extends «set» by applying all implications in «implications» once, returning the
>   resulting, extended, set 

#### <a name="compatible-subcontext?"> Function `compatible-subcontext?` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   Tests whether ctx-1 is a compatible subcontext of ctx-2. 

#### <a name="compatible-subcontexts"> Function `compatible-subcontexts` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns all compatible subcontexts of ctx. ctx has to be reduced. 

#### <a name="complete-implication-set?"> Function `complete-implication-set?` 

Argument List: <tt>([&zwj;ctx impl-set])</tt> 

Documentation: 
>   Checks wheter given set of implications is complete in context ctx. This is a very costly
>   computation. 

#### <a name="concept-lattice"> Function `concept-lattice` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns for a given context ctx its concept lattice. 

#### <a name="concept?"> Function `concept?` 

Argument List: <tt>([&zwj;ctx [&zwj;set-of-obj set-of-att]])</tt> 

Documentation: 
>   Tests whether given pair is a concept in context ctx. 

#### <a name="concepts"> Function `concepts` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns a sequence of all concepts of ctx. 

#### <a name="conclusion"> Function `conclusion` 

Argument List: <tt>([&zwj;thing])</tt> 

Documentation: 
>   Returns conclusion of given object. 

#### <a name="conexp-version"> Function `conexp-version` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns the version of conexp as a string. 

#### <a name="confidence"> Function `confidence` 

Argument List: <tt>([&zwj;implication context])</tt> 

Documentation: 
>   Computes the confidence of the given implication in the given context. 

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

#### <a name="context-from-clop"> Function `context-from-clop` 

Argument List: <tt>([&zwj;base-set clop])</tt> 

Documentation: 
>   Returns a context whose intents are exactly the closed sets of the
>   given closure operator on the given base-set. 

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
>   Returns tuple of number of objects, number of attributes and fill rate. 

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
>   inclusive, use context-disjoint-union if this is not what you want. 

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

#### <a name="default-handler-for-complete-counterexamples"> Function `default-handler-for-complete-counterexamples` 

Argument List: <tt>([&zwj;ctx known impl])</tt> 

Documentation: 
>   Default handler for attribute exploration. Does it's interaction on the console. 

#### <a name="default-handler-for-incomplete-counterexamples"> Function `default-handler-for-incomplete-counterexamples` 

Argument List: <tt>([&zwj;possible-ctx certain-ctx known impl])</tt> 

Documentation: 
>   Default handler for attribute exploration with incomplete counterexamples. Does it's
>     interaction on the console. 

#### <a name="define-context-input-format"> Function `define-context-input-format` 

Argument List: <tt>([&zwj;input-format [&zwj;file] & body])</tt> 

Documentation: 
>   Defines input format for contexts. 

#### <a name="define-context-output-format"> Function `define-context-output-format` 

Argument List: <tt>([&zwj;input-format [&zwj;thing file] & body])</tt> 

Documentation: 
>   Defines output format for contexts. 

#### <a name="define-lattice-input-format"> Function `define-lattice-input-format` 

Argument List: <tt>([&zwj;input-format [&zwj;file] & body])</tt> 

Documentation: 
>   Defines input format for lattices. 

#### <a name="define-lattice-output-format"> Function `define-lattice-output-format` 

Argument List: <tt>([&zwj;input-format [&zwj;thing file] & body])</tt> 

Documentation: 
>   Defines output format for lattices. 

#### <a name="define-layout-input-format"> Function `define-layout-input-format` 

Argument List: <tt>([&zwj;input-format [&zwj;file] & body])</tt> 

Documentation: 
>   Defines input format for layouts. 

#### <a name="define-layout-output-format"> Function `define-layout-output-format` 

Argument List: <tt>([&zwj;input-format [&zwj;thing file] & body])</tt> 

Documentation: 
>   Defines output format for layouts. 

#### <a name="define-mv-context-input-format"> Function `define-mv-context-input-format` 

Argument List: <tt>([&zwj;input-format [&zwj;file] & body])</tt> 

Documentation: 
>   Defines input format for mv-contexts. 

#### <a name="define-mv-context-output-format"> Function `define-mv-context-output-format` 

Argument List: <tt>([&zwj;input-format [&zwj;thing file] & body])</tt> 

Documentation: 
>   Defines output format for mv-contexts. 

#### <a name="defmacro-"> Function `defmacro-` 

Argument List: <tt>([&zwj;name & decls])</tt> 

Documentation: 
>   Same as defmacro but yields a private definition 

#### <a name="diag-context"> Function `diag-context` 

Argument List: <tt>([&zwj;base-set])</tt> 

Documentation: 
>   Returns = on base-set as context. 

#### <a name="dichotomic-scale"> Function `dichotomic-scale` 

Argument List: <tt>([&zwj;values])</tt> 

Documentation: 
>   Returns the dichotimic scale on the set values. Note that base must
>   have exactly two arguments. 

#### <a name="die-with-error"> Function `die-with-error` 

Argument List: <tt>([&zwj;error strings])</tt> 

Documentation: 
>   Stops program by raising the given error with strings as message. 

#### <a name="difference"> Function `difference` 

Argument List: <tt>([&zwj;s1] [&zwj;s1 s2] [&zwj;s1 s2 & sets])</tt> 

Documentation: 
>   Return a set that is the first set without elements of the remaining sets 

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

#### <a name="directly-neighboured?"> Function `directly-neighboured?` 

Argument List: <tt>([&zwj;lat x y])</tt> 

Documentation: 
>   Checks whether x is direct lower neighbour of y in lattice lat. 

#### <a name="disjoint-union"> Function `disjoint-union` 

Argument List: <tt>([&zwj;& sets])</tt> 

Documentation: 
>   Computes the disjoint union of sets by joining the cross-products of the
>   sets with natural numbers. 

#### <a name="dissoc-in"> Function `dissoc-in` 

Argument List: <tt>([&zwj;m [&zwj;k & ks :as keys]])</tt> 

Documentation: 
>   Dissociates an entry from a nested associative structure returning a new
>   nested structure. keys is a sequence of keys. Any empty maps that result
>   will not be present in the new structure. 

#### <a name="distinct-by-key"> Function `distinct-by-key` 

Argument List: <tt>([&zwj;sequence key])</tt> 

Documentation: 
>   Returns a sequence of all elements of the given sequence with distinct key values,
>   where key is a function from the elements of the given sequence. If two elements
>   correspond to the same key, the one is chosen which appeared earlier in the sequence.
>   
>   This function is copied from clojure.core/distinct and adapted for using a key function. 

#### <a name="distributive?"> Function `distributive?` 

Argument List: <tt>([&zwj;lat])</tt> 

Documentation: 
>   Checks (primitively) whether given lattice lat is distributive or not. 

#### <a name="div"> Function `div` 

Argument List: <tt>([&zwj;a b])</tt> 

Documentation: 
>   Integer division. 

#### <a name="down-arrows"> Function `down-arrows` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Computes the down arrow relation of ctx. 

#### <a name="dual-context"> Function `dual-context` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Dualizes context ctx, that is (G,M,I) gets (M,G,I^{-1}). 

#### <a name="dual-lattice"> Function `dual-lattice` 

Argument List: <tt>([&zwj;lat])</tt> 

Documentation: 
>   Dualizes given lattice lat. 

#### <a name="ensure-length"> Function `ensure-length` 

Argument List: <tt>([&zwj;string length] [&zwj;string length padding])</tt> 

Documentation: 
>   Fills given string with padding to have at least the given length. 

#### <a name="ensure-seq"> Function `ensure-seq` 

Argument List: <tt>([&zwj;x])</tt> 

Documentation: 
>   Given anything that can be made a sequence from, returns that
>   sequence. If given a number x, returns (range x). 

#### <a name="equivalent-implications?"> Function `equivalent-implications?` 

Argument List: <tt>([&zwj;impls-1 impls-2])</tt> 

Documentation: 
>   Returns true iff the two seqs of implications are equivalent. 

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

#### <a name="expt"> Function `expt` 

Argument List: <tt>([&zwj;a b])</tt> 

Documentation: 
>   Exponentiation of arguments. Is exact if given arguments are exact
>   and returns double otherwise. 

#### <a name="extents"> Function `extents` 

Argument List: <tt>([&zwj;ctx] [&zwj;ctx pred])</tt> 

Documentation: 
>   Computes a sequence of all extents of «ctx», in a lectic order.  Optionally, one can
>   specify a predicate function «pred» that acts as a filter on all extents of «ctx».
>   «pred» should specify the same conditions as the predicate function to
>   «next-closed-set-in-family». 

#### <a name="first-non-nil"> Function `first-non-nil` 

Argument List: <tt>([&zwj;seq])</tt> 

Documentation: 
>   Returns first non-nil element in seq, or nil if there is none. 

#### <a name="floor"> Function `floor` 

Argument List: <tt>([&zwj;n])</tt> 

Documentation: 
>   (floor n) returns the greatest integer less than or equal to n.
>   If n is an exact number, floor returns an integer, otherwise a double. 

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

#### <a name="forall"> Function `forall` 

Argument List: <tt>([&zwj;bindings condition])</tt> 

Documentation: 
>   Implements logical forall quantor. Bindings is of the form [var-1
>   seq-1 var-2 seq-2 ...]. Returns boolean value. 

#### <a name="frequent-closed-itemsets"> Function `frequent-closed-itemsets` 

Argument List: <tt>([&zwj;context minsupp])</tt> 

Documentation: 
>   Computes for context a lazy sequence of all frequent and closed itemsets, given minsupp as
>   minimal support. 

#### <a name="gcd"> Function `gcd` 

Argument List: <tt>([&zwj;a b])</tt> 

Documentation: 
>   (gcd a b) returns the greatest common divisor of a and b 

#### <a name="generic-equals"> Function `generic-equals` 

Argument List: <tt>([&zwj;[&zwj;this other] class fields])</tt> 

Documentation: 
>   Implements a generic equals for class on fields. 

#### <a name="get-default-context-format"> Function `get-default-context-format` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns default write format for contexts. 

#### <a name="get-default-lattice-format"> Function `get-default-lattice-format` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns default write format for lattices. 

#### <a name="get-default-layout-format"> Function `get-default-layout-format` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns default write format for layouts. 

#### <a name="get-default-mv-context-format"> Function `get-default-mv-context-format` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns default write format for mv-contexts. 

#### <a name="graph-of-function?"> Function `graph-of-function?` 

Argument List: <tt>([&zwj;relation source target])</tt> 

Documentation: 
>   Returns true iff relation is the graph of a function from source to target. 

#### <a name="has-lattice-order?"> Function `has-lattice-order?` 

Argument List: <tt>([&zwj;lat])</tt> 

Documentation: 
>   Given a lattice checks if its order is indeed a lattice order. 

#### <a name="has-version?"> Function `has-version?` 

Argument List: <tt>([&zwj;{my-major :major, my-minor :minor, my-patch :patch}])</tt> 

Documentation: 
>   Compares given version of conexp and returns true if and only if
>   the current version of conexp is higher or equal than the given one 

#### <a name="hash-combine-hash"> Function `hash-combine-hash` 

Argument List: <tt>([&zwj;& args])</tt> 

Documentation: 
>   Combines the hashes of all things given. 

#### <a name="holds?"> Function `holds?` 

Argument List: <tt>([&zwj;impl ctx])</tt> 

Documentation: 
>   Returns true iff impl holds in given context ctx. 

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

#### <a name="impl"> Function `impl` 

Argument List: <tt>([&zwj;& elements])</tt> 

Documentation: 
>   Convenience interface for creating implications.  Write implications just as
>   
>     user=> (impl 1 2 3 ==> 4 5 6)
>     (#{1 2 3} ==> #{4 5 6}) 

#### <a name="implication-context"> Function `implication-context` 

Argument List: <tt>([&zwj;n])</tt> 

Documentation: 
>   Returns a context for a non-negative number n which has as it's
>   extents the closure systems on the set {0 .. (n-1)} and as it's
>   intents the corresponding implicational theory. 

#### <a name="implication?"> Function `implication?` 

Argument List: <tt>([&zwj;thing])</tt> 

Documentation: 
>   Returns true iff thing is an implication. 

#### <a name="improve-basic-order"> Function `improve-basic-order` 

Argument List: <tt>([&zwj;base clop])</tt> 

Documentation: 
>   Improves basic order on the sequence base, where the closure operator
>   clop operates on. 

#### <a name="incidence"> Function `incidence` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns the incidence of a context as a set of pairs. 

#### <a name="incident?"> Function `incident?` 

Argument List: <tt>([&zwj;ctx g m])</tt> 

Documentation: 
>   Returns true if and only if in context ctx, the object g is
>   incident with the attribute m. 

#### <a name="index"> Function `index` 

Argument List: <tt>([&zwj;xrel ks])</tt> 

Documentation: 
>   Returns a map of the distinct values of ks in the xrel mapped to a
>   set of the maps in xrel with the corresponding values of ks. 

#### <a name="inf"> Function `inf` 

Argument List: <tt>([&zwj;lattice])</tt> 

Documentation: 
>   Returns a function computing the infimum in lattice. 

#### <a name="inf-additive-layout"> Function `inf-additive-layout` 

Argument List: <tt>([&zwj;lattice])</tt> 

Documentation: 
>   Returns an infimum additive layout for lattice. 

#### <a name="inits"> Function `inits` 

Argument List: <tt>([&zwj;sqn])</tt> 

Documentation: 
>   Returns a lazy sequence of the beginnings of sqn. 

#### <a name="integer-length"> Function `integer-length` 

Argument List: <tt>([&zwj;n])</tt> 

Documentation: 
>   Length of integer in binary 

#### <a name="intents"> Function `intents` 

Argument List: <tt>([&zwj;ctx] [&zwj;ctx pred])</tt> 

Documentation: 
>   Computes a sequence of all intents of «ctx», in a lectic order.  Optionally, one can
>   specify a predicate function «pred» that acts as a filter on all intents of «ctx».
>   «pred» should specify the same conditions as the predicate function to
>   «next-closed-set-in-family». 

#### <a name="interordinal-scale"> Function `interordinal-scale` 

Argument List: <tt>([&zwj;values] [&zwj;values <= >=] [&zwj;values others <= >=])</tt> 

Documentation: 
>   Returns the interordinal scale on the set base, optionally given
>   two order relations <= and >=. 

#### <a name="intersect-implicational-theories"> Function `intersect-implicational-theories` 

Argument List: <tt>([&zwj;base-set & implication-sets])</tt> 

Documentation: 
>   Given a set «base-set» and collections «implication-sets» of implications, returns the
>   canonical base of the intersection of the corresponding closure theories. 

#### <a name="intersection"> Function `intersection` 

Argument List: <tt>([&zwj;s1] [&zwj;s1 s2] [&zwj;s1 s2 & sets])</tt> 

Documentation: 
>   Return a set that is the intersection of the input sets 

#### <a name="interval-scale"> Function `interval-scale` 

Argument List: <tt>([&zwj;values] [&zwj;values others] [&zwj;values others < >=])</tt> 

Documentation: 
>   Returns the interval scale on the set values. Note that values must
>   be ordered (e.g. vector or list), because otherwise the result will
>   be arbitrary 

#### <a name="invert-context"> Function `invert-context` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Inverts context ctx, that is (G,M,I) gets (G,M,(G x M) \ I). 

#### <a name="irredundant-subset"> Function `irredundant-subset` 

Argument List: <tt>([&zwj;impls])</tt> 

Documentation: 
>   Given a set impls of implications, returns an irredundant subset of impls.  Note that
>   this set does not need to be of minimal cardinality. 

#### <a name="join"> Function `join` 

Argument List: <tt>([&zwj;xrel yrel] [&zwj;xrel yrel km])</tt> 

Documentation: 
>   When passed 2 rels, returns the rel corresponding to the natural
>   join. When passed an additional keymap, joins on the corresponding
>   keys. 

#### <a name="latex"> Function `latex` 

Argument List: <tt>([&zwj;this] [&zwj;this choice])</tt> 

Documentation: 
>   Returns a string representation of this. 

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

#### <a name="luxenburger-base"> Function `luxenburger-base` 

Argument List: <tt>([&zwj;context minsupp-or-predicate minconf])</tt> 

Documentation: 
>   Computes the luxenburger-base of a given context «context», returning the result as a
>   lazy sequence.  Uses «minconf» as minimal confidence.  If «minsupp-or-predicate» is a
>   number, uses that as a minimal support threshold.  In this case, «minsupp» ∈ [0,1] must
>   hold.  If «minsupp-or-predicate» is a function, uses this as a predicate to filter all
>   candidate itemsets.  In this case, the predicate should be valid predicate value for
>   «intents». 

#### <a name="luxenburger-basis"> Function `luxenburger-basis` 

Argument List: <tt>([&zwj;context minsupp-or-predicate minconf])</tt> 

Documentation: 
>   Computes the luxenburger-base of a given context «context», returning the result as a
>   lazy sequence.  Uses «minconf» as minimal confidence.  If «minsupp-or-predicate» is a
>   number, uses that as a minimal support threshold.  In this case, «minsupp» ∈ [0,1] must
>   hold.  If «minsupp-or-predicate» is a function, uses this as a predicate to filter all
>   candidate itemsets.  In this case, the predicate should be valid predicate value for
>   «intents». 

#### <a name="make-context"> Function `make-context` 

Argument List: <tt>([&zwj;objects attributes incidence])</tt> 

Documentation: 
>   Standard constructor for contexts. Takes a sequence of objects,
>   a sequence of attributes and either a set of pairs or function of
>   two elements being true iff its arguments are incident. Note that
>   the object and attribute sequences are converted to sets and
>   therefore have to not contain any douplicate elements. The incidence
>   relation is auzomatically restricted to the cartesian product of the
>   object an the attribute set. 

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

#### <a name="make-implication"> Function `make-implication` 

Argument List: <tt>([&zwj;premise conclusion])</tt> 

Documentation: 
>   Creates an implication (premise => conclusion \ premise). 

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
>   Returns all minimal hypergraph transversals of the hypergraph defined by «edges» on the
>   vertex sets «vertices». 

#### <a name="minimal-implication-set?"> Function `minimal-implication-set?` 

Argument List: <tt>([&zwj;impl-set])</tt> 

Documentation: 
>   Checks whether given set of implications is minimal, i.e. no
>   implication in this set follows from the others. 

#### <a name="minimum-set-covers"> Function `minimum-set-covers` 

Argument List: <tt>([&zwj;base-set sets])</tt> 

Documentation: 
>   For a given set base-set and a collection of sets returns all subcollections of sets such that
>   the union of the contained sets cover base-set and that are minimal with that property. 

#### <a name="modular?"> Function `modular?` 

Argument List: <tt>([&zwj;lat])</tt> 

Documentation: 
>   Checks (primitively) whether given lattice lat is modular or not. 

#### <a name="mv-context-to-string"> Function `mv-context-to-string` 

Argument List: <tt>([&zwj;mv-ctx] [&zwj;mv-ctx order-on-objects order-on-attributes])</tt> 

Documentation: 
>   Returns a string representing the given many-valued context mv-ctx
>   as a value-table. 

#### <a name="new-by-name"> Function `new-by-name` 

Argument List: <tt>([&zwj;class-name & args])</tt> 

Documentation: 
>   Constructs a Java object whose class is specified by a String. 

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

#### <a name="nominal-scale"> Function `nominal-scale` 

Argument List: <tt>([&zwj;values] [&zwj;values others])</tt> 

Documentation: 
>   Returns the nominal scale on the set base. 

#### <a name="not-yet-implemented"> Function `not-yet-implemented` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Throws UnsupportedOperationException with "Not yet implemented"
>   message. 

#### <a name="now"> Function `now` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns the current time in a human readable format. 

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

#### <a name="order"> Function `order` 

Argument List: <tt>([&zwj;lattice])</tt> 

Documentation: 
>   Returns a function of one or two arguments representing the order
>   relation. If called with one argument it is assumed that this
>   argument is a pair of elements. 

#### <a name="order-by"> Function `order-by` 

Argument List: <tt>([&zwj;sequence])</tt> 

Documentation: 
>   Returns a function on two arguments a and b that returns true if and only if b occurs
>   not after a in the given sequence (ascending order.) 

#### <a name="ordinal-scale"> Function `ordinal-scale` 

Argument List: <tt>([&zwj;values] [&zwj;values <=] [&zwj;values others <=])</tt> 

Documentation: 
>   Returns the ordinal scale on the set values, optionally given an
>   order relation <=. 

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

#### <a name="premise"> Function `premise` 

Argument List: <tt>([&zwj;thing])</tt> 

Documentation: 
>   Returns premise of given object. 

#### <a name="print-context"> Function `print-context` 

Argument List: <tt>([&zwj;ctx & args])</tt> 

Documentation: 
>   Prints the result of applying context-to-string to the given
>   arguments. 

#### <a name="prod"> Function `prod` 

Argument List: <tt>([&zwj;index start end & expr])</tt> 

Documentation: 
>   Computes the product of expr for indices from start to end, named
>   as index. 

#### <a name="project"> Function `project` 

Argument List: <tt>([&zwj;xrel ks])</tt> 

Documentation: 
>   Returns a rel of the elements of xrel with only the keys in ks 

#### <a name="proper-conclusion"> Function `proper-conclusion` 

Argument List: <tt>([&zwj;ctx A])</tt> 

Documentation: 
>   Returns all elements which are implied in context ctx by A but are neither contained in A or
>   follow from a strict subsets of A. 

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

#### <a name="proper-premises-for-attribute"> Function `proper-premises-for-attribute` 

Argument List: <tt>([&zwj;ctx m])</tt> 

Documentation: 
>   Returns all proper premises for the attribute «m» in the formal context «ctx». 

#### <a name="proper-subset?"> Function `proper-subset?` 

Argument List: <tt>([&zwj;set-1 set-2])</tt> 

Documentation: 
>   Returns true iff set-1 is a proper subset of set-2. 

#### <a name="proper-superset?"> Function `proper-superset?` 

Argument List: <tt>([&zwj;set-1 set-2])</tt> 

Documentation: 
>   Returns true iff set-1 is a proper superset of set-2. 

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

#### <a name="quit"> Function `quit` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Quits conexp-clj. 

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

#### <a name="read-context"> Function `read-context` 

Argument List: <tt>([&zwj;file] [&zwj;file explicit-format])</tt> 

Documentation: 
>   Reads context from file, automatically determining the format used. 

#### <a name="read-lattice"> Function `read-lattice` 

Argument List: <tt>([&zwj;file] [&zwj;file explicit-format])</tt> 

Documentation: 
>   Reads lattice from file, automatically determining the format used. 

#### <a name="read-layout"> Function `read-layout` 

Argument List: <tt>([&zwj;file] [&zwj;file explicit-format])</tt> 

Documentation: 
>   Reads layout from file, automatically determining the format used. 

#### <a name="read-mv-context"> Function `read-mv-context` 

Argument List: <tt>([&zwj;file] [&zwj;file explicit-format])</tt> 

Documentation: 
>   Reads mv-context from file, automatically determining the format used. 

#### <a name="reduce!"> Function `reduce!` 

Argument List: <tt>([&zwj;fn initial-value coll])</tt> 

Documentation: 
>   Does the same as reduce, but calls transient on the initial value
>   and persistent! on the result. 

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

#### <a name="reflexive-transitive-closure"> Function `reflexive-transitive-closure` 

Argument List: <tt>([&zwj;base-set pairs])</tt> 

Documentation: 
>   Computes the reflexive, transitive closure of a given set of pairs
>   on base-set. 

#### <a name="rename"> Function `rename` 

Argument List: <tt>([&zwj;xrel kmap])</tt> 

Documentation: 
>   Returns a rel of the maps in xrel with the keys in kmap renamed to the vals in kmap 

#### <a name="rename-attributes"> Function `rename-attributes` 

Argument List: <tt>([&zwj;ctx old-to-new])</tt> 

Documentation: 
>   Rename attributes in ctx by given function old-to-new. 

#### <a name="rename-keys"> Function `rename-keys` 

Argument List: <tt>([&zwj;map kmap])</tt> 

Documentation: 
>   Returns the map with the keys in kmap renamed to the vals in kmap 

#### <a name="rename-objects"> Function `rename-objects` 

Argument List: <tt>([&zwj;ctx old-to-new])</tt> 

Documentation: 
>   Rename objects in ctx by given function old-to-new. 

#### <a name="respects?"> Function `respects?` 

Argument List: <tt>([&zwj;set impl])</tt> 

Documentation: 
>   Returns true iff set respects given implication impl. 

#### <a name="restrict-concept"> Function `restrict-concept` 

Argument List: <tt>([&zwj;concept subcontext])</tt> 

Documentation: 
>   Restricts the given concept to the given subcontext. 

#### <a name="round"> Function `round` 

Argument List: <tt>([&zwj;n])</tt> 

Documentation: 
>   (round n) rounds to the nearest integer.
>   round always returns an integer.  Rounds up for values exactly in between two integers. 

#### <a name="ryssel-base"> Function `ryssel-base` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns the implications computed by Ryssels Algorithm, as a lazy sequence. 

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

#### <a name="select"> Function `select` 

Argument List: <tt>([&zwj;pred xset])</tt> 

Documentation: 
>   Returns a set of the elements for which pred is true 

#### <a name="seqable?"> Function `seqable?` 

Argument List: <tt>([&zwj;x])</tt> 

Documentation: 
>   Returns true if (seq x) will succeed, false otherwise. 

#### <a name="set-default-context-format!"> Function `set-default-context-format!` 

Argument List: <tt>([&zwj;format__3780__auto__])</tt> 

Documentation: 
>   Sets default write format for contexts to format. 

#### <a name="set-default-lattice-format!"> Function `set-default-lattice-format!` 

Argument List: <tt>([&zwj;format__3780__auto__])</tt> 

Documentation: 
>   Sets default write format for lattices to format. 

#### <a name="set-default-layout-format!"> Function `set-default-layout-format!` 

Argument List: <tt>([&zwj;format__3780__auto__])</tt> 

Documentation: 
>   Sets default write format for layouts to format. 

#### <a name="set-default-mv-context-format!"> Function `set-default-mv-context-format!` 

Argument List: <tt>([&zwj;format__3780__auto__])</tt> 

Documentation: 
>   Sets default write format for mv-contexts to format. 

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

#### <a name="smallest-bond"> Function `smallest-bond` 

Argument List: <tt>([&zwj;ctx-1 ctx-2 rel])</tt> 

Documentation: 
>   Returns the smallest bond between ctx-1 and ctx-2 that has the
>   elements of rel as crosses. 

#### <a name="sort-by-first"> Function `sort-by-first` 

Argument List: <tt>([&zwj;x y])</tt> 

Documentation: 
>   Ensures that pairs are ordered by first entry first. 

#### <a name="sort-by-second"> Function `sort-by-second` 

Argument List: <tt>([&zwj;x y])</tt> 

Documentation: 
>   Ensures that pairs are ordered by second entry first. This gives
>   better output for context sums, products, ... 

#### <a name="sound-implication-set?"> Function `sound-implication-set?` 

Argument List: <tt>([&zwj;ctx impl-set])</tt> 

Documentation: 
>   Checks whether given set of implications is sound, i.e. every
>   implication holds in the given context. 

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

#### <a name="standard-context"> Function `standard-context` 

Argument List: <tt>([&zwj;lat])</tt> 

Documentation: 
>   Returns the standard context of lattice lat. 

#### <a name="standard-layout"> Function `standard-layout` 

Argument List: <tt></tt> 

Documentation: 
>   Standard layout function. Call on a lattice to get a layout. 

#### <a name="stem-base"> Function `stem-base` 

Argument List: <tt>([&zwj;ctx] [&zwj;ctx background-knowledge] [&zwj;ctx background-knowledge predicate])</tt> 

Documentation: 
>   Returns the canonical base of given context, as a lazy sequence.  Uses
>   «background-knowledge» as starting set of implications, which will not appear in the
>   result.  If «predicate» is given (a function), computes only those implications from the
>   canonical base whose premise satisfy this predicate, i.e. «predicate» returns true on
>   these premises.  Note that «predicate» has to satisfy the same conditions as the
>   predicate to «next-closed-set-in-family». 

#### <a name="stem-base-from-base"> Function `stem-base-from-base` 

Argument List: <tt>([&zwj;implications])</tt> 

Documentation: 
>   For a given set of implications returns its stem-base. 

#### <a name="subcontext?"> Function `subcontext?` 

Argument List: <tt>([&zwj;ctx-1 ctx-2])</tt> 

Documentation: 
>   Tests whether ctx-1 is a subcontext ctx-2 or not. 

#### <a name="subset?"> Function `subset?` 

Argument List: <tt>([&zwj;set1 set2])</tt> 

Documentation: 
>   Is set1 a subset of set2? 

#### <a name="subsets"> Function `subsets` 

Argument List: <tt>([&zwj;set])</tt> 

Documentation: 
>   Returns all subsets of set. 

#### <a name="sum"> Function `sum` 

Argument List: <tt>([&zwj;index start end & expr])</tt> 

Documentation: 
>   Computes the sum of expr for indices from start to end, named as
>   index. 

#### <a name="sup"> Function `sup` 

Argument List: <tt>([&zwj;lattice])</tt> 

Documentation: 
>   Returns a function computing the supremum in lattice. 

#### <a name="superset?"> Function `superset?` 

Argument List: <tt>([&zwj;set1 set2])</tt> 

Documentation: 
>   Is set1 a superset of set2? 

#### <a name="support"> Function `support` 

Argument List: <tt>([&zwj;thing ctx])</tt> 

Documentation: 
>   Computes the support of the set of attributes B in context ctx. If an implications is given,
>   returns the support of this implication in the given context. 

#### <a name="tails"> Function `tails` 

Argument List: <tt>([&zwj;sqn])</tt> 

Documentation: 
>   Returns a lazy sequence of the tails of sqn. 

#### <a name="test-conexp"> Function `test-conexp` 

Argument List: <tt>([&zwj;] [&zwj;with-contrib?])</tt> 

Documentation: 
>   Runs tests for conexp. If with-contrib? is given and true, tests
>   conexp.contrib.tests too. 

#### <a name="tests-to-run"> Function `tests-to-run` 

Argument List: <tt>([&zwj;& namespaces])</tt> 

Documentation: 
>   Defines tests to run when the namespace in which this macro is
>   called is tested by test-ns.  Additionally, runs all tests in the
>   current namespace, before all other tests in supplied as arguments. 

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

#### <a name="up-arrows"> Function `up-arrows` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Computes the up arrow relation of ctx. 

#### <a name="up-down-arrows"> Function `up-down-arrows` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns up-down-arrow relation of ctx. 

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

#### <a name="write-context"> Function `write-context` 

Argument List: <tt>([&zwj;format context file] [&zwj;context file])</tt> 

Documentation: 
>   Writes context to file using format. 

#### <a name="write-lattice"> Function `write-lattice` 

Argument List: <tt>([&zwj;format lattice file] [&zwj;lattice file])</tt> 

Documentation: 
>   Writes lattice to file using format. 

#### <a name="write-layout"> Function `write-layout` 

Argument List: <tt>([&zwj;format layout file] [&zwj;layout file])</tt> 

Documentation: 
>   Writes layout to file using format. 

#### <a name="write-mv-context"> Function `write-mv-context` 

Argument List: <tt>([&zwj;format mv-context file] [&zwj;mv-context file])</tt> 

Documentation: 
>   Writes mv-context to file using format. 

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


## <a name="conexp.layouts.base"> Public API of conexp.layouts.base 


### Available Functions 

- [`->Layout`](#->Layout)
- [`annotation`](#annotation)
- [`check-labels`](#check-labels)
- [`concept-lattice-annotation`](#concept-lattice-annotation)
- [`concept-lattice-layout?`](#concept-lattice-layout?)
- [`connections`](#connections)
- [`context`](#context)
- [`def-layout-fn`](#def-layout-fn)
- [`full-order-relation`](#full-order-relation)
- [`inf-irreducibles`](#inf-irreducibles)
- [`information`](#information)
- [`lattice`](#lattice)
- [`lattice-from-layout-data`](#lattice-from-layout-data)
- [`layout?`](#layout?)
- [`lower-label`](#lower-label)
- [`lower-label-position`](#lower-label-position)
- [`lower-labels`](#lower-labels)
- [`lower-neighbours`](#lower-neighbours)
- [`make-layout`](#make-layout)
- [`make-layout-nc`](#make-layout-nc)
- [`nodes`](#nodes)
- [`positions`](#positions)
- [`set-to-label`](#set-to-label)
- [`sup-irreducibles`](#sup-irreducibles)
- [`update-positions`](#update-positions)
- [`upper-label`](#upper-label)
- [`upper-label-position`](#upper-label-position)
- [`upper-labels`](#upper-labels)
- [`upper-neighbours`](#upper-neighbours)
- [`upper-neighbours-of-inf-irreducibles`](#upper-neighbours-of-inf-irreducibles)
- [`verify-labels`](#verify-labels)
- [`verify-lattice-positions-connections`](#verify-lattice-positions-connections)

### Function Documentation 

#### <a name="-&gt;Layout"> Function `->Layout` 

Argument List: <tt>([&zwj;lattice positions connections upper-labels lower-labels information])</tt> 

Documentation: 
>   Positional factory function for class conexp.layouts.base.Layout. 

#### <a name="annotation"> Function `annotation` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns the annotation of this layout as hash-map of nodes to
>   pairs, where the first entry is the upper label and second one is
>   the lower label. 

#### <a name="concept-lattice-annotation"> Function `concept-lattice-annotation` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns the shortend annotation for a concept lattice. 

#### <a name="concept-lattice-layout?"> Function `concept-lattice-layout?` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Tests whether layout comes from a concept lattice.
>   
>   Note: This implementation is not correct, as it only tests whether
>   the layout repects the subset relation in the first component and
>   the superset relation in the second component of every node. 

#### <a name="connections"> Function `connections` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns set of connections of layout. 

#### <a name="context"> Function `context` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns a context whose lattice is represented by this layout. 

#### <a name="def-layout-fn"> Function `def-layout-fn` 

Argument List: <tt>([&zwj;name doc-string [&zwj;layout & args] & body])</tt> 

Documentation: 
>   Defines a function name on layout. If this function has been called
>   on this layout before, returns the stored value. Otherwise computes
>   a value and stores it. 

#### <a name="full-order-relation"> Function `full-order-relation` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns underlying order relation of layout. This operation may be
>   very costly. 

#### <a name="inf-irreducibles"> Function `inf-irreducibles` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns the set of infimum irreducible elements of layout. 

#### <a name="information"> Function `information` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns stored additional information of layout. 

#### <a name="lattice"> Function `lattice` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns the lattice underlying the given layout. 

#### <a name="lattice-from-layout-data"> Function `lattice-from-layout-data` 

Argument List: <tt>([&zwj;positions connections])</tt> 

Documentation: 
>   Returns lattice represented by layout. 

#### <a name="layout?"> Function `layout?` 

Argument List: <tt>([&zwj;thing])</tt> 

Documentation: 
>   Returns true iff thing is a layout. 

#### <a name="lower-label"> Function `lower-label` 

Argument List: <tt>([&zwj;layout x])</tt> 

Documentation: 
>   Returns the lower label of x in layout, if it exists. Otherwise returns nil. 

#### <a name="lower-label-position"> Function `lower-label-position` 

Argument List: <tt>([&zwj;layout x])</tt> 

Documentation: 
>   Returns the position of the lower label of x in layout, if existent. Otherwise returns nil. 

#### <a name="lower-labels"> Function `lower-labels` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns the lower labels of a given layout. 

#### <a name="lower-neighbours"> Function `lower-neighbours` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns hash-map mapping node names to sets of their upper neighbours. 

#### <a name="make-layout"> Function `make-layout` 

Argument List: <tt>([&zwj;lattice positions connections upper-label lower-label] [&zwj;positions connections upper-label lower-label] [&zwj;lattice positions connections] [&zwj;positions connections])</tt> 

Documentation: 
>   Creates layout datatype from given positions hash-map, mapping node
>   names to coordinate pairs, and connections, a set of pairs of node
>   names denoting edges in the layout. 

#### <a name="make-layout-nc"> Function `make-layout-nc` 

Argument List: <tt>([&zwj;lattice positions connections upper-labels lower-labels] [&zwj;lattice positions connections] [&zwj;positions connections upper-label lower-label] [&zwj;positions connections])</tt> 

Documentation: 
>   Creates layout datatype from given information. The arguments thereby have the following meaning:
>   
>    - lattice is the underlying lattice of the to be constructed layout
>    - positions is a hash-map, mapping node names to coordinate pairs,
>    - connections is a set of pairs of node names denoting edges in the layout,
>    - upper-labels is a map mapping nodes to pairs of upper labels and coordinates or nil,
>    - lower-labels is like upper-labels for lower-labels.
>   
>   This functions does only a limited amount of error checking. 

#### <a name="nodes"> Function `nodes` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns all nodes of a given layout. 

#### <a name="positions"> Function `positions` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Return positions map of layout. 

#### <a name="set-to-label"> Function `set-to-label` 

Argument List: <tt>([&zwj;set])</tt> 

Documentation: 
>   Converts set of elements to a label. 

#### <a name="sup-irreducibles"> Function `sup-irreducibles` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns the set of supremum irreducible elements of layout. 

#### <a name="update-positions"> Function `update-positions` 

Argument List: <tt>([&zwj;layout new-positions])</tt> 

Documentation: 
>   Updates position map in layout to be new-positions. Keys of both
>   hash-maps must be the same. 

#### <a name="upper-label"> Function `upper-label` 

Argument List: <tt>([&zwj;layout x])</tt> 

Documentation: 
>   Returns the upper label of x in layout, if it exists. Otherwise returns nil. 

#### <a name="upper-label-position"> Function `upper-label-position` 

Argument List: <tt>([&zwj;layout x])</tt> 

Documentation: 
>   Returns the position of the upper label of x in layout, if existent. Otherwise returns nil. 

#### <a name="upper-labels"> Function `upper-labels` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns the upper labels of a given layout. 

#### <a name="upper-neighbours"> Function `upper-neighbours` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns hash-map mapping node names to sets of their upper neighbours. 

#### <a name="upper-neighbours-of-inf-irreducibles"> Function `upper-neighbours-of-inf-irreducibles` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns hash-map mapping the infimum irreducible elements to their
>   upper neighbours. 


## <a name="conexp.layouts.common"> Public API of conexp.layouts.common 


### Available Functions 

- [`layout-by-placement`](#layout-by-placement)
- [`placement-by-initials`](#placement-by-initials)
- [`to-inf-additive-layout`](#to-inf-additive-layout)

### Function Documentation 

#### <a name="layout-by-placement"> Function `layout-by-placement` 

Argument List: <tt>([&zwj;lattice top placement])</tt> 

Documentation: 
>   Computes additive layout of lattice by given positions of the keys
>   of placement. The values of placement should be the positions of the
>   corresponding keys. Top element will be at top. 

#### <a name="placement-by-initials"> Function `placement-by-initials` 

Argument List: <tt>([&zwj;lattice top placement])</tt> 

Documentation: 
>   Computes placement for all elements by positions of some initial
>   nodes. Top element will be at top. 

#### <a name="to-inf-additive-layout"> Function `to-inf-additive-layout` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns an infimum additive layout from given layout, taking the
>   positions of the infimum irreducible elements as initial positions for
>   the resulting additive layout. 


## <a name="conexp.layouts.force"> Public API of conexp.layouts.force 


### Available Functions 

- [`*attractive-amount*`](#*attractive-amount*)
- [`*gravitative-amount*`](#*gravitative-amount*)
- [`*repulsive-amount*`](#*repulsive-amount*)
- [`attractive-energy`](#attractive-energy)
- [`distance`](#distance)
- [`energy-by-inf-irr-positions`](#energy-by-inf-irr-positions)
- [`force-layout`](#force-layout)
- [`gravitative-energy`](#gravitative-energy)
- [`layout-energy`](#layout-energy)
- [`line-length-squared`](#line-length-squared)
- [`node-line-distance`](#node-line-distance)
- [`phi`](#phi)
- [`repulsive-energy`](#repulsive-energy)
- [`square`](#square)
- [`sum`](#sum)

### Function Documentation 

#### <a name="attractive-energy"> Function `attractive-energy` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Computes the attractive energy of the given layout. 

#### <a name="distance"> Function `distance` 

Argument List: <tt>([&zwj;[&zwj;x_1 y_1] [&zwj;x_2 y_2]])</tt> 

Documentation: 
>   Returns distance of two points. 

#### <a name="energy-by-inf-irr-positions"> Function `energy-by-inf-irr-positions` 

Argument List: <tt>([&zwj;layout seq-of-inf-irrs])</tt> 

Documentation: 
>   Returns pair of energy function and function returning the n-th
>   partial derivative when given index n. 

#### <a name="force-layout"> Function `force-layout` 

Argument List: <tt>([&zwj;layout] [&zwj;layout iterations])</tt> 

Documentation: 
>   Improves given layout with force layout. 

#### <a name="gravitative-energy"> Function `gravitative-energy` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns the gravitative energy of the given layout. 

#### <a name="layout-energy"> Function `layout-energy` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns the overall energy of the given layout. 

#### <a name="line-length-squared"> Function `line-length-squared` 

Argument List: <tt>([&zwj;[&zwj;x_1 y_1] [&zwj;x_2 y_2]])</tt> 

Documentation: 
>   Returns the square of the length of the line between [x_1, y_1] and [x_2, y_2]. 

#### <a name="node-line-distance"> Function `node-line-distance` 

Argument List: <tt>([&zwj;[&zwj;x y] [&zwj;x_1 y_1] [&zwj;x_2 y_2]])</tt> 

Documentation: 
>   Returns the distance from node [x,y] to the line between [x_1, y_1]
>   and [x_2, y_2]. 

#### <a name="phi"> Function `phi` 

Argument List: <tt>([&zwj;[&zwj;x_1 y_1] [&zwj;x_2 y_2]])</tt> 

Documentation: 
>   Returns the angle between an inf-irreducible node [x_1,y_1]
>   and its upper neighbor [x_2, y_2]. 

#### <a name="repulsive-energy"> Function `repulsive-energy` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Computes the repulsive energy of the given layout. 

#### <a name="sum"> Function `sum` 

Argument List: <tt>([&zwj;bindings expr])</tt> 

Documentation: 
>   Sums up all values of bindings obtained with expr. See doseq. 


## <a name="conexp.layouts.freese"> Public API of conexp.layouts.freese 


### Available Functions 

- [`freese-layout`](#freese-layout)
- [`interactive-freese-layout`](#interactive-freese-layout)

### Function Documentation 

#### <a name="freese-layout"> Function `freese-layout` 

Argument List: <tt>([&zwj;lattice])</tt> 

Documentation: 
>   Returns the Freese Layout of the given lattice. 

#### <a name="interactive-freese-layout"> Function `interactive-freese-layout` 

Argument List: <tt>([&zwj;lattice])</tt> 

Documentation: 
>   Returns the interactive Freese Layout of the given lattice. This is
>   a function of one argument (the projection angle) returning the
>   corresponding layout. 


## <a name="conexp.layouts.layered"> Public API of conexp.layouts.layered 


### Available Functions 

- [`as-chain`](#as-chain)
- [`layer-coordinates`](#layer-coordinates)
- [`simple-layered-layout`](#simple-layered-layout)

### Function Documentation 

#### <a name="as-chain"> Function `as-chain` 

Argument List: <tt>([&zwj;lattice])</tt> 

Documentation: 
>   Returns the layout of lattice as a simple chain. 

#### <a name="layer-coordinates"> Function `layer-coordinates` 

Argument List: <tt>([&zwj;number layer])</tt> 

Documentation: 
>   Assigns coordinates to a given layer such that it is centerer
>   around 0 at height given by number. 

#### <a name="simple-layered-layout"> Function `simple-layered-layout` 

Argument List: <tt>([&zwj;lattice])</tt> 

Documentation: 
>   Simple layered layout for lattice visualization. 


## <a name="conexp.layouts.util"> Public API of conexp.layouts.util 


### Available Functions 

- [`compute-below-above`](#compute-below-above)
- [`discretize-layout`](#discretize-layout)
- [`edges`](#edges)
- [`edges-by-border`](#edges-by-border)
- [`enclosing-rectangle`](#enclosing-rectangle)
- [`fit-layout-to-grid`](#fit-layout-to-grid)
- [`fit-point-to-grid`](#fit-point-to-grid)
- [`lattice->graph`](#lattice->graph)
- [`layers`](#layers)
- [`scale-layout`](#scale-layout)
- [`scale-points-to-rectangle`](#scale-points-to-rectangle)
- [`top-down-elements-in-layout`](#top-down-elements-in-layout)

### Function Documentation 

#### <a name="compute-below-above"> Function `compute-below-above` 

Argument List: <tt>([&zwj;edges])</tt> 

Documentation: 
>   Computes maps mapping endpoints of edges to all elements above and
>   below it. Returns [below,above]. 

#### <a name="discretize-layout"> Function `discretize-layout` 

Argument List: <tt>([&zwj;layout x_cells y_cells])</tt> 

Documentation: 
>   Adjusts the given layout to fit on a grid of x_cells cells in the x
>   coordinate and y_cells cells in the y coordinate. 

#### <a name="edges"> Function `edges` 

Argument List: <tt>([&zwj;lattice])</tt> 

Documentation: 
>   Returns a sequence of pairs of vertices of lattice which are
>   directly neighbored in lattice. 

#### <a name="edges-by-border"> Function `edges-by-border` 

Argument List: <tt>([&zwj;lattice])</tt> 

Documentation: 
>   Computes edges of a lattice by a border algorithm as given by José
>   L. Balcázar and Cristina Tîrnǎucǎ. 

#### <a name="enclosing-rectangle"> Function `enclosing-rectangle` 

Argument List: <tt>([&zwj;points])</tt> 

Documentation: 
>   Returns left lower and right upper edge of the minimal rectangle
>   containing all points. The coordinates are given in a vector of the
>   form [x_min y_min x_max y_max]. 

#### <a name="fit-layout-to-grid"> Function `fit-layout-to-grid` 

Argument List: <tt>([&zwj;layout origin x_pad y_pad])</tt> 

Documentation: 
>   Specifies a grid by a origin point and paddings x_pad and y_pad
>   between two adjacent grid lines in x and y direction
>   respectively. Returns the layout resulting from adjusting the given
>   layout on this layout. 

#### <a name="fit-point-to-grid"> Function `fit-point-to-grid` 

Argument List: <tt>([&zwj;[&zwj;x_origin y_origin] x_pad y_pad [&zwj;x y]])</tt> 

Documentation: 
>   Moves given point [x y] to the next point on the grid given by the
>   origin [x_origin y_origin] and the paddings x_pad and y_pad. 

#### <a name="lattice-&gt;graph"> Function `lattice->graph` 

Argument List: <tt>([&zwj;lattice])</tt> 

Documentation: 
>   Converts given lattice to it's corresponding graph with loops
>   removed. 

#### <a name="layers"> Function `layers` 

Argument List: <tt>([&zwj;lattice])</tt> 

Documentation: 
>   Returns the layers of the given lattice, that is sequence of points
>   with equal depth, starting with the lowest layer. 

#### <a name="scale-layout"> Function `scale-layout` 

Argument List: <tt>([&zwj;[&zwj;x1 y1] [&zwj;x2 y2] layout])</tt> 

Documentation: 
>   Scales given layout to rectangle [x1 y1], [x2 y2]. 

#### <a name="scale-points-to-rectangle"> Function `scale-points-to-rectangle` 

Argument List: <tt>([&zwj;[&zwj;x1 y1] [&zwj;x2 y2] points])</tt> 

Documentation: 
>   Scales the collection of points such that they fit in the
>   rectangle given by [x1 y1] and [x2 y2]. 

#### <a name="top-down-elements-in-layout"> Function `top-down-elements-in-layout` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Returns the elements in layout ordered top down. 


## <a name="conexp.contrib.concept-approximation"> Public API of conexp.contrib.concept-approximation 


  Concept Approximation as described by C. Meschke.

### Available Functions 

- [`apprx-handler`](#apprx-handler)
- [`explore-approximations`](#explore-approximations)

### Function Documentation 

#### <a name="apprx-handler"> Function `apprx-handler` 

Argument List: <tt>([&zwj;ctx known new-impl new-objs cross-handler])</tt> 

Documentation: 
>   Special handler for concept approximation exploration. 

#### <a name="explore-approximations"> Function `explore-approximations` 

Argument List: <tt>([&zwj;context])</tt> 

Documentation: 
>   Performs concept approximation exploration and returns the final context. 


## <a name="conexp.contrib.doc"> Public API of conexp.contrib.doc 


### Available Functions 

- [`conexp-fns-needing-doc`](#conexp-fns-needing-doc)
- [`conexp-namespaces`](#conexp-namespaces)
- [`make-conexp-standard-api-documentation`](#make-conexp-standard-api-documentation)
- [`public-api`](#public-api)
- [`public-api-to-file`](#public-api-to-file)
- [`public-api-to-markdown`](#public-api-to-markdown)

### Function Documentation 

#### <a name="conexp-fns-needing-doc"> Function `conexp-fns-needing-doc` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns function in public conexp-clj API not having documentation. 

#### <a name="make-conexp-standard-api-documentation"> Function `make-conexp-standard-api-documentation` 

Argument List: <tt>([&zwj;file])</tt> 

Documentation: 
>   Generates the standard conexp-clj API documentation and writes it to the specified
>   file 

#### <a name="public-api"> Function `public-api` 

Argument List: <tt>([&zwj;ns])</tt> 

Documentation: 
>   Returns a map of public functions of namespaces to pairs of their argumentation list
>   and their documentation. 

#### <a name="public-api-to-file"> Function `public-api-to-file` 

Argument List: <tt>([&zwj;file & args])</tt> 

Documentation: 
>   Writes the API documentation as generated by public-api-to-markdown to the given file,
>   i.e. redirects standard output to file and applies public-api-to-markdown on args. 

#### <a name="public-api-to-markdown"> Function `public-api-to-markdown` 

Argument List: <tt>([&zwj;] [&zwj;namespaces])</tt> 

Documentation: 
>   Prints to standard out the API documentation of the main namespaces of conexp-clj, or
>   the namespaces provided as argument. 


## <a name="conexp.contrib.fuzzy.fca"> Public API of conexp.contrib.fuzzy.fca 


  Basic definitions for Fuzzy FCA

### Available Functions 

- [`->Fuzzy-Context`](#->Fuzzy-Context)
- [`doubly-scale-fuzzy-context`](#doubly-scale-fuzzy-context)
- [`fuzzy-aprime`](#fuzzy-aprime)
- [`fuzzy-attribute-derivation`](#fuzzy-attribute-derivation)
- [`fuzzy-object-derivation`](#fuzzy-object-derivation)
- [`fuzzy-oprime`](#fuzzy-oprime)
- [`globalization`](#globalization)
- [`make-fuzzy-context`](#make-fuzzy-context)
- [`make-fuzzy-context-from-matrix`](#make-fuzzy-context-from-matrix)
- [`mv->fuzzy-context-nc`](#mv->fuzzy-context-nc)
- [`validity`](#validity)

### Function Documentation 

#### <a name="-&gt;Fuzzy-Context"> Function `->Fuzzy-Context` 

Argument List: <tt>([&zwj;objects attributes incidence])</tt> 

Documentation: 
>   Positional factory function for class conexp.contrib.fuzzy.fca.Fuzzy-Context. 

#### <a name="doubly-scale-fuzzy-context"> Function `doubly-scale-fuzzy-context` 

Argument List: <tt>([&zwj;ctx])</tt> 

Documentation: 
>   Returns the doubly scaled formal context for the given fuzzy
>   context ctx. 

#### <a name="fuzzy-aprime"> Function `fuzzy-aprime` 

Argument List: <tt>([&zwj;context D])</tt> 

Documentation: 
>   Computes the fuzzy derivation of the fuzzy set D of attributes in the given context. 

#### <a name="fuzzy-attribute-derivation"> Function `fuzzy-attribute-derivation` 

Argument List: <tt>([&zwj;context D])</tt> 

Documentation: 
>   Computes the fuzzy derivation of the fuzzy set D of attributes in the given context. 

#### <a name="fuzzy-object-derivation"> Function `fuzzy-object-derivation` 

Argument List: <tt>([&zwj;context C] [&zwj;context C hedge])</tt> 

Documentation: 
>   Computes the fuzzy derivation of the fuzzy set C of objects in the given context, using hedge if
>   given. 

#### <a name="fuzzy-oprime"> Function `fuzzy-oprime` 

Argument List: <tt>([&zwj;context C] [&zwj;context C hedge])</tt> 

Documentation: 
>   Computes the fuzzy derivation of the fuzzy set C of objects in the given context, using hedge if
>   given. 

#### <a name="globalization"> Function `globalization` 

Argument List: <tt>([&zwj;x])</tt> 

Documentation: 
>   Implements globalization. 

#### <a name="make-fuzzy-context"> Function `make-fuzzy-context` 

Argument List: <tt>([&zwj;objects attributes incidence])</tt> 

Documentation: 
>   Creates a fuzzy context from the given attributes. A fuzzy context
>   is nothing else than a Many-Valued Context with real entries between
>   0 and 1. 

#### <a name="make-fuzzy-context-from-matrix"> Function `make-fuzzy-context-from-matrix` 

Argument List: <tt>([&zwj;objects attributes values])</tt> 

Documentation: 
>   Creates a fuzzy context from the given (number of) objects, (number
>   of) attributes and the value table, which must contain only real
>   values between 0 and 1. 

#### <a name="mv-&gt;fuzzy-context-nc"> Function `mv->fuzzy-context-nc` 

Argument List: <tt>([&zwj;mv-ctx])</tt> 

Documentation: 
>   Converts a many-valued-context to a fuzzy context, without checking. 

#### <a name="validity"> Function `validity` 

Argument List: <tt>([&zwj;fuzzy-context A B] [&zwj;fuzzy-context A B hedge])</tt> 

Documentation: 
>   Returns the degree to which the implication A ==> B is true in the
>   given fuzzy-context. A and B are fuzzy subsets of the attributes of
>   fuzzy-context. 


## <a name="conexp.contrib.fuzzy.logics"> Public API of conexp.contrib.fuzzy.logics 


  Basic definitions for fuzzy logics

### Available Functions 

- [`define-fuzzy-operator`](#define-fuzzy-operator)
- [`f-and`](#f-and)
- [`f-impl`](#f-impl)
- [`f-neg`](#f-neg)
- [`f-or`](#f-or)
- [`f-star`](#f-star)
- [`t-norm`](#t-norm)
- [`with-fuzzy-logic`](#with-fuzzy-logic)

### Function Documentation 

#### <a name="define-fuzzy-operator"> Function `define-fuzzy-operator` 

Argument List: <tt>([&zwj;name arity])</tt> 

Documentation: 
>   Defines a fuzzy operator, which throws an
>   UnsupportedOperationException when called. The operator is meant to be
>   rebound. 

#### <a name="t-norm"> Function `t-norm` 

Argument List: <tt>([&zwj;(t-norm-name)])</tt> 

Documentation: 
>   Returns the t-norm and it's residuum corresponding to the name given. 

#### <a name="with-fuzzy-logic"> Function `with-fuzzy-logic` 

Argument List: <tt>([&zwj;norm & body])</tt> 

Documentation: 
>   For the given t-norm norm and the names of the corresponding operators evaluates body in an
>   dynamic environment where the fuzzy logic for norm is in effect. 


## <a name="conexp.contrib.fuzzy.sets"> Public API of conexp.contrib.fuzzy.sets 


  Basic definitions for fuzzy sets

### Available Functions 

- [`->Fuzzy-Set`](#->Fuzzy-Set)
- [`define-fuzzy-set-operation`](#define-fuzzy-set-operation)
- [`fuzzy-difference`](#fuzzy-difference)
- [`fuzzy-intersection`](#fuzzy-intersection)
- [`fuzzy-set`](#fuzzy-set)
- [`fuzzy-set-as-hashmap`](#fuzzy-set-as-hashmap)
- [`fuzzy-set?`](#fuzzy-set?)
- [`fuzzy-subset?`](#fuzzy-subset?)
- [`fuzzy-subsets`](#fuzzy-subsets)
- [`fuzzy-union`](#fuzzy-union)
- [`make-fuzzy-set`](#make-fuzzy-set)
- [`pointwise-fuzzy`](#pointwise-fuzzy)
- [`subsethood`](#subsethood)

### Function Documentation 

#### <a name="-&gt;Fuzzy-Set"> Function `->Fuzzy-Set` 

Argument List: <tt>([&zwj;hashmap])</tt> 

Documentation: 
>   Positional factory function for class conexp.contrib.fuzzy.sets.Fuzzy-Set. 

#### <a name="define-fuzzy-set-operation"> Function `define-fuzzy-set-operation` 

Argument List: <tt>([&zwj;name docstring op])</tt> 

Documentation: 
>   Defines a fuzzy set operation by applying op pointwise to all its
>   arguments. If only one argument is given, it is returned. 

#### <a name="fuzzy-difference"> Function `fuzzy-difference` 

Argument List: <tt>([&zwj;& fuzzy-sets])</tt> 

Documentation: 
>   Difference of fuzzy set. 

#### <a name="fuzzy-intersection"> Function `fuzzy-intersection` 

Argument List: <tt>([&zwj;& fuzzy-sets])</tt> 

Documentation: 
>   Intersection of fuzzy sets. 

#### <a name="fuzzy-set"> Function `fuzzy-set` 

Argument List: <tt></tt> 

Documentation: 
>   Alias for make-fuzzy-set 

#### <a name="fuzzy-set-as-hashmap"> Function `fuzzy-set-as-hashmap` 

Argument List: <tt>([&zwj;fuzzy-set])</tt> 

Documentation: 
>   Returns the hashmap corresponding to the given fuzzy set. 

#### <a name="fuzzy-set?"> Function `fuzzy-set?` 

Argument List: <tt>([&zwj;thing])</tt> 

Documentation: 
>   Tests thing for being a fuzzy set. 

#### <a name="fuzzy-subset?"> Function `fuzzy-subset?` 

Argument List: <tt>([&zwj;fuzzy-set-1 fuzzy-set-2])</tt> 

Documentation: 
>   Returns true iff fuzzy-set-1 is a subset of fuzzy-set-2. 

#### <a name="fuzzy-subsets"> Function `fuzzy-subsets` 

Argument List: <tt>([&zwj;values base-set])</tt> 

Documentation: 
>   Returns all fuzzy subsets of the given fuzzy set base-set with given fuzzy values. 

#### <a name="fuzzy-union"> Function `fuzzy-union` 

Argument List: <tt>([&zwj;& fuzzy-sets])</tt> 

Documentation: 
>   Union of fuzzy sets. 

#### <a name="make-fuzzy-set"> Function `make-fuzzy-set` 

Argument List: <tt></tt> 

Documentation: 
>   Constructs a fuzzy set from a given collection. 

#### <a name="pointwise-fuzzy"> Function `pointwise-fuzzy` 

Argument List: <tt>([&zwj;op fuzzy-sets])</tt> 

Documentation: 
>   Returns a fuzzy set where op is done pointwise on the fuzzy sets. 

#### <a name="subsethood"> Function `subsethood` 

Argument List: <tt>([&zwj;fuzzy-set-1 fuzzy-set-2] [&zwj;fuzzy-set-1 fuzzy-set-2 hedge])</tt> 

Documentation: 
>   Returns the degree to which fuzzy-set-1 is a subset of fuzzy-set-2. Applies hedge to the truth
>   value of an element being in fuzzy-set-1, if given. 


## <a name="conexp.contrib.draw"> Public API of conexp.contrib.draw 


### Available Functions 

- [`add-nodes-with-connections`](#add-nodes-with-connections)
- [`all-inf-add-influenced-nodes`](#all-inf-add-influenced-nodes)
- [`all-nodes-above`](#all-nodes-above)
- [`all-nodes-below`](#all-nodes-below)
- [`all-sup-add-influenced-nodes`](#all-sup-add-influenced-nodes)
- [`conexp.contrib.draw.lattices.proxy$javax.swing.JPanel$WithScene$2dcb4691`](#conexp.contrib.draw.lattices.proxy$javax.swing.JPanel$WithScene$2dcb4691)
- [`conexp.contrib.draw.nodes_and_connections.proxy$java.lang.Object$GInteraction$27fdb2c5`](#conexp.contrib.draw.nodes_and_connections.proxy$java.lang.Object$GInteraction$27fdb2c5)
- [`conexp.contrib.draw.nodes_and_connections.proxy$no.geosoft.cc.graphics.GObject$ff19274a`](#conexp.contrib.draw.nodes_and_connections.proxy$no.geosoft.cc.graphics.GObject$ff19274a)
- [`connection?`](#connection?)
- [`default-node-radius`](#default-node-radius)
- [`do-lines`](#do-lines)
- [`do-nodes`](#do-nodes)
- [`draw-concept-lattice`](#draw-concept-lattice)
- [`draw-lattice`](#draw-lattice)
- [`draw-lattice-to-file`](#draw-lattice-to-file)
- [`draw-layout`](#draw-layout)
- [`draw-on-scene`](#draw-on-scene)
- [`fit-scene-to-layout`](#fit-scene-to-layout)
- [`get-diagram-from-scene`](#get-diagram-from-scene)
- [`get-layout-from-panel`](#get-layout-from-panel)
- [`get-layout-from-scene`](#get-layout-from-scene)
- [`get-name`](#get-name)
- [`get-scene-from-panel`](#get-scene-from-panel)
- [`getScene`](#getScene)
- [`highlight-node`](#highlight-node)
- [`lower-connections`](#lower-connections)
- [`lower-neighbors`](#lower-neighbors)
- [`lower-node`](#lower-node)
- [`make-lattice-editor`](#make-lattice-editor)
- [`move-interaction`](#move-interaction)
- [`move-node-by`](#move-node-by)
- [`move-node-unchecked-to`](#move-node-unchecked-to)
- [`node?`](#node?)
- [`position`](#position)
- [`radius`](#radius)
- [`set-layout-of-scene`](#set-layout-of-scene)
- [`set-node-radius!`](#set-node-radius!)
- [`update-layout-of-scene`](#update-layout-of-scene)
- [`upper-connections`](#upper-connections)
- [`upper-neighbors`](#upper-neighbors)
- [`upper-node`](#upper-node)
- [`zoom-interaction`](#zoom-interaction)

### Function Documentation 

#### <a name="add-nodes-with-connections"> Function `add-nodes-with-connections` 

Argument List: <tt>([&zwj;scn node-coordinate-map node-connections annotation])</tt> 

Documentation: 
>   Adds to scene scn nodes placed by node-coordinate-map and connected
>   via pairs in the sequence node-connections. 

#### <a name="all-inf-add-influenced-nodes"> Function `all-inf-add-influenced-nodes` 

Argument List: <tt>([&zwj;node])</tt> 

Documentation: 
>   Returns all nodes (with weights) which are infimum-additively
>   influenced by node. 

#### <a name="all-nodes-above"> Function `all-nodes-above` 

Argument List: <tt>([&zwj;node])</tt> 

Documentation: 
>   Returns the set of all nodes above node. 

#### <a name="all-nodes-below"> Function `all-nodes-below` 

Argument List: <tt>([&zwj;node])</tt> 

Documentation: 
>   Returns the set of all nodes below node. 

#### <a name="all-sup-add-influenced-nodes"> Function `all-sup-add-influenced-nodes` 

Argument List: <tt>([&zwj;node])</tt> 

Documentation: 
>   Returns all nodes (with weights) which are supremum-additively
>   influenced by node. 

#### <a name="connection?"> Function `connection?` 

Argument List: <tt>([&zwj;thing])</tt> 

Documentation: 
>   Tests whether thing is a connection of a lattice diagram or not. 

#### <a name="default-node-radius"> Function `default-node-radius` 

Argument List: <tt></tt> 

Documentation: 
>   Initial node radius when drawing lattices. 

#### <a name="do-lines"> Function `do-lines` 

Argument List: <tt>([&zwj;[&zwj;line scene] & body])</tt> 

Documentation: 
>   Do whatever with every connection on the scene. Redraws the scene
>   afterwards. 

#### <a name="do-nodes"> Function `do-nodes` 

Argument List: <tt>([&zwj;[&zwj;node scene] & body])</tt> 

Documentation: 
>   Do whatever with every node on the scene. Redraws the scene
>   afterwards. 

#### <a name="draw-concept-lattice"> Function `draw-concept-lattice` 

Argument List: <tt>([&zwj;ctx & args])</tt> 

Documentation: 
>   Draws the concept lattice of a given context, passing all remaining
>   args to draw-lattice. 

#### <a name="draw-lattice"> Function `draw-lattice` 

Argument List: <tt>([&zwj;lattice & args])</tt> 

Documentation: 
>   Draws lattice with given layout. Passes all other parameters to
>   draw-layout. 

#### <a name="draw-lattice-to-file"> Function `draw-lattice-to-file` 

Argument List: <tt>([&zwj;lattice file-name & {:keys [&zwj;layout-fn dimension], :or {layout-fn standard-layout, dimension [&zwj;600 600]}}])</tt> 

Documentation: 
>   Exports layout of given lattice to the given file. 

#### <a name="draw-layout"> Function `draw-layout` 

Argument List: <tt>([&zwj;layout & {:keys [&zwj;visible dimension], :or {visible true, dimension [&zwj;600 600]}}])</tt> 

Documentation: 
>   Draws given layout on a canvas. Returns the frame and the scene (as
>   map). The following options are allowed, their default values are
>   given in parantheses:
>   
>     - visible (true)
>     - dimension [600 600]
>    

#### <a name="draw-on-scene"> Function `draw-on-scene` 

Argument List: <tt>([&zwj;layout])</tt> 

Documentation: 
>   Draws given layout on a GScene and returns it. 

#### <a name="fit-scene-to-layout"> Function `fit-scene-to-layout` 

Argument List: <tt>([&zwj;scene] [&zwj;scene layout])</tt> 

Documentation: 
>   Adjusts scene such that layout fits on it. Uses stored layout if
>   none is given. Calls :image-changed hook. 

#### <a name="get-diagram-from-scene"> Function `get-diagram-from-scene` 

Argument List: <tt>([&zwj;scene])</tt> 

Documentation: 
>   Returns nodes and lines of a scene. 

#### <a name="get-layout-from-panel"> Function `get-layout-from-panel` 

Argument List: <tt>([&zwj;panel])</tt> 

Documentation: 
>   If the given panel contains a lattice editor, return the
>   corresponding layout and nil otherwise. 

#### <a name="get-layout-from-scene"> Function `get-layout-from-scene` 

Argument List: <tt>([&zwj;scn])</tt> 

Documentation: 
>   Returns layout from a scene. 

#### <a name="get-name"> Function `get-name` 

Argument List: <tt>([&zwj;thing])</tt> 

Documentation: 
>   Returns name of thing. 

#### <a name="get-scene-from-panel"> Function `get-scene-from-panel` 

Argument List: <tt>([&zwj;panel])</tt> 

Documentation: 
>   If the given panel contains a lattice editor, returns the
>   corresponding scene, nil otherwise. 

#### <a name="getScene"> Function `getScene` 

Argument List: <tt>([&zwj;this])</tt> 

Documentation: 
>   Returns the associated scene. 

#### <a name="highlight-node"> Function `highlight-node` 

Argument List: <tt>([&zwj;node])</tt> 

Documentation: 
>   Toggles the highlight-state of the given node. 

#### <a name="lower-connections"> Function `lower-connections` 

Argument List: <tt>([&zwj;node])</tt> 

Documentation: 
>   Returns all lower connections of node in a lattice diagram. 

#### <a name="lower-neighbors"> Function `lower-neighbors` 

Argument List: <tt>([&zwj;node])</tt> 

Documentation: 
>   Returns all lower neighbors of node in a lattice diagram. 

#### <a name="lower-node"> Function `lower-node` 

Argument List: <tt>([&zwj;conn])</tt> 

Documentation: 
>   Returns for a connection conn the lower node in a lattice diagram. 

#### <a name="make-lattice-editor"> Function `make-lattice-editor` 

Argument List: <tt>([&zwj;frame layout])</tt> 

Documentation: 
>   Creates a lattice editor with initial layout. 

#### <a name="move-interaction"> Function `move-interaction` 

Argument List: <tt>([&zwj;scene])</tt> 

Documentation: 
>   Standard move interaction for lattice diagrams. Installs
>   :move-start, :move-drag and :move-stop hooks on scene to be called
>   whenever a node is moved. Callbacks get the moved vertex as
>   argument, :move-drag additionally gets the vector by which the given
>   vertex has been moved. 

#### <a name="move-node-by"> Function `move-node-by` 

Argument List: <tt>([&zwj;node dx dy])</tt> 

Documentation: 
>   Moves node by [dx dy] making sure it will not be over some of its
>   upper neighbors or under some of its lower neighbors. 

#### <a name="move-node-unchecked-to"> Function `move-node-unchecked-to` 

Argument List: <tt>([&zwj;node new-x new-y])</tt> 

Documentation: 
>   Moves node to [new-x new-y]. 

#### <a name="node?"> Function `node?` 

Argument List: <tt>([&zwj;thing])</tt> 

Documentation: 
>   Tests whether thing is a node of a lattice diagram or not. 

#### <a name="position"> Function `position` 

Argument List: <tt>([&zwj;node])</tt> 

Documentation: 
>   Returns the position of a node in a lattice diagram. 

#### <a name="radius"> Function `radius` 

Argument List: <tt>([&zwj;node])</tt> 

Documentation: 
>   Returns the radius of a node in a lattice diagram. 

#### <a name="set-layout-of-scene"> Function `set-layout-of-scene` 

Argument List: <tt>([&zwj;scene layout])</tt> 

Documentation: 
>   Sets given layout as current layout of scene. 

#### <a name="set-node-radius!"> Function `set-node-radius!` 

Argument List: <tt>([&zwj;node radius])</tt> 

Documentation: 
>   Sets radius of node. 

#### <a name="update-layout-of-scene"> Function `update-layout-of-scene` 

Argument List: <tt>([&zwj;scene layout])</tt> 

Documentation: 
>   Updates layout according to new layout. The underlying lattice must
>   not be changed. 

#### <a name="upper-connections"> Function `upper-connections` 

Argument List: <tt>([&zwj;node])</tt> 

Documentation: 
>   Returns all upper connections for node in a lattice diagram. 

#### <a name="upper-neighbors"> Function `upper-neighbors` 

Argument List: <tt>([&zwj;node])</tt> 

Documentation: 
>   Returns all upper neighbors of node in a lattice diagram. 

#### <a name="upper-node"> Function `upper-node` 

Argument List: <tt>([&zwj;conn])</tt> 

Documentation: 
>   Returns for a connection conn the upper node in a lattice diagram. 

#### <a name="zoom-interaction"> Function `zoom-interaction` 

Argument List: <tt>([&zwj;scene])</tt> 

Documentation: 
>   Standrd zoom interaction for lattice diagrams. Installs
>   :zoom hook called whenever view changes. Callbacks take no
>   arguments. 


## <a name="conexp.contrib.exec"> Public API of conexp.contrib.exec 


  Executing external programs with a common interface.

### Available Functions 

- [`context-to-file`](#context-to-file)
- [`define-external-program`](#define-external-program)
- [`program-exists?`](#program-exists?)
- [`run-in-shell`](#run-in-shell)
- [`with-context-from-output`](#with-context-from-output)
- [`with-context-from-tmpfile`](#with-context-from-tmpfile)

### Function Documentation 

#### <a name="context-to-file"> Function `context-to-file` 

Argument List: <tt>([&zwj;context format])</tt> 

Documentation: 
>   Writes context to a file using format and returns the name of the
>   file. 

#### <a name="define-external-program"> Function `define-external-program` 

Argument List: <tt>([&zwj;program-name & command-line])</tt> 

Documentation: 
>   Defines program-name to be a function calling an external
>   program. Keywords are treated as arguments to the function and for
>   every keyword there will be a corresponding function argument with
>   the same name (without the colon). Special treatment is given
>   to :input-file, which must be followed by a valid format to
>   write-context. Its argument will be called "context" (thus you
>   shall not name any other argument this way) and will get a context
>   as input. This context will then be written to a file using the give
>   format and this file will be given as an argument to the external
>   process.
>   
>   Returns a map of :exit, :out and :err pointing to corresponding exit
>   value, value at stdout and value at stderr, respectively. 

#### <a name="program-exists?"> Function `program-exists?` 

Argument List: <tt>([&zwj;program-name])</tt> 

Documentation: 
>   Tests whether the given program-name is an executable program in
>   the current path. 

#### <a name="run-in-shell"> Function `run-in-shell` 

Argument List: <tt>([&zwj;& cmdln])</tt> 

Documentation: 
>   Runs given commandline in an external shell process. Returns the
>   output on stdout as result. 

#### <a name="with-context-from-output"> Function `with-context-from-output` 

Argument List: <tt>([&zwj;& body])</tt> 

Documentation: 
>   Runs body, expecting to get a string containing a formal
>   context. Writes this string to a file and reads it in
>   with read-context, returning the result. 

#### <a name="with-context-from-tmpfile"> Function `with-context-from-tmpfile` 

Argument List: <tt>([&zwj;file & body])</tt> 

Documentation: 
>   Creates a new temporary file, executes body and returns the result
>   from calling read-context on file. 


## <a name="conexp.contrib.factor-analysis"> Public API of conexp.contrib.factor-analysis 


  Implements factorization algorithms for contexts.

### Available Functions 

- [`attribute-concepts`](#attribute-concepts)
- [`clear-factors`](#clear-factors)
- [`factorize-context`](#factorize-context)
- [`find-maximal`](#find-maximal)
- [`fuzzy-oplus-a`](#fuzzy-oplus-a)
- [`incidence-cover?`](#incidence-cover?)
- [`object-concepts`](#object-concepts)
- [`oplus-count`](#oplus-count)

### Function Documentation 

#### <a name="attribute-concepts"> Function `attribute-concepts` 

Argument List: <tt>([&zwj;context])</tt> 

Documentation: 
>   Returns the attribute concepts of context. 

#### <a name="clear-factors"> Function `clear-factors` 

Argument List: <tt>([&zwj;factors])</tt> 

Documentation: 
>   From the given factors remove all which are not needed. 

#### <a name="factorize-context"> Function `factorize-context` 

Argument List: <tt>([&zwj;method context & args])</tt> 

Documentation: 
>   Factorize context by given method. Note that the method determines
>   whether the context is a formal context (as in the boolean case) or
>   a many-valued one (as in the fuzzy case). 

#### <a name="find-maximal"> Function `find-maximal` 

Argument List: <tt>([&zwj;values context U D])</tt> 

Documentation: 
>   Find a pair [m v], where m is an attribute and v in (0,1],
>   such that the cardinality of the set returned by fuzzy-oplus-a is
>   maximal. Returns the triple [m v count], where count is the
>   aforementioned cardinality. 

#### <a name="fuzzy-oplus-a"> Function `fuzzy-oplus-a` 

Argument List: <tt>([&zwj;context U D a j])</tt> 

Documentation: 
>   Helper function to compute D\oplus_a j. 

#### <a name="incidence-cover?"> Function `incidence-cover?` 

Argument List: <tt>([&zwj;method context concepts])</tt> 

Documentation: 
>   Returns true iff the given sequence of concepts cover the incidence
>   relation of the given context. 

#### <a name="object-concepts"> Function `object-concepts` 

Argument List: <tt>([&zwj;context])</tt> 

Documentation: 
>   Returns the object concepts of context. 

#### <a name="oplus-count"> Function `oplus-count` 

Argument List: <tt>([&zwj;context U D j])</tt> 

Documentation: 
>   Implements |D oplus j|. D must be subset of the attributes of
>   context, as must #{j}. 


## <a name="conexp.contrib.gui"> Public API of conexp.contrib.gui 


### Available Functions 

- [`gui`](#gui)

### Function Documentation 

#### <a name="gui"> Function `gui` 

Argument List: <tt>([&zwj;& args])</tt> 

Documentation: 
>   Starts the standard gui for conexp-clj. args may be a sequence of
>   parameters given by keywords and values. 


## <a name="conexp.contrib.java"> Public API of conexp.contrib.java 


### Available Functions 

- [`conexp-functions`](#conexp-functions)
- [`dissect-arglist`](#dissect-arglist)
- [`function-signatures`](#function-signatures)
- [`generate-definition`](#generate-definition)
- [`generate-java-interface`](#generate-java-interface)
- [`to-valid-Java-name`](#to-valid-Java-name)

### Function Documentation 

#### <a name="conexp-functions"> Function `conexp-functions` 

Argument List: <tt>([&zwj;ns])</tt> 

Documentation: 
>   Returns a hash-map of function names to vars of ns. The function
>   names are converted by to-valid-Java-name. 

#### <a name="function-signatures"> Function `function-signatures` 

Argument List: <tt>([&zwj;new-name var])</tt> 

Documentation: 
>   Returns sequence of function signatures, each being suitable for
>   gen-class. 

#### <a name="generate-definition"> Function `generate-definition` 

Argument List: <tt>([&zwj;prefix new-name orig-name & arglists])</tt> 

Documentation: 
>   Generates function definition with name new-name, calling orig-name
>   with supplied arguments. Prepends prefix before new-name 

#### <a name="generate-java-interface"> Function `generate-java-interface` 

Argument List: <tt>([&zwj;orig-ns new-ns file-name])</tt> 

Documentation: 
>   Given a name of a file generates the code for the Java interface in
>   that file. After this has been compiled it can be used to call
>   conexp-clj functions from Java. 

#### <a name="to-valid-Java-name"> Function `to-valid-Java-name` 

Argument List: <tt>([&zwj;old-name])</tt> 

Documentation: 
>   Convert old-name to a valid Java variable name. 


## <a name="conexp.contrib.nonsense"> Public API of conexp.contrib.nonsense 


  What it says.

### Available Functions 

- [`crossfoot`](#crossfoot)
- [`doomsday-in-month`](#doomsday-in-month)
- [`doomsday-of-year`](#doomsday-of-year)
- [`factorial`](#factorial)
- [`leap-year?`](#leap-year?)
- [`number->weekday`](#number->weekday)
- [`prime?`](#prime?)
- [`weekday-of-date`](#weekday-of-date)

### Function Documentation 

#### <a name="crossfoot"> Function `crossfoot` 

Argument List: <tt>([&zwj;n])</tt> 

Documentation: 
>   Returns the crossfoot of n. 

#### <a name="doomsday-in-month"> Function `doomsday-in-month` 

Argument List: <tt>([&zwj;year month])</tt> 

Documentation: 
>   Returns number of day in month doomsday occurs on. 

#### <a name="doomsday-of-year"> Function `doomsday-of-year` 

Argument List: <tt>([&zwj;year])</tt> 

Documentation: 
>   Returns the doomsday of the year given. 

#### <a name="factorial"> Function `factorial` 

Argument List: <tt>([&zwj;n])</tt> 

Documentation: 
>   Returns n!. 

#### <a name="leap-year?"> Function `leap-year?` 

Argument List: <tt>([&zwj;year])</tt> 

Documentation: 
>   Returns true iff year is a leap year. 

#### <a name="number-&gt;weekday"> Function `number->weekday` 

Argument List: <tt>([&zwj;n])</tt> 

Documentation: 
>   Converts number to weekday. 

#### <a name="prime?"> Function `prime?` 

Argument List: <tt>([&zwj;n])</tt> 

Documentation: 
>   Returns true iff n is prime with a certainty of (- 1 (/ 1 (expt 2 1000))) 

#### <a name="weekday-of-date"> Function `weekday-of-date` 

Argument List: <tt>([&zwj;year month day])</tt> 

Documentation: 
>   Returns the day of week of the given date, in Gregorian calendar. 


## <a name="conexp.contrib.profiler"> Public API of conexp.contrib.profiler 


  Provides simple function for statistical and instrumental profiling.

### Available Functions 

- [`*profile-data*`](#*profile-data*)
- [`add-profiled-thread`](#add-profiled-thread)
- [`current-thread`](#current-thread)
- [`delete-profiled-thread`](#delete-profiled-thread)
- [`enable-profiling`](#enable-profiling)
- [`get-profiled-data`](#get-profiled-data)
- [`get-root-thread-group`](#get-root-thread-group)
- [`get-thread-list`](#get-thread-list)
- [`pprint-profiling`](#pprint-profiling)
- [`print-summary`](#print-summary)
- [`prof`](#prof)
- [`profile`](#profile)
- [`profiled-data`](#profiled-data)
- [`profilers`](#profilers)
- [`run-and-profile`](#run-and-profile)
- [`show-profiled-threads`](#show-profiled-threads)
- [`show-profiling`](#show-profiling)
- [`stackdump`](#stackdump)
- [`start-profiling`](#start-profiling)
- [`stop-profiling`](#stop-profiling)
- [`summarize`](#summarize)
- [`tick-thread`](#tick-thread)
- [`with-profile-data`](#with-profile-data)
- [`with-profiled-fns`](#with-profiled-fns)
- [`with-profiler`](#with-profiler)

### Function Documentation 

#### <a name="add-profiled-thread"> Function `add-profiled-thread` 

Argument List: <tt>([&zwj;thread period])</tt> 

Documentation: 
>   Adds thread to the currently profiled threads and starts
>   profiling it. 

#### <a name="current-thread"> Function `current-thread` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns the current thread. 

#### <a name="delete-profiled-thread"> Function `delete-profiled-thread` 

Argument List: <tt>([&zwj;thread])</tt> 

Documentation: 
>   Removes thread from the currently profiled threads. Returns the
>   collected data. 

#### <a name="enable-profiling"> Function `enable-profiling` 

Argument List: <tt></tt> 

Documentation: 
>   Set this to false before loading/compiling to omit
>   profiling code. 

#### <a name="get-profiled-data"> Function `get-profiled-data` 

Argument List: <tt>([&zwj;thread])</tt> 

Documentation: 
>   Returns the profiled data for thread or nil if thread is not
>   profiled. 

#### <a name="get-root-thread-group"> Function `get-root-thread-group` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns the root ThreadGroup. 

#### <a name="get-thread-list"> Function `get-thread-list` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns the list of all threads currently running. 

#### <a name="pprint-profiling"> Function `pprint-profiling` 

Argument List: <tt>([&zwj;thread data overall-count period threshold])</tt> 

Documentation: 
>   Pretty prints the given profiling data, given as a sequence of
>   [name hitcount] pairs. 

#### <a name="print-summary"> Function `print-summary` 

Argument List: <tt>([&zwj;profile-summary])</tt> 

Documentation: 
>   Prints a table of the results returned by summarize. 

#### <a name="prof"> Function `prof` 

Argument List: <tt>([&zwj;name & body])</tt> 

Documentation: 
>   If enable-profiling is true, wraps body in profiling code.
>   Returns the result of body. Profile timings will be stored in
>   *profile-data* using name, which must be a keyword, as the key.
>   Timings are measured with System/nanoTime. 

#### <a name="profile"> Function `profile` 

Argument List: <tt>([&zwj;& body])</tt> 

Documentation: 
>   Runs body with profiling enabled, then prints a summary of results. Returns nil. 

#### <a name="profiled-data"> Function `profiled-data` 

Argument List: <tt></tt> 

Documentation: 
>   Contains for every thread a hash-map mapping method
>   names to the number of occurences found while profiling. 

#### <a name="profilers"> Function `profilers` 

Argument List: <tt></tt> 

Documentation: 
>   Contains for every thread the profiling thread. 

#### <a name="run-and-profile"> Function `run-and-profile` 

Argument List: <tt>([&zwj;f])</tt> 

Documentation: 
>   Runs the given function f, which must be a function of no
>   arguments, in a separate thread and profiles it. Returns the new
>   thread. 

#### <a name="show-profiled-threads"> Function `show-profiled-threads` 

Argument List: <tt>([&zwj;])</tt> 

Documentation: 
>   Returns a sequence of all profiled threads. 

#### <a name="show-profiling"> Function `show-profiling` 

Argument List: <tt>([&zwj;& {:keys [&zwj;thread pattern threshold], :or {thread (current-thread), pattern #".*", threshold -1.0}}])</tt> 

Documentation: 
>   Show the statistics collected so far while profiling
>   thread. Options include
>   
>     :thread    for the thread to be profiled (default
>                  to (current-thread)),
>     :pattern   for filtering the output via re-find
>     :threshold for the minimum precentage a function has
>                  to ticked to be shown. 

#### <a name="stackdump"> Function `stackdump` 

Argument List: <tt>([&zwj;thread])</tt> 

Documentation: 
>   Returns a sequence of names representing the stack of the given
>   thread. 

#### <a name="start-profiling"> Function `start-profiling` 

Argument List: <tt>([&zwj;& {:keys [&zwj;thread period], :or {thread (current-thread), period 100}}])</tt> 

Documentation: 
>   Starts profiling the given thread. Options include
>   
>      :thread for the thread to be profiled (defaults
>              to (current-thread)),
>      :period for the period between two ticks, given in ms, defaults
>              to 100 

#### <a name="stop-profiling"> Function `stop-profiling` 

Argument List: <tt>([&zwj;& {:keys [&zwj;thread], :or {thread (current-thread)}}])</tt> 

Documentation: 
>   Stops profiling the given thread. The :thread keyword argument may
>   be given to indicate which profiling to stop, defaults
>   to (current-thread). 

#### <a name="summarize"> Function `summarize` 

Argument List: <tt>([&zwj;profile-data])</tt> 

Documentation: 
>   Takes the raw data returned by with-profile-data and returns a map
>   from names to summary statistics. Each value in the map will look
>   like:
>   
>    {:mean ..., :min ..., :max ..., :count ..., :sum ...}
>   
>   :mean, :min, and :max are how long the profiled section took to run,
>   in nanoseconds. :count is the total number of times the profiled
>   section was executed. :sum is the total amount of time spent in the
>   profiled section, in nanoseconds. 

#### <a name="tick-thread"> Function `tick-thread` 

Argument List: <tt>([&zwj;thread])</tt> 

Documentation: 
>   Adds current stack trace to profiled data of thread. 

#### <a name="with-profile-data"> Function `with-profile-data` 

Argument List: <tt>([&zwj;& body])</tt> 

Documentation: 
>   Executes body with *profile-data* bound to an atom of a new map.
>   Returns the raw profile data as a map. Keys in the map are profile
>   names (keywords), and values are lists of elapsed time, in
>   nanoseconds. 

#### <a name="with-profiled-fns"> Function `with-profiled-fns` 

Argument List: <tt>([&zwj;fns & body])</tt> 

Documentation: 
>   Runs code in body with all given functions being profiled. Each
>   function will be instrumentalized to measure its invocations. 

#### <a name="with-profiler"> Function `with-profiler` 

Argument List: <tt>([&zwj;starting-options output-options & body])</tt> 

Documentation: 
>   Runs given body under the supervision of the profiler and prints
>   the result. starting-options will be given to start-profiling,
>   output-options will be given to show-profiling. 


## <a name="conexp.contrib.retracts"> Public API of conexp.contrib.retracts 


  Package for computing retracts from formal contexts.

### Available Functions 

- [`endofunctions-as-hash`](#endofunctions-as-hash)
- [`endofunctions-by-homomorphism`](#endofunctions-by-homomorphism)
- [`endofunctions-from-hash`](#endofunctions-from-hash)
- [`homomorphism-by-csc`](#homomorphism-by-csc)
- [`homomorphisms-by-cscs`](#homomorphisms-by-cscs)
- [`pprint-retracts`](#pprint-retracts)
- [`retract-to-pprint-str`](#retract-to-pprint-str)
- [`retract?`](#retract?)
- [`retracts`](#retracts)

### Function Documentation 

#### <a name="endofunctions-as-hash"> Function `endofunctions-as-hash` 

Argument List: <tt>([&zwj;context hom])</tt> 

Documentation: 
>   Given a homomorphisms which has been obtained by a compatible
>   subcontext this function computes a hash representing all
>   endofunctions of context originating from this homomorphism. The
>   result may be thought of as a "multifunction". 

#### <a name="endofunctions-by-homomorphism"> Function `endofunctions-by-homomorphism` 

Argument List: <tt>([&zwj;context hom])</tt> 

Documentation: 
>   Returns all endofunctions obtained by homomorphism. 

#### <a name="endofunctions-from-hash"> Function `endofunctions-from-hash` 

Argument List: <tt>([&zwj;endo-hash])</tt> 

Documentation: 
>   Computes all endofunctions (as a hash) in endo-hash. 

#### <a name="homomorphism-by-csc"> Function `homomorphism-by-csc` 

Argument List: <tt>([&zwj;subcontext])</tt> 

Documentation: 
>   Returns the homomorphisms obtained by resticting every concept to the
>   given compatible subcontext. (csc == compatible subcontext) 

#### <a name="homomorphisms-by-cscs"> Function `homomorphisms-by-cscs` 

Argument List: <tt>([&zwj;context])</tt> 

Documentation: 
>   Returns all homomorphisms originating from compatible
>   subcontexts. context has to be reduced. 

#### <a name="pprint-retracts"> Function `pprint-retracts` 

Argument List: <tt>([&zwj;context])</tt> 

Documentation: 
>   Pretty prints retracts of context 

#### <a name="retract-to-pprint-str"> Function `retract-to-pprint-str` 

Argument List: <tt>([&zwj;retract])</tt> 

Documentation: 
>   Returns a string for pretty printing a retract of a context. 

#### <a name="retract?"> Function `retract?` 

Argument List: <tt>([&zwj;context endo])</tt> 

Documentation: 
>   Tests whether given concept endofunction, given as hash-map, is a
>   retract of context or not. 

#### <a name="retracts"> Function `retracts` 

Argument List: <tt>([&zwj;context])</tt> 

Documentation: 
>   Returns all retracts of context as computed by the algorithm of
>   Felix Kästner. 
