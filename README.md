# conexp-clj [![Build Status](https://travis-ci.org/tomhanika/conexp-clj.svg?branch=dev)](https://travis-ci.org/tomhanika/conexp-clj)

This is conexp-clj, a general purpose software tool for [Formal Concept
Analysis](http://www.upriss.org.uk/fca/fca.html).

The project has been started by Daniel Borchmann under supervision of Christian
Meschke as part of the DFG project GA 216/10-1.


## Features

conexp-clj is a pocket-calculator for Formal Concept Analysis.  Its main purpose is to
enable nontrivial examples to be computed easily.

conexp-clj features include:

* Basic Operations on Formal Contexts
* Relational Algebra with Formal Contexts
* Transparent IO for Formal Contexts (in development)
* Scaling for Many-Valued Contexts
* Implicational Theory and Basic Attribute Exploration
* NextClosure (of course)
* Computing Luxenburger-Bases and Iceberg Concept Sets
* IO for Many-Valued Contexts
* Lattice Layouts and Lattice IO (some...)
* A bit of Fuzzy-FCA
* Interface for sage

Note that conexp-clj is not a high-performance tool for Formal Concept Analysis.
If you want this, check out Uta Priss' [website on FCA
software](http://www.upriss.org.uk/fca/fcasoftware.html).


## Prerequisites

You need

* a Jave Runtime Environment (≥ 1.6)
* [Leiningen](http://github.com/technomancy/leiningen) (≥ 2.0.0) if you want to run
  conexp-clj from source


## How to Run

The recommended way to run conexp-clj is to download a
[pre-compiled version](http://algebra20.de/conexp-clj-2.0.0-RC1.jar).

You can then run it through

    $ java -jar conexp-clj-2.0.0-RC1.jar

from you command line (without the "$") to get a bare conexp-clj repl.  If you want to try
the experimental GUI, you can use

    $ java -jar conexp-clj-2.0.0-RC1.jar -g

instead.


## Documentation

1. [Getting Started](doc/Getting-Started.md)
2. Don't Bother Me with Theory, I Want to Do Stuff! (aka: Tutorials)
   1. [A Gentle Introduction](doc/tutorials/icfca-2013/icfca2013-tutorial-live.org) (ICFCA 2013)
   2. [Compute the Canonical Base from a Formal Context that is given in CSV Format](doc/tutorials/How-to-compute-the-Canonical-Base-from-a-Context-given-in-CSV-Format.md)
3. [Basic Notation and Syntax](doc/Basic-Notation-and-Syntax.md)
4. Basic Usage
   1. [Creating and Working with Formal Contexts](doc/Formal-Contexts.md)
   2. [Concept Lattices](doc/Concept-Lattices.md)
   3. [IO](doc/IO.md)
   4. [Implications](doc/Implications.md)
   5. [Exploration](doc/Exploration.md)
   6. [Scaling Many-Valued Contexts](doc/Scaling-Many-Valued-Contexts.md)
5. Advanced Usage
   1. [Factorizing Formal Contexts](doc/code/factor-analysis.clj)
   2. [Fuzzy FCA in conexp-clj](doc/code/fuzzy.clj)
   3. [pq-cores](doc/pq-cores-in-Formal-Contexts.md)
6. Use cases of conexp-clj
   1. [Formal Contexts from Implications](doc/code/implication-closure.clj)
   2. [A Formal Context of Functions](doc/code/function-context.clj)
   3. [Context of All Permutations on a Finite Set](doc/code/permutation-context.clj)
   4. [The Tamari Lattice](doc/code/tamari-lattice.clj)
7. Interoperability
   1. [Calling conexp-clj functions from Java](doc/Java.md)
   2. [REST-API Usage](doc/REST-API-usage.md)
8. [API documentation](doc/API.md)


## Running conexp-clj from source

To run conexp-clj from source, switch in the source directory of conexp-clj and run

    $ lein deps

This will download any missing jar files needed for conexp-clj to run.  To quickly obtain
a repl just issue

    $ lein repl

If you want a more sophisticated repl, you may try
[cider](https://github.com/clojure-emacs/cider).


## Compilation Instructions for conexp-clj

To create a standalone zip on your own, just run `make` in the top source
directory. This will (should) do everything to create a zip archive containing a
compiled version of conexp-clj. You can also run this compiled version directly
by invoking `./conexp-clj/bin/conexp-clj` (after unpacking the .zip file.)


## Contributing Authors

See [AUTHORS.md](AUTHORS.md).

## License

Copyright ⓒ 2009—2018 Daniel Borchmann, 2018–2020 Tom Hanika

Distributed under the Eclipse Public License.

This program uses an adapted version of
the [G library](http://geosoft.no/graphics/index.html), a 2D graphics library
and rendering engine for Java, ⓒ 2009 GeoSoft, licensed under
the [GNU Lesser General Public License](http://www.gnu.org/copyleft/lesser.html)
(LGPL).  Modifications to the original version of G are only concerned with
exposing internals necessary for conexp-clj to work.  The modified version of G
is again licensed under LGPL.

This program uses parts of the [LatDraw library](http://latdraw.org), ⓒ 2002
Ralph Freese.
