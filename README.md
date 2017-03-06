# conexp-clj [![Build Status](https://travis-ci.org/exot/conexp-clj.svg?branch=dev)](https://travis-ci.org/exot/conexp-clj)

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

Note that conexp-clj is not a high-performance tool for Formal Concept Analysis.  If you
want this, check out Uta Priss'
[website on FCA software](http://www.fcahome.org.uk/fcasoftware.html).


## Prerequisites

You need

* a Jave Runtime Environment (≥ 1.6)
* [Leiningen](http://github.com/technomancy/leiningen) (≥ 2.0.0) if you want to run
  conexp-clj from source


## How to Run

The recommended way to run conexp-clj is to download a
[pre-compiled version](https://wwwtcs.inf.tu-dresden.de/~borch/downloads/conexp-clj/),
unpacking the zip file and puting the contained `bin` directory into you path.
You can then run

    $ conexp-clj

from you command line (without the "$") to get a bare conexp-clj repl.  If you want to try
the experimental GUI, you can use

    $ conexp-clj --gui

instead.


## Documentation

The current main source of documentation on conexp-clj is its
[Wiki](http://github.com/exot/conexp-clj/wiki).  Additionally, for general help on a
function `f`, you can use the clojure function `doc` with

~~~
(doc f)
~~~

For finding functions you may find useful, you can use `find-doc`

~~~
(find-doc "Whatever you may find useful")
~~~

Additional Documentation:

- Basic example files: those files cover

  * [basics](doc/examples/01-basics.clj)
  * [formal contexts](doc/examples/02-contexts.clj)
  * [lattices](doc/examples/03-lattices.clj)
  * [IO](doc/examples/04-io.clj)
  * [implications](doc/examples/05-implications.clj)

  They have been written by Sebastian Böhm.

- Advances example files:

 * [Attribute Exploration](doc/examples/exploration.clj)
   a demonstration how attribute exploration can be done in conexp-clj.
 * [Fuzzy FCA](doc/examples/fuzzy.clj),
   a sample file to show how to use fuzzy FCA with conexp-clj
 * [Factor Analysis](doc/examples/factor-analysis.clj),
   a small program that demonstrates how to use conexp-clj for factorizing contexts
 * [Formal Contexts for Implications](doc/examples/implication-closure.clj),
   computing a context for a set of implications
 * A
   [Formal Context of Functions](doc/examples/function-context.clj),
   see the paper by Artem Revenko and Sergej Kuznetzov for the CLA2010
 * [Permutations as Formal Context](doc/examples/permutation-context.clj),
   computes a context whose concept lattice is isomorphic to the lattice of permutations on the set
   \{0,...,n\}.
 * [Tamari Lattice](doc/examples/tamari-lattice.clj),
   the lattice of all bracketings of n+1 symbols (a.k.a. the Tamari Lattice of parameter n)

- A [Tutorial](doc/icfca-2013-tutorial), given
  at the 11th International Conference on Formal Concept Analysis


## Running conexp-clj from source

To run conexp-clj from source, switch in the source directory of conexp-clj and run

    $ lein deps

This will download any missing jar files needed for conexp-clj to run.  To quickly obtain
a repl just issue

    $ lein repl

If you want a more sophisticated repl, you may try
[nrepl.el](http://github.com/kingtim/nrepl.el).


## Compilation Instructions for conexp-clj

To create a standalone zip on your own, just run `make` in the top source directory. This
will (should) do everything to create a zip archive containing a compiled version of
conexp-clj. You can also run this compiled version directly by invoking
./conexp-clj/bin/conexp-clj (after unpacking the .zip file.)


## Contributing Authors

See [AUTHORS.md](AUTHORS.md).

## License

Copyright ⓒ 2009—2017 Daniel Borchmann

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
