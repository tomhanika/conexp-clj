conexp-clj
==========

This is conexp-clj, an attempt to rewrite the famous ConExp to extend it with
new ideas.

The project has been started by Daniel Borchmann under supervision of Christian
Meschke as part of the DFG project GA 216/10-1.

For more information, you can visit conexp-clj's website at
http://daniel.kxpq.de/math/conexp-clj/.


Features
--------

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


Prerequisites
-------------

You need

* a Jave Runtime Environment (≥ 1.6)
* [Leiningen](http://github.com/technomancy/leiningen) (≥ 2.0.0) if you want to run
  conexp-clj from source


How to Run
----------

The recommended way to run conexp-clj is to download the pre-compiled version
[here](https://github.com/exot/conexp-clj/downloads).  Just unpack the zip file and put
the contained `bin` directory in you path.  You can then run

    $ conexp-clj
    
from you command line (without the "$") to get a bare conexp-clj repl.  If you want to try
the experimental GUI, you can use

    $ conexp-clj --gui
    
instead.


Running conexp-clj from source
------------------------------

To run conexp-clj from source, switch in the source directory of conexp-clj and run

    $ lein deps

This will download any missing jar files needed for conexp-clj to run.  To quickly obtain
a repl just issue

    $ lein repl

If you want a more sophisticated repl, you may try
[nrepl.el](http://github.com/kingtim/nrepl.el).


Compilation Instructions for conexp-clj
---------------------------------------

To create a standalone zip on your own, just run `make` in the top source directory. This
will (should) do everything to create a zip archive containing a compiled version of
conexp-clj. You can also run this compiled version directly by invoking
./conexp-clj/bin/conexp-clj (after unpacking the .zip file.)


Contributing Authors
--------------------

Currently conexp-clj is developed and maintained by

  * Daniel Borchmann

Additional Contributors are

  * Immanuel Albrecht (Context Editor Plugin for the GUI)
  * Stefan Borgwardt  (Shared Intents)
  * Gleb Kanterov (interval-scale)

License
-------

Copyright ⓒ 2009—2012 Daniel Borchmann

Distributed under the Eclipse Public License.
