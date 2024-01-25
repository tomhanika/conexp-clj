# conexp-clj [![Build Status](https://img.shields.io/github/actions/workflow/status/tomhanika/conexp-clj/run-tests.yaml?branch=dev&label=build)](https://github.com/tomhanika/conexp-clj/actions/workflows/run-tests.yaml) [![built with nix](https://img.shields.io/static/v1?logo=nixos&logoColor=white&label=&message=Built%20with%20Nix&color=41439a)](https://builtwithnix.org)

This is conexp-clj, a general purpose software tool for [Formal Concept
Analysis](http://www.upriss.org.uk/fca/fca.html).    Its main purpose is to
enable nontrivial examples to be computed easily, but it can be used for much,
much more.


## Documentation

1. [Getting Started](doc/Getting-Started.org)
2. Don't Bother Me with Theory, I Want to Do Stuff! (aka: Tutorials)
   1. [A Gentle Introduction](doc/tutorials/icfca-2013/icfca2013-tutorial-live.org) (ICFCA 2013)
   2. [Compute the Canonical Base from a Formal Context that is given in CSV Format](doc/tutorials/How-to-compute-the-Canonical-Base-from-a-Context-given-in-CSV-Format.org)
3. A more complete overview over `conexp-clj`
   1. [Notation and Syntax](doc/Basic-Notation-and-Syntax.org)
   2. [Creating and Working with Formal Contexts](doc/Formal-Contexts.org)
   3. [Concept Lattices](doc/Concept-Lattices.org)
   4. [IO for Formal Contexts](doc/IO.org)
   5. [Implications](doc/Implications.org)
   6. [Exploration](doc/Exploration.org)
   7. [Scaling Many-Valued Contexts](doc/Scaling-Many-Valued-Contexts.org)
4. Example use cases of `conexp-clj`
   1. [Formal Contexts from Implications](doc/code/implication-closure.clj)
   2. [A Formal Context of Functions](doc/code/function-context.clj)
   3. [Context of All Permutations on a Finite Set](doc/code/permutation-context.clj)
   4. [The Tamari Lattice](doc/code/tamari-lattice.clj)
   5. [Preconcept Covers](doc/code/covering-preconcepts.clj)
   6. [Number of Elements of the Free Distributive
      Lattice](doc/code/free-distributive-lattice.clj)
   7. [Counting Linear Extensions](doc/code/linear_extensions.clj)
   8. [Computing Traces in Contexts](doc/code/trace-context.clj)
   9. [Counting Quasiorders](doc/code/quasiorders.clj)
   10. [Rudolph's Algorithm for Computing Bases](doc/code/rudolph_computation.clj)
5. Advanced Topics
   1. [pq-cores](doc/pq-cores-in-Formal-Contexts.md)
   2. [REST-API Usage](doc/REST-API-usage.md)
   3. [triadic-exploration](doc/Triadic-Exploration.org)
   4. [protoconcepts](doc/Protoconcepts.org)
   5. [Incomplete Contexts](doc/IncompleteContexts.org)
   6. [Factorization of Formal Contexts](doc/Factorization-of-Formal-Contexts.md)
6. [API documentation](doc/API.md)
7. [Development](doc/Development.org)


## History

The project has been started by Daniel Borchmann under supervision of Christian
Meschke as part of the DFG project GA 216/10-1.  It has since been developed
further into a general purpose FCA tool by Daniel Borchmann until his departure
from academia in 2017.  From then on, Tom Hanika took over and is still the
principal maintainer of `conexp-clj`.


## Limitations

Note that `conexp-clj` is not a high-performance tool for Formal Concept
Analysis and may sometimes be considerably slower then comparable tools.  If you
want more performance, check out Uta Priss' [website on FCA
software](http://www.upriss.org.uk/fca/fcasoftware.html).


## Contributing Authors

See [AUTHORS.md](AUTHORS.md).

## How to cite `conexp-clj`?
If you have used `conexp-clj` for your scientific work, the developers
would appreciate if you use the following reference.

```
@inproceedings{DBLP:conf/icfca/HanikaH19,
  author    = {Tom Hanika and
               Johannes Hirth},
  editor    = {Diana Cristea and
               Florence Le Ber and
               Rokia Missaoui and
               L{\'{e}}onard Kwuida and
               Baris Sertkaya},
  title     = {Conexp-Clj - {A} Research Tool for {FCA}},
  booktitle = {Supplementary Proceedings of {ICFCA} 2019 Conference and Workshops,
               Frankfurt, Germany, June 25-28, 2019},
  series    = {{CEUR} Workshop Proceedings},
  volume    = {2378},
  pages     = {70--75},
  publisher = {CEUR-WS.org},
  year      = {2019},
  url       = {http://ceur-ws.org/Vol-2378/shortAT8.pdf},
  timestamp = {Wed, 12 Feb 2020 16:44:55 +0100},
  biburl    = {https://dblp.org/rec/conf/icfca/HanikaH19.bib},
  bibsource = {dblp computer science bibliography, https://dblp.org}
}
```

## License

Copyright ⓒ 2009—2018 Daniel Borchmann, 2018–2023 Tom Hanika

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
