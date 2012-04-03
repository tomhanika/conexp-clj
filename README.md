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

(missing)


How to Run
----------

(missing)


Compilation Instructions for conexp-clj
---------------------------------------

To compile conexp-clj from source you need leiningen (at least version 1.2.0),
a build tool for clojure. To get it just issue

    $ wget http://github.com/technomancy/leiningen/raw/stable/bin/lein

put the file lein in your path, make it executable and run

    $ lein self-install

That's it. Note that this works for Linux and Unix systems. For
Windows there is some experimental version of leiningen, please see
the corresponding website for this.

Now switch in the source directory of conexp-clj and run

    $ lein deps

This will download any missing jar files needed for conexp-clj to run. With
this you can now run conexp-clj directly from source and hack in its internals!

For a fast repl run

    $ lein repl

If you want a swank server, run

    $ lein plugin install swank-clojure «VERSION»
    $ lein swank
    ...
    Connection opened on localhost port 4005.

and connnect your Emacs (or whatever) to Port 4005 at 127.0.0.1. See the
documentation of swank-clojure for more details on this.

To create a standalone zip just run `make` in the top source directory. This
will do everything to create a zip archive containing a compiled version of
conexp-clj. You can also run this compiled version directly by invoking
./conexp-clj/bin/conexp-clj.sh.



Contributing Authors
--------------------

Currently conexp-clj is developed and maintained by

  * Daniel Borchmann

Additional Contributors are

  * Immanuel Albrecht (Context Editor Plugin for the GUI)
  * Stefan Borgwardt  (Shared Intents)
