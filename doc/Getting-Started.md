## Running conexp-clj from pre-compiled binaries

To run `conexp-clj`, a Java Runtime Environment with version ≥ 1.8.  The
recommended way to run `conexp-clj` then is to download a [pre-compiled
version](http://algebra20.de/conexp-clj-2.0.0-RC1.jar).

You can then run it through

    java -jar conexp-clj-2.0.0-RC1.jar

This will get you a prompt for conexp-clj, much like

    conexp.main=>

You can now use all the power of formal concept analysis from `conexp-clj`, and also everything Clojure provides.  For example, you can compute the value of the expression `1 + 1` as

    conexp.main=> (+ 1 1)
    2

(where you do not type the `conexp.main=>` and the 2 is the result of the evaluation)


## Running conexp-clj directly from source


It is also possible to get command line access for conexp-clj directly from its source.  Getting this to work requires the following three (easy) steps:

1. Get the [source code](http://github.com/tomhanika/conexp-clj) of conexp-clj
2. Get [Leiningen](https://github.com/technomancy/leiningen)

To run conexp-clj from source, switch in the source directory of conexp-clj and run

    $ lein deps

This will download any missing jar files needed for conexp-clj to run.  To quickly obtain
a repl just issue

    $ lein repl

This will give you a command prompt as in the previous case.

If you want a more sophisticated repl, you may try
[cider](https://github.com/clojure-emacs/cider).

## File Based Access

Typing in long sequences of commands is tedious, and conexp-clj inherits from Clojure the possibility to compile and run complex programs.  Everything you can type into the prompt can also be written into a file, say `file.clj`.  To run (and compile) this file, just type do

     user=> (load-file "file.clj")
     ...


## Graphical User Interface

It is also possible to use conexp-clj's incomplete GUI fragment.  You can start it running the following code in you favourite shell

    java -jar conexp-clj-2.0.0-RC1.jar -g

But please note that the GUI is not only (inherently) limited in its functionality, but also quite (not to say: really) buggy.

## Online Documentation

For general help on a function `f`, you can use the clojure function `doc` with

```clojure
(doc f)
```

For finding functions you may find useful, you can use `find-doc`

```clojure
(find-doc "Whatever you may find useful")
```
