There are multiple ways to work with conexp-clj, and we shall describe to most common ones here.

## Command Line Access 

Probably the easiest way to get started with conexp-clj is to use its command line.  For this, get a pre-compiled version of conexp-clj from its [Download Page](http://www.math.tu-dresden.de/~borch/downloads/).  Extract the corresponding `zip` file and switch into the newly created directory.  Then run the following command in a shell (where `$` denotes the shell prompt)
    
    $ ./bin/conexp-clj

This will get you a prompt for conexp-clj, much like

    user=>

You can now use all the power of formal concept analysis from conexp-clj, and also everything Clojure provides.  For example, you can compute the value of the expression `1 + 1` as

    user=> (+ 1 1)
    2

(where you do not type the `user=>` and the 2 is the result of the evaluation)

## Command Line Access from Source

It is also possible to get command line access for conexp-clj directly from its source.  Getting this to work requires the following three (easy) steps:

1. Get the [source code](http://github.com/tomhanika/conexp-clj) of conexp-clj
2. Get [Leiningen](https://github.com/technomancy/leiningen)
3. Run the following command from the toplevel directory of the source code of conexp-clj

        $ lein repl

   This will give you a command prompt as in the previous case

        user=>

   However, to get the functionality of conexp-clj, you need to tell the program to use the functions of conexp-clj in the following way

        (use 'conexp.main)

## File Based Access

Typing in long sequences of commands is tedious, and conexp-clj inherits from Clojure the possibility to compile and run complex programs.  Everything you can type into the prompt can also be written into a file, say `file.clj`.  To run (and compile) this file, just type do

     user=> (load-file "file.clj")
     ...

## Emacs and nrepl

A particularly interesting way to use conexp-clj (especially for development) is to use [nrepl](http://dev.clojure.org/jira/browse/NREPL) in combination with [nrepl.el](https://github.com/technomancy/nrepl.el), see the documentation of nrepl.el for further information.

## Other ways

It is also possible to use conexp-clj's incomplete GUI fragment.  You can start it running the following code in you favourite shell

    $ conexp-clj --gui

But please note that the GUI is not only (inherently) limited in its functionality, but also quite (not to say: really) buggy.
