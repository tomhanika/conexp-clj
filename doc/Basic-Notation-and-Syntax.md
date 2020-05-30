conexp-clj is written in [Clojure](http://www.clojure.org), a modern dialect of Lisp.  It is the purpose of this page to introduce some of the basic concepts of Clojure which are important to know when working with conexp-clj.  Also, conexp-clj inherits some of Clojure notational idiosyncracies, which we also want to discuss here.

Of course, this is not a tutorial on programming in Clojure.  For this, consult the documentation on http://www.clojure.org.

## Notational Idiosyncracies

The most obvious difference between Clojure and standard math notation are

1. Prefix notiation
2. Special syntax for tuples and sets

We shall explore prefix notation (by means of examples) in the following section.

The special syntax Clojure brings with it for tuples and sets are as follows: the tuple (1,2,3) is written in Clojure as
    
    [1 2]
    [1, 2]

(the comma is obtional and has the same semantics as a whitespace character)

The reason you cannot use parenthese for tuples is that, as a Lisp, Clojure uses these to represent function calls (in particular) or just lists (in general).  We shall revisit this in the third section.

Sets have their own syntax in Clojure, as {1,2,3} is written as

    #{1 2 3}

This is because the syntax `{1 2 3 4}` is reserved for hash-maps, i.e. functions that map keys to values.

## Expressions

As a Lisp, Clojure inherits the quite special syntax of full parentheses prefix notation.  This may be quite strange-looking at first, but, as it turns out, has a lot of advantages over ALGOL-like syntax.  We give a short overview over the syntax of Clojure here.

Say, for example, you want to compute the sum of 1 and 2.  This can be done (not only in your head, but) in Clojure (and therefore, in conexp-clj as well) by

    user=> (+ 1 2)
    3

If you change your mind (as humans do quite often) and instead want to compute the sum of 1, 2, 3 and 4, you could say

    user=> (+ (+ (+ 1 2) 3) 4)
    10

as this would be the straight forward translation from infix to prefix.  But Clojure can do better, as you can just say

    user=> (+ 1 2 3 4)
    10

That said, the function `+` has no definite (and hence any) arity.  You can even call `+` with no arguments, to obtain the neutral element with respect to addition

    user=> (+)
    0

In more general, suppose you have a function f of arity n and n arguments x_1, …, x_n.  Then, in Clojure, calling f with these arguments, i.e. evaluating f(x_1, …, x_n) is written as

    user=> (f x_1 … x_n)
    ...

You see, there are no more parentheses as in the infix case, and you can even safe some commas.

A last word on expressions in Clojure (and some Lisps in general): there is no distinction between statements and expressions in Clojure.  Hence, everything returns a value, even if it is considered to be a statement in other languages:

    user=> (if (<= 1 2) 3 4)
    3

This even applies to otherwise inherently statement-ish things as function definitions, as we shall see in the following section.

## Functions

As a (mostly) functional programming language, Clojure's main building material are functions.  Thereby, we refer to a *function* not in the sense of mathematics, but in the sense of computer science, although this distinction has made as small as possible in functional programming languages.  But, there are still differences.

The basic syntax to define a function is the following:

    user=> (defn f [x_1 … x_n]
              some-expression)
    #'user/f

Let's illustrate this with an example.  Suppose we want to define a function `f` that, given a single argument `n`, returns `(+ 1 n)` if `n` is a number and `nil` (meaning *nothing*) else.  We can do this as follows

    user=> (defn f [n]
             (if (number? n)
               (+ n 1)
               nil
    #'user/f

There are (at least) two observations to make:

1. There is no explicit return statement that constitutes the return-value of `f`.  Instead, in Clojure,
   the return-value of `f` is implicitly computed as the value of the last expression in the body of `f`.  
   In this case, the last (and only) expression is `(if (number? n) (+ n 1) nil)`, which evaluates to
   `(+ n 1)` if `n` is a number, and `nil` otherwise.
2. Formatting is just for readability and does not effect the semantics of `f` in any way.

Now, calling this function with an argument works

    user=> (f 1)
    2
    user=> (f "I am not a number")
    nil
    user=> (f 1.234)
    2.234
