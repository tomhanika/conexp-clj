Implications are a fundamental means to express dependencies between attributes within
formal contexts.  Because of this importance, `conexp-clj` provides a wide range of
functionality for implications.  This page discusses basic usage of implications, the
computation of some common bases and association rules.  Exploration (which also utilizes
implications) is discuss in [Exploration](Exploration).

## Basic Operations on Implications

Implications are defined by means of the function `make-implication`, which gets two sets
as arguments (the *premise* and the *conclusion* of the implication).

```clj
(make-implication #{1} #{2})
;; (#{1} ⟶ #{2})
```

The arguments can be anything `seqable`, for example vectors or lists; the arguments will
be converted to sets automatically.

```clj
(make-implication [1] '(2))
;; (#{1} ⟶ #{2})
```

There is also a macro `impl`, which provides some kind of easier generation of
implications.

```clj
(impl 1 2 3 ==> 4 5 6)
;; (#{1 2 3} ⟶ #{4 5 6})
```

Everything before the token `==>` will be an element of the premise, and everything after
it will be in the conclusion.  Note that both can be empty.

```clj
(impl ==> 4 5 6)
;; (#{} ⟶ #{4 5 6})
(impl 1 2 3 ==> )
;; (#{1 2 3} ⟶ #{})
(impl 1 2 3) ; will produce a warning
;; (#{1 2 3} ⟶ #{})
```

Having an implication, its premise and conclusion can be accessed by the same-named
functions, `premise` and `conclusion`.

```clj
(premise (make-implication #{1} #{2}))
;; #{1}
(conclusion (make-implication #{1} #{2}))
;; #{2}
```

To test something for being an implication or not, one can make use of `implication?`

```clj
(implication? (impl ==>))
;; true
(implication? 1)
;; false
```

Having an implication, one can test whether a set *respects* this implication, and whether
this implication *holds* withing a formal context.

```clj
(def implication (impl 1 ==> 2))
;; #'user/implication
(respects? #{1 2} implication)
;; true
(holds? implication (diag-context 5))
;; false
```

Implications are closely related to closure operators, and indeed every set of
implications induces one (and every closure operator on sets can be described in this
way).  `conexp-clj` provides the function `clop-by-implications` to obtain this closure
operator.  Underlying this function is `close-under-implications`, which computes the
inclusion-minimal set of a given set which respects all given implications.

```clj
(close-under-implications [(impl 1 ==> 2) (impl 2 ==> 3)] #{1})
;; #{1 2 3}
```

Based upon such closure operators, basic logical notions such as *entailment* for
implications can be decided.  The function `follows?` (or `follows-semantically?`, for
historical reasons) provides this functionality.

```clj
(follows? (impl 1 ==> 3) [(impl 1 ==> 2) (impl 2 ==> 3)])
;; true
```

Moreover, one can decide whether sets of implications are *equivalent*
(`equivalent-implications?`), i.e. have the same induced closure operator and whether a
set of implications is *minimal* (`minimal-implication-set?`), i.e. whether no
implications can be removed from the set without changing the induced closure operator.
If a set is not minimal, a minimal subset can be generated using `irredundant-subset`.

If a formal context is given, one can check whether a set of implications is *sound*
(`sound-implication-set?`), i.e. whether all implications in this set hold in the given
formal context, and whether the set of implications is *complete*
(`complete-implication-set?`), i.e. every implication which holds in the formal context
already follows from the given set of implications.  However, note that checking for
completeness is a rather expensive operation (at least, if the implications are sound).

Another interesting function is `intersect-implicational-theories`, which, given some sets
of implications on a common base set, computes a set of implications whose induced closure
operator corresponds to the intersection of the induced closure operators of the given
sets of implications.

## Computing Bases

Given a formal context `ctx`, one of the most basic operations in formal concept analysis
is to compute a *base* of `ctx`, i.e. a sound and complete set of implications of `ctx`.
Such a base is given by all implications A ⟶ A'' for all subsets A of the attribute set of
`ctx`.  In `conexp-clj`, this base can be computed by

```clj
(set-of (make-implication A (adprime ctx A) | A (subsets (attributes ctx))))
```

but, of course, this base is rather large (always exponential in `(count (attributes
ctx))`).

A better solution for this might (might) be the computation of the *canonical base*, a
special base whose size is minimal among all sizes of bases of the formal context.  This
can be done using `canonical-base` (`stem-base` for historians).

```clj
(canonical-base (diag-context 3))
;; ((#{1 2} ⟶ #{0}) (#{0 2} ⟶ #{1}) (#{0 1} ⟶ #{2}))
(canonical-base (adiag-context 3))
;; ()
```

The result returned is a lazy sequence, and as such its elements are not computed if they
are not needed.  This can be handy when computing the canonical base of rather large
contexts, as the computation can (and mostly is) very expensive.  Indeed, even the
canonical base can be exponential in the size of the given formal context (and thus
exponential in the size of its attribute set).

The implementation of `canonical-base` makes use of a more general algorithm, namely the
computation of the canonical base of a given closure operator on sets.  This functionality
is exposed by `canonical-base-from-clop`.

If one has already computed a base of the formal context at hand, but instead wants to
have the canonical base of the formal context instead, one can make use of the function
`canonical-base-from-base` (`stem-base-from-base`) which computes this canonical base from
the original one in time quadratic in its size.

Instead of computing the canonical base, one can also make use of the base of *proper
premises*, which may be larger than the canonical base but easier to compute.  This base
can be computed using `proper-premise-implications`, and functions related to this are
`proper-conclusion`, `proper-premise?`, `proper-premises-for-attributes` and
`proper-premises`.

## Association Rules

A very prominent use of implications is in the form of *association rules* as employed by
basic data-mining techniques.  Within this scope, implications are tagged with their
*support* and *confidence* within the given data set (here: a formal context), which are
then used as measures of interestingness for an implication.  The support of an
implication is just the relative amount of objects whose extent includes both the premise
and the conclusion of the implication.  In other words, the support of an implication is
just the amount of objects where this implication takes effect.  The confidence of an
implication is the "relative probability" that an object that satisfies the premise also
satisfies the conclusion.  Support and confidence can be computed by (guess what!)
`support` and `confidence`.

The task is then, given a formal context and some lower bounds for support and confidence,
to compute a base of all implications whose support and confidence is above these bounds.
This can be achieved by Luxenburger's base, implemented as `luxenburger-base`
(`luxenburger-basis`).  A function related to this is `frequent-closed-itemsets`.

```clj
(def rctx (random-context 10 10 0.5))
;; #'user/rctx
rctx
;;   |0 1 2 3 4 5 6 7 8 9 
;; --+--------------------
;; 0 |. . . x . x . . x . 
;; 1 |x . x x x x . x x . 
;; 2 |x x x . x x x . x . 
;; 3 |x . . . x . x x x x 
;; 4 |x . x . x . . . x . 
;; 5 |x x x x . . . . . x 
;; 6 |. . . . . . . . . . 
;; 7 |. . . x x x x x . x 
;; 8 |x . . x x x x . . . 
;; 9 |x . . x . . x . . . 
(luxenburger-base rctx 0.2 0.8)
;; ((#{6} ⟶ #{4}) (#{5} ⟶ #{4}) (#{5} ⟶ #{3}) (#{6} ⟶ #{0}) (#{4} ⟶ #{0}) (#{8} ⟶ #{0 4}) (#{0 4} ⟶ #{8}))
```

Note that Luxenburger's base **does not** include valid implications.  If you want to have
these as well, you can compute additionally the canonical base, even together with a
support threshold.  See the documentation of `canonical-base` for this.
