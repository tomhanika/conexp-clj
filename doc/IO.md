`conexp-clj` offers a transparent way of handling input and output for unary formal
contexts, many-valued contexts, lattices and their layouts.  The main idea behind this
approach is to free the user from the technical details of IO operations by automatically
determing the appropriate format.

## Formal Contexts

Suppose that you have a file `context` that contains a unary formal context.  Reading in
this formal context is then simply done as follows:

```clj
(read-context "context")
```

`conexp-clj` will try to determine the file format on its own, returning the formal
contexts on success.  Supported formats for input are

- Burmeister (`:burmeister`)
- FCAalgs (`:fcalgs`)
- Colibri (`:colibri`)
- ConExp (`:conexp`)
- `conexp-clj` simple (`:simple`)
- CSV (`:csv`)
- Binary CSV (`:binary-csv`)
- Galicia (`:galicia`)
- Anonymous Burmeister (`:anonymous-burmeister`)

You can get the list of available input formats for contexts by executing `(list-context-input-formats)`.

In case `conexp-clj` fails to determine your format correctly, you can explicitly specify
it

```clj
(read-context "context" :burmeister)
```

Writing formal contexts to files works in nearly the same way.  Suppose you have given
your context `ctx`, then writing it to `context` in the Burmeister format just works this
way

```clj
(write-context :burmeister ctx "context")
```

If you don't specify an output format, a standard output format will be used, which you
can query with `get-default-context-format` and set with `set-default-context-format!`.

You can obtain a list of available context output formats by calling
`list-context-output-formats` with no arguments.  In addition to the input formats for
formal contexts, you are also able to export formal contexts directly to TeX-code using
`:tex`.


## Many-Valued Contexts

IO operations for many-valued contexts works in nearly the same way as for the case of
unary formal contexts.  The corresponding functions are `read-mv-context` and
`write-mv-context` with the same order of arguments as in the unary case.  You can obtain
a list of available input formats with `list-mv-context-input-formats`, which gives you
`:simple` and `:data-table`; `list-mv-context-output-formats` yields the list of available
output formats.

See
[house-votes-84.data](https://github.com/exot/conexp-clj/blob/master/stuff/testing-data/house-votes-84.data)
and
[agaricus-lepiota.data](https://github.com/exot/conexp-clj/blob/master/stuff/testing-data/agaricus-lepiota.data)
for examples of the `:data-table` format.  Note that the first line is always the set of
attributes.  If the following lines have an entry more then the first line, the first
entry will become the name of the object.  Otherwise, objects will be given arbitrary
names.
