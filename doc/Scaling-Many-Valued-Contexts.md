Scaling is the main process of turning many-valued data into unary formal contexts.  See
the book by Ganter and Wille for an introduction.

Suppose we have given the following many-valued formal context

```plain
user=> mv-context
  |color size       
--+-----------------
1 |blue  large      
2 |green very-large 
3 |red   small      
```

We want to scale this context to make it unary.  Of course, if an object is very larger,
it is also large, so we use an biordinal scale for the attribute `size`.  On the other
hand, the colors don't have share any interrelations, so we just use the nominal scale for
`color`.

The macro that implements a high-level abstraction to scaling is `scale-mv-context-with`.
It is able of handling every possible scale (as these are only formal contexts), but
provides some convenience definitions for known scales.

Let us see how scaling can be done, and let us discuss the details afterwards.

```plain
user=> (scale-mv-context-with mv-context
         [color] (nominal-scale values)
         [size]  (biordinal-scale '[small large very-large]
                                  '[small large very-large]
                                  1
                                  (order-by '[small])
                                  (order-by '[large very-large])))
  |[color blue] [color green] [color red] [size <= small] [size >= large] [size >= very-large] 
--+--------------------------------------------------------------------------------------------
1 |x            .             .           .               x               .                    
2 |.            x             .           .               x               x                    
3 |.            .             x           x               .               .                    
```

Intuitively, we tell `scale-mv-context-with` to scale `mv-context` such that attribute
`color` gets scaled by a nominal scale and that `size` gets scaled using a biordinal
scale.  There, the variable `values` just stands for all possible values the corresponding
attribute can have.

Now, the functions `nominal-scale` and `biordinal-scale` are just ordinary functions, and
we can examine which contexts they produced.

The nominal scale is not a surprise:

```plain
user=> (nominal-scale '[blue green red])
      |blue green red 
------+---------------
blue  |x    .     .   
green |.    x     .   
red   |.    .     x
```

To construct the biordinal scale, we supply five arguments: 

- the set of values of the corresponding attributes, 
- the set of attributes of the resulting scale, 
- how many values should be used for the first scale, 
- how the first set of attributes should be ordered
- how the second set of attributes should be ordered

Looking at our particular example may help to understand this:

```plain
user=> (biordinal-scale '[small large very-large]
                        '[small large very-large]
                        1
                        (order-by '[small])
                        (order-by '[large very-large]))
           |<= small >= large >= very-large 
-----------+--------------------------------
small      |x        .        .             
large      |.        x        .             
very-large |.        x        x             
```

Note that `biordinal-scale` expects the attributes in ascending order, otherwise the
automatically constructed attributes names don't make any sense.
