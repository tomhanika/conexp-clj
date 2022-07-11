# Factorization

To factorize a Formal-Context into two seperate Contexts, which contain the Object-Factorization and the Attribute-Factorization, one may use the following Algorithms.
For demonstration purposes the waterContext is used here with k = 5.
```clj
(def waterContext (make-context-from-matrix 8 9 [1 1 0 0 0 0 1 0 0 1 1 0 0 0 0 1 1 0 1 1 1 0 0 0 1 1 0 1 0 1 0 0 0 1 1 1 1 1 0 1 0 1 0 0 0 1 1 1 1 0 1 0 0 0 1 0 1 1 1 0 0 0 0 1 0 1 1 0 1 0 0 0]))

;;  |0 1 2 3 4 5 6 7 8
;;--+------------------
;;0 |x x . . . . x . .
;;1 |x x . . . . x x .
;;2 |x x x . . . x x .
;;3 |x . x . . . x x x
;;4 |x x . x . x . . .
;;5 |x x x x . x . . .
;;6 |x . x x x . . . .
;;7 |x . x x . x . . .
```

# Panda
The [Panda-Algorithm] (https://doi.org/10.1137/1.9781611972801.15) is a greedy algorithm who searches tuple (A,B) in the context, whose derivation coveres as many incidences as possible.

```clj
(def waterPanda (->factorization (panda waterContext 5)))

(context waterPanda)
;;  |0 1 2 3 4 5 6 7 8
;;--+------------------
;;0 |x x . . . . x x .
;;1 |x x . . . . x x .
;;2 |x x x . . . x x .
;;3 |x x x . . . x x x
;;4 |x x x x . x . . .
;;5 |x x x x . x . . .
;;6 |x . x x x . . . .
;;7 |x x x x . x . . .

(object-factor waterPanda)
;;  |0 1 2 3 4
;;--+----------
;;0 |. . . . x
;;1 |. . . . x
;;2 |. . x . x
;;3 |x . x . x
;;4 |. . . x .
;;5 |. . . x .
;;6 |. x x . .
;;7 |. . . x .

(attribute-factor waterPanda)
;;  |0 1 2 3 4 5 6 7 8
;;--+------------------
;;0 |. . . . . . . . x
;;1 |x . . x x . . . .
;;2 |. . x . . . . . .
;;3 |x x x x . x . . .
;;4 |x x . . . . x x .
```

# Grecond
The [Grecond-Algorithm](https://doi.org/10.1016/j.jcss.2009.05.002) is a greedy algorithm, whose target it is to search for k concepts, where there are the most covered incidences.

```clj
(def waterGrecond (->factorization (grecond waterContext 5)))

(context waterGrecond)
;;  |0 1 2 3 4 5 6 7 8
;;--+------------------
;;0 |x x . . . . x . .
;;1 |x x . . . . x x .
;;2 |x x x . . . x x .
;;3 |x . x . . . x x .
;;4 |x x . x . x . . .
;;5 |x x x x . x . . .
;;6 |x . x . . . . . .
;;7 |x . x x . x . . .

(object-factor waterGrecond)
;;  |0 1 2 3 4
;;--+----------
;;0 |. x . x .
;;1 |. x . x x
;;2 |x x . x x
;;3 |x . . . x
;;4 |. x x . .
;;5 |x x x . .
;;6 |x . . . .
;;7 |x . x . .

(attribute-factor waterGrecond)
;;  |0 1 2 3 4 5 6 7 8
;;--+------------------
;;0 |x . x . . . . . .
;;1 |x x . . . . . . .
;;2 |x . . x . x . . .
;;3 |x x . . . . x . .
;;4 |x . . . . . x x .
```

# Hyper
The [Hyper-Algorithm](https://doi.org/10.1007/s10618-010-0203-9) is a greedy algorithm, trying to cover as much incidences as possible with overlapping rectangles and constructing the factorizations so that the error is as low as possible.

```clj
(def waterHyper (->factorization (hyper waterContext 5)))

(context waterHyper)
;;  |0 1 2 3 4 5 6 7 8
;;--+------------------
;;0 |x x . . . . . . .
;;1 |x x . . . . x x .
;;2 |x x x . . . x x .
;;3 |x . x . . . x x .
;;4 |x x . x . x . . .
;;5 |x x x x . x . . .
;;6 |x . x x . . . . .
;;7 |x . x x . x . . .

(object-factor waterHyper)
;;  |0 1 2 3 4
;;--+----------
;;0 |. x x x .
;;1 |. . . . .
;;2 |. . . x x
;;3 |x x x x .
;;4 |x x . . .
;;5 |. . . x x
;;6 |. x . . x
;;7 |x . . . .

(attribute-factor waterHyper)
;;  |0 1 2 3 4 5 6 7 8
;;--+------------------
;;0 |x . . . . . x x .
;;1 |x . x x . . . . .
;;2 |x x . . . . . . .
;;3 |x . . x . x . . .
;;4 |x . x . . . . . .
```

# Greess
The [Greess-Algorithm](https://arxiv.org/abs/1306.4905) is a greedy algorithm, which stars with marking "essential" incidences. When every essential incidence is marked it will try to find the concepts which cover the most of the essintial incidence, until every marked incidence is covered. After that it will generate the Object-Factorization and the Attribute-Factorization.

```clj
(def waterGreess (->factorization (greess waterContext 5)))

(context waterGreess)
;;  |0 1 2 3 4 5 6 7 8
;;--+------------------
;;0 |x x . . . . . . .
;;1 |x x . . . . x x .
;;2 |x x x . . . x x .
;;3 |x . x . . . x x x
;;4 |x x . . . . . . .
;;5 |x x x x . x . . .
;;6 |. . . . . . . . .
;;7 |x . x x . x . . .

(object-factor waterGreess)
;;  |0 1 2 3 4
;;--+----------
;;0 |x . . . .
;;1 |x . x . .
;;2 |x . x x .
;;3 |. . x . x
;;4 |x . . . .
;;5 |x x . . .
;;6 |. . . . .
;;7 |. x . . .
;;8 |. . . . .

(attribute-factor waterGreess)
;;  |0 1 2 3 4 5 6 7
;;--+----------------
;;0 |x x . . . . . .
;;1 |x . x x . x . .
;;2 |x . . . . . x x
;;3 |x x x . . . x x
;;4 |x . x . . . x x
```
