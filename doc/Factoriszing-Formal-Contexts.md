# Factorization

To factorize a Formal-Context into two seperate Contexts, which contain the Object-Factorization and the Attribute-Factorization, one may use the following Algorithms.

#Panda
The [Panda-Algorithm] (https://doi.org/10.1137/1.9781611972801.15) is a greedy algorithm who searches tuple (A,B) in the context, whose derivation coveres as many incidences as possible.

To retrieve the Object-Factorization and the Attribute-Factorization from ctx-1 use:
```clj
(def ctx-1 (make-context-from-matrix 6 6
  [1 1 1 0 1 1
   0 1 0 0 1 0
   0 0 1 0 0 1
   0 0 0 1 1 1
   0 0 0 0 1 0
   0 0 0 0 0 1]))

(panda ctx-1 6)
;;({:ci [0 0 1 0 0 1], :ct [0 0 0 0 0 1]}
;; {:ci [0 1 0 0 1 0], :ct [0 0 0 0 1 0]} 
;; {:ci [1 0 0 0 0 0], :ct [1 0 0 0 0 0]}
;; {:ci [1 0 1 0 0 0], :ct [0 0 1 0 0 0]}
;; {:ci [1 1 0 0 0 0], :ct [0 1 0 0 0 0]}
;; {:ci [1 0 0 1 0 0], :ct [0 0 0 0 1 1]})

;;=> ci = Object-Factorization, ct = Attribute-Factorization
```
4 being the size of the Factorizations.

and

```clj
(calcPandaContext (panda ctx-1 6))
;;  |0 1 2 3 4 5
;;--+------------
;;0 |x x x . x x
;;1 |. x . . x .
;;2 |. . x . . x
;;3 |. . . . x x
;;4 |. . . . x .
;;5 |. . . . . x
```

constructing back the Context from the factorization-contexts.

#Grecond
The [Grecond-Algorithm](https://doi.org/10.1016/j.jcss.2009.05.002) is a greedy algorithm, whose target it is to search for k concepts, where there are the most covered incidences.

```clj
(grecond ctx-1)
;;[[#{0} #{0 1 4 2 5}]
;; [#{3} #{4 3 5}]
;; [#{0 3 2 5} #{5}]
;; [#{0 2} #{2 5}]
;; [#{0 1 4 3} #{4}]
;; [#{0 1} #{1 4}]]
```

and

```clj
(calcGrecondContext (grecond ctx-1) 6 6)

;;  |0 1 2 3 4 5 
;;--+------------
;;0 |x x x . x x
;;1 |. x . . x .
;;2 |. . x . . x
;;3 |. . . x x x
;;4 |. . . . x .
;;5 |. . . . . x
```

