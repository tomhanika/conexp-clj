To factorize a Formal-Context into two seperate Contexts, which contain the Object-Factorization and the Attribute-Factorization, one may use the following Algorithms:
<ol>
  <li>asso https://doi.org/10.1109/TKDE.2008.53</li>
  <li>grecond https://doi.org/10.1016/j.jcss.2009.05.002</li>
  <li>tiling https://doi.org/10.1007/978-3-540-30214-8_22</li>
  <li>greess https://doi.org/10.1016/j.jcss.2015.06.002</li>
  <li>hyper https://doi.org/10.1007/s10618-010-0203-9</li>
  <li>panda https://doi.org/10.1137/1.9781611972801.15</li>
  <li>topfiberm https://doi.org/10.48550/arXiv.1903.10326</li>
</ol> 
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
To calculate the factorization we can use the following syntax using the grecond algorithm:
```clj
(:context (apply ->factorization-record (grecond water-context 5)));; gives the context calculated using the grecond algorithm
(:object-factor (apply ->factorization-record (grecond water-context 5)));; gives the object-factor calculated using the grecond algorithm
(:attribute-factor (apply ->factorization-record (grecond water-context 5)));; gives the attribute-factor calculated using the grecond algorithm
``` 

Every algorithm except asso and topfiberm need a context and a number k to produce factorizations.

```clj
(:context (apply ->factorization-record (topfiberm water-context k tp SR)));; tp is a threshold from 0 to 1, Sr is a search radius 
(:context (apply ->factorization-record (asso water-context k tp w+ w-)));; tp is a threshold from 0 to 1, w+ determines how positive incidences are weighted, w- determines how negative incidences are weighted
``` 