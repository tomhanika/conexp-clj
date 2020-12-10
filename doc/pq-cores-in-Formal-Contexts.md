[Knowledge Cores in Large Formal Contexts](https://arxiv.org/abs/2002.11776) provides a wide range of functionalities to analyze core structures in Formal Contexts. These methods are implemented in conexp-clj.

# Computing Cores
To compute pq-cores of a context `ctx` we use the `compute-core` function

```clj
(def ctx (make-context [1 2 3 4] [1 2 3 4] <=))
;; => #'user/ctx-1
;;   |1 2 3 4 
;; --+--------
;; 1 |x x x x 
;; 2 |. x x x 
;; 3 |. . x x 
;; 4 |. . . x

(compute-core ctx 2 2)
;;   |2 3 4 
;; --+------
;; 1 |x x x 
;; 2 |x x x 
;; 3 |. x x
```

# Discover interesting pq-cores

To discover particular interesting cores we can investigate the sizes of all pq-cores or the concept lattice size of all pq-cores. One approach for this was discussed in [Knowledge Cores in Large Formal Contexts](https://arxiv.org/abs/2002.11776). We use the `ctx-core-sizes` and `core-lattice-sizes` functions to efficiently compute these sizes.

```clj
(ctx-core-sizes ctx)      ;; [p q size] the order by p,q is not guarantied!
;; => [[1 1 16.0] [1 2 12.0] [1 3 8.0] [1 4 4.0] 
;;     [2 1 12.0] [2 2 9.0]  [2 3 6.0] [2 4 0.0]
;;     [3 1 8.0]  [3 2 6.0]  [3 3 0.0] [3 4 0.0] 
;;     [4 1 4.0]  [4 2 0.0]  [4 3 0.0] [4 4 0.0]]

(core-lattice-sizes ctx)  ;; [p q sizes] the order by p,q is not guarantied!
;; => [[1 1 4] [1 2 3] [1 3 2] [1 4 1] 
;;     [2 1 3] [2 2 2] [2 3 1] [2 4 0] 
;;     [3 1 2] [3 2 1] [3 3 0] [3 4 0]
;;     [4 1 1] [4 2 0] [4 3 0] [4 4 0]]
```
In case your context is to large for such a computation you can limit the computation of core sizes to those with p or q over a certain threshold.

```clj
(core-lattice-sizes ctx 4 3)  ;; [p q sizes] p>=3 or q >= 2. Those with lower p and q are 0 by default. the order by p,q is not guarantied!
;; => [[1 1 0] [1 2 0] [1 3 2] [1 4 1] 
;;     [2 1 0] [2 2 0] [2 3 1] [2 4 0] 
;;     [3 1 0] [3 2 0] [3 3 0] [3 4 0]
;;     [4 1 1] [4 2 0] [4 3 0] [4 4 0]]
```

Predetermine the restriction value for p and q is difficult. The `large-ctx-lattice-sizes-partial` function uses binary search to a p,q value such that the first computed cores lattice sizes (p,1 and 1,q) have maximum value. In our exemplary analysis on larger contexts we used a value of 60 which is twice as large as what we considered readable.

```clj
(large-ctx-lattice-sizes-partial ctx 2)  ;; [p q sizes] starting with core lattice size of at most 2 for the 1,p and q,1 core. Those with lower p and q are 0 by default. the order by p,q is not guarantied!
;; => [[1 1 0] [1 2 0] [1 3 2] [1 4 1] 
;;     [2 1 0] [2 2 0] [2 3 1] [2 4 0] 
;;     [3 1 2] [3 2 1] [3 3 0] [3 4 0]
;;     [4 1 1] [4 2 0] [4 3 0] [4 4 0]]
```
A python script to plot the core sizes can be found in [pqcore-heatmaps](https://github.com/hirthjo/pqcore-heatmaps).