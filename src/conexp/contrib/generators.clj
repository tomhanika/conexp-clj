(ns #^{:doc "Package for generator functions."}
  conexp.contrib.generators)

;; Aim:
;;
;;  user=> (defg generator [x y]
;;           (dotimes [i x]
;;             (dotimes [j y]
;;               (yield (+ x y)))))
;;  #'user/generator
;;  user=> (def gen (generator 3 4))
;;  #'user/gen
;;  user=> (gen)
;;  0
;;  user=> (gen)
;;  1
;;  user=> (generate (generator 2 2))
;;  (0 1 1 2)
;;  user=>
;;

nil
