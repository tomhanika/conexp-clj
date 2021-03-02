;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.math.optimize-test
  (:use conexp.math.optimize)
  (:use clojure.test))

;;;

(defn- real= [tol a b]
  (>= tol (Math/abs (double (- a b)))))

(deftest test-minimize
  (is (let [[[x] opt] (minimize #(Math/sin (aget ^doubles % 0)) ;note: function gets array of doubles!
                                (fn [k]
                                  (condp = k
                                    0 #(Math/cos (aget ^doubles % 0)),
                                    (constantly 0.0)))
                                [0.0]
                                {})]
        (and (real= 1e-16 opt -1.0)
             (real= 1e-16 x -1.5707963267948966))))
  (is (let [[[x] opt] (minimize #(Math/sin (aget ^doubles % 0)) ;note: function gets array of doubles!
                                [0.0]
                                {})]
        (and (real= 1e-10 opt -1.0)
             (real= 1e-7 x -1.5707963267948966)))))

(deftest test-maximize
  (is (let [[[x] opt] (maximize #(Math/sin (aget ^doubles % 0)) ;note: function gets array of doubles!
                                (fn [k]
                                  (condp = k
                                    0 #(Math/cos (aget ^doubles % 0)),
                                    (constantly 0.0)))
                                [0.0]
                                {})]
        (and (real= 1e-16 opt 1.0)
             (real= 1e-16 x 1.5707963267948966))))
  (is (let [[[x] opt] (maximize #(Math/sin (aget ^doubles % 0)) ;note: function gets array of doubles!
                                [0.0]
                                {})]
        (and (real= 1e-10 opt 1.0)
             (real= 1e-7 x 1.5707963267948966)))))

;;;

nil
