;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.fcas-test
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.lattices
        conexp.fca.implications
        conexp.io.fcas
        conexp.io.util-test)
  (:use clojure.test))

;;;

(def- context-oi
  "Context to use for out-in testing"
  (make-context #{"a" "b" "c"}
                #{"1" "2" "3"}
                #{["a" "1"] ["a" "3"]
                  ["b" "2"] ["c" "3"]}))

(def- fca-ctx-oi
  "FCA with context"
  {:context context-oi})

(deftest test-fca-only-context-out-in
  "Context is the only input"
  (with-testing-data [fca [fca-ctx-oi],
                      fmt (list-fca-formats)]
    (= fca (out-in fca 'fca fmt))))

(def- lattice-oi
  "Lattice to use for out-in testing"
  (concept-lattice context-oi))

(def- fca-lat-oi
  "FCA with context and lattice"
  {:context context-oi :lattice lattice-oi})

(deftest test-fca-with-lattice-out-in
  "Context and lattice as input"
  (with-testing-data [fca [fca-lat-oi],
                      fmt (list-fca-formats)]
    (= fca (out-in fca 'fca fmt))))

(def- implications-oi
  "Implications to use for out-in testing"
  (canonical-base context-oi))

(def- fca-impl-oi
  "FCA with context and implications"
  {:context context-oi :implication-sets [implications-oi]})

(deftest test-fca-with-implication-out-in
  "Context and implications as input"
  (with-testing-data [fca [fca-impl-oi],
                      fmt (list-fca-formats)]
    (= fca (out-in fca 'fca fmt))))

(def- fca-oi
  "FCA with context, lattice and implications"
  {:context context-oi :lattice lattice-oi :implication-sets [implications-oi]})

(deftest test-fca-out-in
  "Context, lattice and implications as input"
  (with-testing-data [fca [fca-oi],
                      fmt (list-fca-formats)]
    (= fca (out-in fca 'fca fmt))))

(def- fca-several-implication-sets-oi
  "FCA with context, lattice and several implication sets"
  {:context context-oi :lattice lattice-oi :implication-sets [implications-oi implications-oi]})

(deftest test-fca-several-implication-sets-out-in
  "Context, lattice and several implication sets as input"
  (with-testing-data [fca [fca-several-implication-sets-oi],
                      fmt (list-fca-formats)]
    (= fca (out-in fca 'fca fmt))))

(def- contexts-oioi
  "Contexts to use for out-in-out-in testing"
  [(make-context #{1 2 3} #{4 5 6} <),
   (make-context #{"a"} #{"+"} #{["a" "+"]})])

(def- fca-ctx-oioi
  "FCAs for out-in-out-in testing"
  (into [] (for [ctx contexts-oioi] {:context ctx})))

(deftest test-fca-only-context-out-in-out-in
    "Several tests with only-context input"
    (with-testing-data [fca fca-ctx-oioi,
                        fmt (list-fca-formats)]
      (out-in-out-in-test fca 'fca fmt)))

(def- lattice-oioi
  "Lattice to use for out-in-out-in testing"
  (mapv concept-lattice contexts-oioi))

(def- fca-lat-oioi
  "FCAs for out-in-out-in testing with context and lattice"
  (into [] (for [[ctx lat]
                 (map list contexts-oioi lattice-oioi)]
             {:context ctx :lattice lat})))

(deftest test-fca-with-lattice-out-in-out-in
  "Several tests with context and lattice input"
  (with-testing-data [fca fca-lat-oioi,
                      fmt (list-fca-formats)]
    (out-in-out-in-test fca 'fca fmt)))

(def- implications-oioi
  "Implications to use for out-in-out-in testing"
  (mapv canonical-base contexts-oioi))

(def- fca-impl-oioi
  "FCAs for out-in-out-in testing with context and implications"
  (into [] (for [[ctx impl]
                 (map list contexts-oioi implications-oioi)]
             {:context ctx :implication-sets [impl]})))

(deftest test-fca-with-implications-out-in-out-in
  "Several tests with context and implication input"
  (with-testing-data [fca fca-impl-oioi,
                      fmt (list-fca-formats)]
    (out-in-out-in-test fca 'fca fmt)))

(def- fca-oioi
  "FCAs for out-in-out-in testing with context, concepts and implications"
  (into [] (for [[ctx lattice impl] 
                 (map list contexts-oioi lattice-oioi implications-oioi)]
             {:context ctx :lattice lattice :implication-sets [impl]})))
  
(deftest test-fca-out-in-out-in
  "Several tests with complete FCA"
  (with-testing-data [fca fca-oioi,
                      fmt (list-fca-formats)]
    (out-in-out-in-test fca 'fca fmt)))

;;;

(deftest test-json-not-matching-schema
  "Read a json format that does not match the given schema."
  (let [file "testing-data/digits-lattice.json"]
    (if-not (.exists (java.io.File. file))
      (warn "Could not verify failing validation of fca schema. Testing file not found.") 
      (is (thrown?
           AssertionError
           (read-fca file :json))))))

(deftest test-json-matching-schema
  "Read a json format that matches the given schema."
  (let [file "testing-data/digits-fca-2.json"]
    (if-not (.exists (java.io.File. file))
      (warn "Could not verify validation of fca schema. Testing file not found.")
      (let [fca (read-fca file :json)]
        (is (= 6 (count (first (:implication-sets fca)))))))))

(deftest test-identify-input-format
  "Test if the automatic identification of the file format works correctly."
  (with-testing-data [fca [fca-oi],
                      fmt (list-fca-formats)]
    (= fca (out-in-without-format fca 'fca fmt)))
  (with-testing-data [fca [(first fca-oioi)],
                      fmt (list-fca-formats)]
    (= fca (out-in-without-format fca 'fca fmt))))

;;;

nil
