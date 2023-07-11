;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.lattices-test
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.lattices
        conexp.io.lattices
        conexp.io.util-test)
  (:use clojure.test))

;;;

(def- testing-lattices
  (map concept-lattice (random-contexts 20 10)))

(deftest test-lattice-oi
  (with-testing-data [lat testing-lattices,
                      fmt (list-lattice-formats)]
    (= lat (out-in lat 'lattice fmt))))

(deftest test-lattice-oioi
  (with-testing-data [lat testing-lattices,
                      fmt (list-lattice-formats)]
    (out-in-out-in-test lat 'lattice fmt)))

;;;

(deftest test-json-matching-schema
  "Read a json format that matches the given schema."
  (let [file "testing-data/digits-lattice1.1.json"]
    (if-not (.exists (java.io.File. file))
      (warn "Could not verify validation of lattice schema. Testing file not found.")
      (let [lattice (read-lattice file :json)]
        (is (= 48 (count (lattice-base-set lattice))))))))

(deftest test-json-not-matching-schema
  "Read a json format that does not match the given schema."
  (if-not (.exists (java.io.File. "testing-data/digits-context.json"))
    (warn "Could not verify failing validation of lattice schema. Testing file not found.") 
    (is (thrown?
         AssertionError
         (read-lattice "testing-data/digits-context.json" :json)))))

(deftest test-identify-input-format
  "Test if the automatic identification of the input format works correctly."
  (with-testing-data [lat testing-lattices,
                      fmt (list-lattice-formats)]
    (= lat (out-in-without-format lat 'lattice fmt))))

;;;

nil
