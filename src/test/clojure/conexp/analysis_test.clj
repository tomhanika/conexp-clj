;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.analysis-test
  "Guards the namespace a REPL session starts in.

  `conexp.analysis` refers some fifty conexp namespaces wholesale, so any public
  name defined in two of them silently shadows one of the two.  Clojure prints a
  warning while loading, which is easy to miss in a build log, and the loser
  simply becomes uncallable."
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [conexp.analysis]
            [conexp.fca.contexts :refer [attributes make-context]]))

(defn- referred-conexp-namespaces
  "The conexp namespaces contributing names to `conexp.analysis`."
  []
  (->> (ns-refers 'conexp.analysis)
       vals
       (keep #(some-> % meta :ns ns-name))
       distinct
       (filter #(string/starts-with? (name %) "conexp"))))

(defn- shadowed-names
  "Maps every public name defined in more than one of `namespaces` to the
  namespaces defining it."
  [namespaces]
  (->> namespaces
       (reduce (fn [owners ns]
                 (reduce (fn [owners sym] (update owners sym (fnil conj #{}) ns))
                         owners
                         (keys (ns-publics ns))))
               {})
       (filter (fn [[_ owners]] (< 1 (count owners))))
       (into (sorted-map))))

(deftest test-no-name-is-defined-twice
  (testing "no public name is defined by two of the namespaces the REPL refers,
            since one of the two would be unreachable from the prompt"
    (let [clashes (shadowed-names (referred-conexp-namespaces))]
      (is (empty? clashes)
          (str "shadowed: "
               (string/join ", " (for [[sym owners] clashes]
                                   (str sym " in " (sort owners)))))))))

(deftest test-add-attribute-is-the-context-operation
  (testing "the documented three argument context operation, which an internal
            step of the incremental Ganter algorithm used to shadow"
    (is (= #'conexp.fca.contexts/add-attribute
           (ns-resolve 'conexp.analysis 'add-attribute)))
    (let [ctx (make-context #{1 2} #{"a"} #{[1 "a"]})]
      (is (= #{"a" "b"}
             (attributes ((ns-resolve 'conexp.analysis 'add-attribute) ctx "b" #{2})))))))

;;;

nil
