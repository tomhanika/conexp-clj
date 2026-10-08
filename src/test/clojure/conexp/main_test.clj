;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.main-test
  (:require [clojure.test :refer :all]
            [conexp.main]))

(def ^:private load-conexp-file
  ;; `-main` itself ends in `System/exit`, so the load path is exercised through
  ;; the function `-main` delegates to.
  @#'conexp.main/load-conexp-file)

(defn- on-a-bare-thread
  "Runs `f` on a fresh thread and returns whatever it threw, or nil.

  A plain thread carries no thread bindings, which is exactly the situation
  `-main` finds itself in when it is the entry point of an AOT compiled uberjar
  rather than something a REPL called.  Running this on the calling thread would
  not reproduce it, because the test runner does hold a binding for `*ns*`."
  [f]
  (let [thrown (atom nil)
        thread (Thread. #(try (f) (catch Throwable t (reset! thrown t))))]
    (.start thread)
    (.join thread 60000)
    @thrown))

(deftest test-load-option-without-a-namespace-binding
  (let [file (java.io.File/createTempFile "conexp-load" ".clj")]
    (try
      (spit file "(def loaded-context (make-context #{1 2} #{'a} #{[1 'a]}))\n")
      (testing "`-l` has to work with no thread binding for `*ns*`, which is how
                it is reached from `java -jar`"
        (let [thrown (on-a-bare-thread #(load-conexp-file (.getAbsolutePath file)))]
          (is (nil? thrown)
              (str "loading threw " (some-> thrown class .getName) ": "
                   (some-> thrown .getMessage)))))
      (testing "and the file is evaluated where conexp is in scope, so it may use
                the library without requiring anything"
        (is (some? (resolve 'conexp.main/loaded-context))))
      (finally
        (ns-unmap 'conexp.main 'loaded-context)
        (.delete file)))))

(deftest test-load-option-leaves-the-namespace-alone
  (let [file (java.io.File/createTempFile "conexp-load" ".clj")
        before *ns*]
    (try
      (spit file "(def untouched 1)\n")
      (load-conexp-file (.getAbsolutePath file))
      (is (= before *ns*) "loading a file must not move the caller's namespace")
      (finally
        (ns-unmap 'conexp.main 'untouched)
        (.delete file)))))

;;;

nil
