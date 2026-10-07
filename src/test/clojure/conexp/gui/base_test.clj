;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.base-test
  "Guards the assumption the Quit menu item rests on.

  Quit closes the main window by posting a WINDOW_CLOSING event to it.  It used
  to post that through `processWindowEvent`, which is protected on
  `java.awt.Window`.  Clojure's reflection only finds public methods, so the
  call threw every time Quit was clicked and the window stayed open, which is
  issue #4.  It posts through the public `dispatchEvent` now.

  Clicking Quit cannot be tested without a display, so what is tested here is
  the distinction that caused the bug: a future change back to a protected
  method would fail."
  (:require [clojure.test :refer :all])
  (:import [java.awt AWTEvent Window]
           [java.awt.event WindowEvent]
           [java.lang.reflect Modifier]))

(defn- reflectively-callable?
  "Whether Clojure could invoke `method` on `klass`, which is to say whether it
  is public and so visible to `clojure.lang.Reflector`."
  [^Class klass ^String method ^Class argument]
  (boolean (some #(and (= method (.getName ^java.lang.reflect.Method %))
                       (Modifier/isPublic (.getModifiers ^java.lang.reflect.Method %))
                       (= 1 (alength (.getParameterTypes ^java.lang.reflect.Method %)))
                       (.isAssignableFrom
                        (aget (.getParameterTypes ^java.lang.reflect.Method %) 0)
                        argument))
                 (.getMethods klass))))

(deftest test-the-event-posting-method-is-public
  (testing "dispatchEvent is public, so the Quit handler can reach it"
    (is (reflectively-callable? Window "dispatchEvent" WindowEvent))
    (is (reflectively-callable? Window "dispatchEvent" AWTEvent)))
  (testing "processWindowEvent is not, which is why Quit used to do nothing"
    (is (not (reflectively-callable? Window "processWindowEvent" WindowEvent)))
    (is (Modifier/isProtected
         (.getModifiers (.getDeclaredMethod Window "processWindowEvent"
                                            (into-array Class [WindowEvent])))))))

(deftest test-quit-uses-the-public-method
  (testing "the handler in conexp.gui.base posts the event rather than
            processing it, so that it works outside the class hierarchy"
    (let [source (slurp "src/main/clojure/conexp/gui/base.clj")
          quit   (subs source (.indexOf source "\"Quit\""))]
      (is (re-find #"\.dispatchEvent" quit))
      (is (not (re-find #"\.processWindowEvent" quit))))))

;;;

nil
