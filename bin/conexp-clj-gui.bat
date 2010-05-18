@echo off

set CONEXP_CLJ_HOME=..
set CONEXP_CLJ_CONSOLE_INIT=%CONEXP_CLJ_HOME%/lib/conexp-clj-gui-init.clj

java -cp "%CONEXP_CLJ_HOME%/lib/*" clojure.main -i %CONEXP_CLJ_CONSOLE_INIT%