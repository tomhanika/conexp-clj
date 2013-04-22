@echo off

set CONEXP_CLJ_HOME=..
set CONEXP_CLJ_INIT=%CONEXP_CLJ_HOME%/lib/conexp-clj.clj

java -server -cp "%CONEXP_CLJ_HOME%/lib/*" clojure.main -e "" %CONEXP_CLJ_INIT% %*
