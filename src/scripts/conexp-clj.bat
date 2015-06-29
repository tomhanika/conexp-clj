@echo off

set CONEXP_CLJ_HOME=..
set CONEXP_CLJ_INIT=%CONEXP_CLJ_HOME%/lib/conexp-clj.clj

java -Dawt.useSystemAAFontSettings=on -cp "%CONEXP_CLJ_HOME%/lib/*" clojure.main %CONEXP_CLJ_INIT% %*
