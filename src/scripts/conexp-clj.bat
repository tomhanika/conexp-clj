@echo off

set CONEXP_CLJ_HOME=..
set CONEXP_CLJ_INIT=%CONEXP_CLJ_HOME%/lib/conexp-clj.clj

java -Dawt.useSystemAAFontSettings=on -cp "%CONEXP_CLJ_HOME%/lib/*" jline.ConsoleRunner clojure.main -e "" %CONEXP_CLJ_INIT% %*
