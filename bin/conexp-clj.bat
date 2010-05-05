@echo off

set CONEXP_CLJ_HOME=..
set CONEXP_CLJ_CONSOLE_INIT=%CONEXP_CLJ_HOME%/lib/conexp-clj-console-init.clj

java -cp "%CONEXP_CLJ_HOME%/lib/*" jline.ConsoleRunner clojure.main -i %CONEXP_CLJ_CONSOLE_INIT% -r