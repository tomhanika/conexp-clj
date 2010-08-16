#!/bin/bash

CONEXP_CLJ_HOME=$(dirname $0)/../
CONEXP_CLJ_INIT=${CONEXP_CLJ_HOME}/lib/conexp-clj.clj

java -server -cp ${CONEXP_CLJ_HOME}/lib/\* jline.ConsoleRunner clojure.main -i ${CONEXP_CLJ_INIT} -r $*
