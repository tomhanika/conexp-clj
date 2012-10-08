#!/bin/bash

CONEXP_CLJ_HOME=$(dirname $(readlink -f $0))/../
CONEXP_CLJ_INIT=${CONEXP_CLJ_HOME}/lib/conexp-clj.clj

java -server -cp ${CONEXP_CLJ_HOME}/lib/\* clojure.main -e "" ${CONEXP_CLJ_INIT} $*
