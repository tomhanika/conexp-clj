#!/bin/bash

mkdir -p lib && cp src/lib/*.jar lib/ && cp src/lib/*.clj lib && \
lein deps && lein jar && mv conexp-clj.jar lib && \
zip -r conexp-clj.zip bin lib AUTHORS LICENSE README
