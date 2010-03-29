#!/bin/bash

mkdir -p lib && \
cp stuff/additional-libs/*.jar lib/ && cp stuff/additional-libs/*.clj lib && \
lein deps && lein jar && \
mkdir -p conexp-clj && \
cp -r bin lib AUTHORS LICENSE README conexp-clj && \
mv conexp-clj.jar conexp-clj/lib/ && \
zip -r conexp-clj.zip conexp-clj
