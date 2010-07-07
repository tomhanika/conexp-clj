#!/bin/bash

lein clean && rm -rfv conexp-clj/ conexp-clj.zip && \
mkdir -p lib && \
cp stuff/libs/*.jar lib/ && cp stuff/libs/*.clj lib && \
lein deps && lein jar && \
mkdir -p conexp-clj && \
cp -r bin lib res AUTHORS LICENSE README conexp-clj && \
mv conexp-clj.jar conexp-clj/lib/conexp-clj-$(date +%Y%m%d.%H%M%S).jar && \
zip -r conexp-clj.zip conexp-clj
