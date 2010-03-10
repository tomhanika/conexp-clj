#!/bin/bash

lein deps && lein jar && mv conexp.jar lib && \
zip conexp-clj.zip bin lib AUTHORS LICENSE README
