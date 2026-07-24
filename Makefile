# conexp-clj build targets.
#
# Node/npm are needed only to BUILD the web GUI (at release time). End users of
# the resulting uberjar need nothing but Java:  java -jar <jar> -a

.PHONY: uberjar frontend frontend-dev run test clean

## uberjar: build a self-contained jar (compiled web GUI + REST API + library).
## Users then run:  java -jar builds/uberjar/conexp-clj-<version>-standalone.jar -a
uberjar: frontend
	lein uberjar

## frontend: compile the web GUI (optimized) into resources/public/js/main.js.
frontend:
	npm ci
	npx shadow-cljs release app

## frontend-dev: hot-reload the web GUI (SPA on :8280); run `lein run -d` alongside.
frontend-dev:
	npm install
	npx shadow-cljs watch app

## run: serve API + GUI from source on :8080 (after `make frontend`).
run:
	lein run -a

## test: run the JVM test suite.
test:
	lein test

## clean: remove build artifacts.
clean:
	lein clean
	rm -rf src/main/resources/public/js .shadow-cljs
