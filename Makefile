VERSION = $(shell cat VERSION)

FILES = $(shell find src -name "*.clj")

target/conexp-clj-$(VERSION)-standalone.jar: $(FILES)
	@lein uberjar

zip: target/conexp-clj-$(VERSION)-standalone.jar
	@mkdir -p conexp-clj/lib/
	@cp stuff/libs/*.clj conexp-clj/lib
	@cp -r stuff/bin README.md conexp-clj/
	@cp -r src/res conexp-clj/lib/
	@cp target/conexp-clj-$(shell cat VERSION)-standalone.jar conexp-clj/lib/
	@zip -q -r conexp-clj-$(shell cat VERSION)-$(shell date -u +"%Y%m%d%H%M").zip conexp-clj

clean:
	@rm -rf conexp-clj/ lib/classes target/

distclean: clean
	@rm -rf lib conexp-clj-*.zip

test: clean
	@lein deps, test

all: zip
