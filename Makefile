VERSION = 0.0.7-alpha-SNAPSHOT
TIME = $(shell date -u +"%Y%m%d%H%M%S")

FILES = $(shell find src -name "*.clj")

all: conexp-clj-$(VERSION).zip
	@mv conexp-clj-$(VERSION).zip conexp-clj-$(VERSION)-$(TIME).zip

target/conexp-clj-$(VERSION)-standalone.jar: $(FILES)
	@lein uberjar

conexp-clj-$(VERSION).zip: target/conexp-clj-$(VERSION)-standalone.jar
	@mkdir -p conexp-clj/lib/ conexp-clj/bin/
	@cp src/scripts/conexp-clj.clj conexp-clj/lib/
	@cp -r src/scripts/conexp-clj src/scripts/conexp-clj.bat conexp-clj/bin/
	@cp README.md conexp-clj/
	@cp -r src/res conexp-clj/lib/
	@cp target/conexp-clj-$(VERSION)-standalone.jar conexp-clj/lib/
	@zip -q -r conexp-clj-$(VERSION).zip conexp-clj

clean:
	@rm -rf conexp-clj/ lib/classes target/

distclean: clean
	@rm -rf lib conexp-clj-*.zip

test: clean
	@lein do deps, test

upload: all
	@chmod a+r conexp-clj-$(VERSION)-$(TIME).zip
	@scp conexp-clj-$(VERSION)-$(TIME).zip math:public_html/downloads/conexp-clj-$(VERSION)-$(TIME).zip
