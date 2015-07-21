VERSION := $(shell cat src/res/version)
TIME    := $(shell date -u +"%Y-%m-%d-%H%M%S")
FILES   := $(shell find src -name "*.clj")

all: clean conexp-clj-$(VERSION).zip
	@mv builds/conexp-clj-$(VERSION).zip builds/conexp-clj-$(VERSION)-$(TIME).zip

target/conexp-clj-$(VERSION)-standalone.jar: $(FILES)
	@lein uberjar

conexp-clj-$(VERSION).zip: target/conexp-clj-$(VERSION)-standalone.jar
	@mkdir -p builds/conexp-clj/lib/ builds/conexp-clj/bin/
	@cp src/scripts/conexp-clj.clj builds/conexp-clj/lib/
	@cp -r src/scripts/conexp-clj src/scripts/conexp-clj.bat builds/conexp-clj/bin/
	@cp README.md builds/conexp-clj/
	@cp target/conexp-clj-$(VERSION)-standalone.jar builds/conexp-clj/lib/
	@zip -q -r builds/conexp-clj-$(VERSION).zip builds/conexp-clj

clean:
	@rm -rf builds/conexp-clj/ lib/classes target/

distclean: clean
	@rm -rf lib builds

test: clean
	@lein do deps, test

upload: all
	@chmod a+r builds/conexp-clj-$(VERSION)-$(TIME).zip
	@scp builds/conexp-clj-$(VERSION)-$(TIME).zip lat:public_html/downloads/conexp-clj-$(VERSION)-$(TIME).zip
