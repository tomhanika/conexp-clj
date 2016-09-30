VERSION := $(shell perl -ne '/defproject conexp-clj "(.*)"/ && print $$1' project.clj)

all:
	@lein uberjar
	@mkdir -p builds/conexp-clj/lib/ builds/conexp-clj/bin/
	@cp src/main/scripts/conexp-clj src/main/scripts/conexp-clj.bat builds/conexp-clj/bin/
	@cp README.md builds/conexp-clj/
	@cp builds/uberjar/conexp-clj-$(VERSION)-standalone.jar builds/conexp-clj/lib/
	@zip -q -r builds/conexp-clj-$(VERSION)-$(shell date -u +"%Y%m%d%H%M%S").zip builds/conexp-clj
