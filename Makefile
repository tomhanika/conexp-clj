all: zip

zip: uberjar
	mkdir -p conexp-clj/lib/
	cp stuff/libs/*.clj conexp-clj/lib
	cp -r stuff/bin README.md conexp-clj/
	cp -r src/res conexp-clj/lib/
	mv target/conexp-clj-*.jar conexp-clj/lib/
	zip -r conexp-clj-$(shell cat VERSION)-$(date -u +"%Y%m%d%H%M").zip conexp-clj

uberjar: distclean
	lein uberjar

clean:
	rm -rf conexp-clj/ lib/classes

distclean: clean
	rm -rf lib conexp-clj-$(shell cat VERSION).zip

test: clean
	lein deps, test
