all: zip

zip: jar
	mkdir -p conexp-clj/lib/
	cp stuff/libs/*.clj conexp-clj/lib
	cp -r stuff/bin LICENSE README.md conexp-clj/
	cp -r src/res lib/*.jar conexp-clj/lib/
	mv conexp-clj-*.jar conexp-clj/lib/
	zip -r conexp-clj-$(shell cat VERSION).zip conexp-clj

jar: distclean
	lein jar

clean:
	rm -rf conexp-clj/ lib/classes

distclean: clean
	rm -rf lib conexp-clj-$(shell cat VERSION).zip

test: clean
	lein deps, test
