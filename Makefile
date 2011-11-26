CONEXP_CLJ_JAR=conexp-clj-$(cat VERSION)-$(date +%Y%m%d.%H%M%S).jar

all: zip

zip: jar
	mv conexp-clj.jar ${CONEXP_CLJ_JAR}
	mkdir -p conexp-clj/lib/
	cp stuff/libs/*.jar conexp-clj/lib/
	cp stuff/libs/*.clj conexp-clj/lib
	cp -r bin res AUTHORS LICENSE README.md conexp-clj/
	cp lib/*.jar conexp-clj/lib/
	mv ${CONEXP_CLJ_JAR} conexp-clj/lib/
	zip -r conexp-clj.zip conexp-clj

jar: distclean
	lein jar

clean:
	rm -rf conexp-clj/ conexp-clj.zip

distclean: clean
	rm -rf lib classes

test:
	lein test!

test-zip:

