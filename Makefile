SOURCE_DIR = /home/beakerx/ppdsp/
JUPYTER_DIR ?= notebooks
FULL_JUPYTER_DIR = /home/beakerx/ppdsp/$(JUPYTER_DIR)

clean:
	rm -rf target

doc:
	lein codox
jupyter:
	docker build -t ppdsp-jupyter .
	docker run --rm -v `pwd`/:/home/beakerx/ppdsp \
		-p 8888:8888 -w $(FULL_JUPYTER_DIR) ppdsp-jupyter

run:
	lein run
build:
	lein uberjar
execute:
	java -jar target/jvm/uberjar/ppdsp-0.1.0-SNAPSHOT-standalone.jar

run-tests:
	lein test
