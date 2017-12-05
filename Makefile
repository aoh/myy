OWL=https://github.com/aoh/owl-lisp/releases/download/v0.1.14/ol-0.1.14.c.gz

everything: test build

build: arduino/myy/myy.ino

bin/ol: c/ol.c
	mkdir -p bin
	cc -O -o bin/ol c/ol.c

c/ol.c:
	curl -L $(OWL) | gzip -d > c/ol.c

arduino/myy/myy.ino: bin/ol myy.scm heap.scm
	mkdir -p arduino/myy
	bin/ol --run myy.scm -o arduino/myy/myy.ino -p arduino heap.scm

test: bin/ol myy.scm
	test/run

clean:
	-rm -rf arduino
	-rm -rf bin

mrproper:
	make clean
	rm c/ol.c

.PHONY: test build clean
