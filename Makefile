CFLAGS?=-Wall -O2
OWLURL=https://github.com/aoh/owl-lisp/releases/download/v0.1.14/ol-0.1.14.c.gz
USR_BIN_OL=/usr/bin/ol
HEAP=arduino/myy/myy.h

everything: test build

build: arduino/myy/myy.ino

bin/ol: tmp/ol.c
	mkdir -p bin
	cc -O -o bin/ol tmp/ol.c

tmp/ol.c:
	mkdir -p tmp
	curl -L https://github.com/aoh/owl-lisp/releases/download/v0.1.14/ol-0.1.14.c.gz | gzip -d > tmp/ol.c

arduino/myy/myy.ino: bin/ol myy.scm heap.scm
	bin/ol --run myy.scm -o arduino/myy/myy.ino -p arduino heap.scm

build: arduino/myy/myy.ino

test: bin/ol myy.scm
	test/run

clean:
	-rm -rf tmp
	-rm arduino/myy/myy.ino
	-rm -rf bin

.PHONY: test build clean
