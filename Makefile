CFLAGS?=-Wall -O2
OWLURL=https://github.com/aoh/owl-lisp/releases/download/v0.1.14/ol-0.1.14.c.gz
USR_BIN_OL=/usr/bin/ol

everything: build test

build: myy.heap
# for now

bin/ol: tmp/ol.c
	mkdir -p bin
	cc -O -o bin/ol tmp/ol.c

tmp/ol.c:
	mkdir -p tmp
	curl -L https://github.com/aoh/owl-lisp/releases/download/v0.1.14/ol-0.1.14.c.gz | gzip -d > tmp/ol.c

myy.heap: myy.scm bin/ol
	bin/ol myy.scm > myy.heap

build: myy.heap
	echo "for now, just built myy.heap for inclusion in myy.ino"

test: myy.heap
	grep -q FP myy.heap
	grep -q heap myy.heap

clean:
	-rm -rf tmp
	-rm myy.heap
	-rm -rf bin

.PHONY: test build clean
