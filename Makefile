CFLAGS?=-Wall -O2
OWLURL=https://github.com/aoh/owl-lisp/releases/download/v0.1.14/ol-0.1.14.c.gz
USR_BIN_OL=/usr/bin/ol

everything: build test

build: myy.heap
# for now

bin/ol: ol.c
	mkdir -p bin
	cc -O -o bin/ol ol.c

ol.c:
	curl -L https://github.com/aoh/owl-lisp/releases/download/v0.1.14/ol-0.1.14.c.gz | gzip -d > ol.c

myy.heap: myy.scm bin/ol
	bin/ol myy.scm > myy.heap

build: myy.heap
	echo "for now, just built myy.heap for inclusion in myy.ino"

test: myy.heap
	grep -q FP myy.heap
	grep -q heap myy.heap

clean:
	-rm myy.heap
	-rm bin/ol

mrproper:
	make clean
	-rm ol.c
	-rm -rf bin
	
.PHONY: test build clean
