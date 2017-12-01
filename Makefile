CFLAGS?=-Wall -O2
OWLURL=https://github.com/aoh/owl-lisp/releases/download/v0.1.14/ol-0.1.14.c.gz
USR_BIN_OL=/usr/bin/ol
HEAP=arduino/myy/myy.h

everything: build test

build: arduino/myy/myy.h

bin/ol: tmp/ol.c
	mkdir -p bin
	cc -O -o bin/ol tmp/ol.c

tmp/ol.c:
	mkdir -p tmp
	curl -L https://github.com/aoh/owl-lisp/releases/download/v0.1.14/ol-0.1.14.c.gz | gzip -d > tmp/ol.c

# a fixed test program for now
$(HEAP): bin/ol myy.scm test/foo.myy
	bin/ol --run myy.scm test/foo.myy $(HEAP)

build: $(HEAP)
	# heap ready to be included to arduino/myy/myy.ino

test: $(HEAP)
	grep -q FP $(HEAP)
	grep -q heap $(HEAP)

clean:
	-rm -rf tmp
	-rm $(HEAP)
	-rm -rf bin

.PHONY: test build clean
