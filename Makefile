OWL=https://github.com/aoh/owl-lisp/releases/download/v0.1.14/ol-0.1.14.c.gz

everything: test build

build: arduino/myy/myy.ino

bin/ol: c/ol.c
	mkdir -p bin
	cc -O -o bin/ol c/ol.c

c/ol.c:
	curl -L $(OWL) | gzip -d > c/ol.c

arduino/myy/myy.ino: bin/ol c/myy.c c/arduino.prelude c/arduino.finale myy.scm heap.scm
	mkdir -p arduino/myy
	bin/ol --run myy.scm -o arduino/myy/myy.ino -p arduino heap.scm

test: bin/ol myy.scm
	test/run

arduino-test:
	test/run-arduino

flash: arduino/myy/myy.ino
	make flash_only
   
flash_only:
	arduino --board adafruit:samd:adafruit_trinket_m0 --upload arduino/myy/myy.ino

flash.result:
	# flash whatever is at arduino/myy/myy.ino and output halting value to flash.result
	make flash_only
	stty -F /dev/ttyACM0 9600 raw -clocal -echo -icrnl
	bin/ol -e "(time-ms)"
	cat /dev/ttyACM0 | grep --line-buffered HALT | head -n 1 | sed -e 's/HALT //' > flash.result
	bin/ol -e "(time-ms)"

cat:
	stty -F /dev/ttyACM0 9600 raw -clocal -echo -icrnl
	cat /dev/ttyACM0

clean:
	-rm -rf arduino
	-rm -rf bin

mrproper:
	make clean
	rm c/ol.c

.PHONY: test build clean mrproper flash cat flash_only
