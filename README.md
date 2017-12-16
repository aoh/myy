# Myy 

Myy is a small Lisp system. It is both an attempt to implement a lean
interactive multithreading programming environment for current 
ATSAMD21-based cheap microcontrollers (such as Adafruit Trinket M0),
and a rewrite of the Owl Lisp core. 

The implementation currently consists of a compiler written in Owl Lisp, 
which produces standalone C programs for either Arduino or Unix platforms. 

## Status

Myy can currently run simple programs, consisting of a subset of the 
intended language, on suitable microcontrollers and on the development
machine itself. Tests consist mainly of tiny programs checking whether
the virtual machine halts on the correct exit value, which on unix 
is used as the program return value and on Arduino is printed via serial 
console.

## Usage

Rlwrap isn't required, but you probably want to have it installed if you 
intend to do any testing or development within the repl.

```
$ git clone https://github.com/aoh/myy.git
$ cd myy
$ make test
[...]
$ rlwrap bin/ol
You see a prompt.
> ,load "myy.scm"
;; Loaded myy.scm
> ,find alpha
current toplevel: alpha-rename, alpha-convert
   (owl alpha): alpha-convert
> (alpha-convert '(lambda (x dx) (+ x dx)))
ALPHA-CONVERT: (lambda (x dx) (+ x dx))
'(lambda (a b) (+ a b))
> 
```

## Use on Trinket

The tests can currently be run on a Trinket M0 connected as /dev/ttyACM0 by 
issuing `make arduino-test`.

