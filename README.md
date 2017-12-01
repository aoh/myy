# Myy 

Myy is a portable non-trivial lisp system mainly initially targeting 16-bit 
microcontrollers with several kilobytes of memory, such as ATSAMD21-based. It 
initially aims to run on ATSAMD21-based devices, such as Adafruit Trinket M0, 
but the design strives to be platform agnostic making it usable on on also
various other kinds of devices and runtimes. 

The main goals are to put small microchips to good use, get a next generaion 
runtime for/of Owl Lisp, and to get a really compact and portable system 
capable of running on various runtims and kinds of hardware.


## Status

Myy is about a week old. It can run compile simple programs to heap images, 
which can be fairly efficiently evaluated by a 16-bit VM running on an Adafruit
Trinket M0. The compiler chain steps are mostly clear, but only a few of the 
lower level transformations are currently implemented.


## Usage

You probably want to have rlwrap installed if you want to do testing 
within the repl:

```
$ git clone https://github.com/aoh/myy.git
$ cd myy
$ make
$ rlwrap bin/ol
You see a prompt.
> ,load "myy.scm"
;; Loaded myy.scm
> ,find binary
current toplevel: binary-primop->opcode, binary-port?
   (owl base): binary-port?
   (scheme base): binary-port?
> (binary-primop->opcode '+)
'add
> 
```

You can also use myy.scm as a program, like it is used in the Makefile, by 
giving owl the --run flag. This causes the last value of the program to be 
called with the commands following --run in the command line.

```
$ echo '(lambda (k x) (k x))' > foo
$ ol --run myy.scm foo -
[test heap dump follows]
```

It is often convenient to start a terminal, tmux window etc running 
`watch ol --run myy.scm ourprogram.myy -` or `watch make` when making 
changes to the compiler to see the effect of the changes while they are 
being made.

