# Myy 

Myy is a portable non-trivial lisp system. It initially mainly targets 
ATSAMD21-based devices, such as Adafruit Trinket M0. The design strives to 
be portable. The intention is to make myy capable of running on various 
kinds for runtimes easily, from 16-bit microcontrollers with a few kilobytes 
of memory to native code on desktops and webassembly.


## Status

Myy is about a week old. It can compile simple programs to heap images, 
which can be fairly efficiently evaluated on a 16-bit VM running on an Adafruit
Trinket M0. The compiler chain steps are mostly clear, but only a few of the 
lower level transformations are currently implemented.


## Usage

Rlwrap isn't required, but you probably want to have it installed if you 
intend to do any testing or development within the repl.

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


