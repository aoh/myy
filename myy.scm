#!/usr/bin/ol --run

;;; 
;;; System settings 
;;; 

; 256 when testing, ~8000 on trinket
(define memsize 256)

(define last (- memsize 1))

(define (debug . args)
   (for-each
      (λ (x) (display-to stderr x))
      (append args (list "\n"))))


;;;
;;; Minimal testing
;;;

(define-syntax check
   (syntax-rules ()
      ((check desired term)
         (let ((result term))
            (if (not (equal? result desired))
               (error "The computer says no."
                  (str (quote term) " is " result " instead of " desired ".")))))))

(check 42 (* 2 (+ 20 1)))



;;;
;;; Sexp pattern matching
;;;

(define (match exp pat)
   (define (matcher exp pat tail)
      (cond
         ((not tail) #false)
         ((eq? pat '?) (cons exp tail))
         ((function? pat) (if (pat exp) (cons exp tail) #false))
         ((eq? exp pat) tail)
         ((pair? pat)
            (if (pair? exp)
               (matcher (car exp) (car pat)
                  (matcher (cdr exp) (cdr pat) tail))
               #false))
         (else #false)))
   (matcher exp pat null))

(check '() (match 'a 'a))
(check '(a) (match 'a '?))
(check '() (match '(a b) '(a b)))
(check #false (match '(a x) '(a b)))
(check '(a (b c)) (match '(a (b c)) '(? ?)))

(define-syntax sexp-cases
   (syntax-rules (else ?)
      ((sexp-cases var (else . then))
         (begin . then))
      ((sexp-cases var ((pat . tern) formals . then) . others)
         (let ((m (match var (quasiquote (pat . tern)))))
            (if m
               (apply (lambda formals . then) m)
               (sexp-cases var . others))))
      ((sexp-cases var (sym . then) . others)
         (if (eq? var (quote sym))
            (begin . then)
            (sexp-cases var . others)))))

(define-syntax sexp-case
   (syntax-rules (else)
      ((sexp-case (else . args))
         (begin . args))
      ((sexp-case (op . args) . cases)
         (let ((var (op . args)))
            (sexp-case var . cases)))
      ((sexp-case var . cases)
         (sexp-cases var . cases))))

(check 'yes (sexp-case 'a (b () 'no) (a () 'yes) (else 'wat)))
(check 'yes
   (sexp-case '(a b)
      ((?) (x) 'no)
      ((? ?) (x y) 'yes)
      (else 42)))




;;;
;;; Data representation
;;;

; ptr:    [00pppppp pppppppp]
; fixnum: [10ffffff ffffff00] -> 12-bit, good for ADC / DAC
; enum:   [10vvvvvv vvvvvv10] -> (null, true, false, halt, ...)
; header: [10ssssss sRtttt11] -> 32 types (16 raw, 16 alloc), 64 words max size

(define bimm #x8000)
(define bmask #b11111111111111)

(define heapsize 256)

(define (make-immediate val type)
   (bor bimm (bor type (<< val 2))))

(define (make-header s t)
   (bor (bor bimm #b11) (bor (<< t 2) (<< s 7))))

(define inull  (make-immediate 0 #b10))
(define itrue  (make-immediate 1 #b10))
(define ifalse (make-immediate 2 #b10))
(define ihalt  (make-immediate 3 #b10))

(define (allocated? desc) (eq? 0 (band bimm desc)))
(define (immediate? desc) (not (allocated? desc)))
(define (fixnum n)
   (make-immediate n #b00))

(define (fixval desc)
   (>> (band desc bmask) 2))

(check #f (allocated? inull))
(check #t (immediate? (fixnum 42)))
(check 42 (fixval (fixnum 42)))
(check #t (immediate? (fixnum 42)))



;;;
;;; Bytecode assembly
;;;

; input: bytecode assembly intermediate language (s-exps)
; output: assembled code and required objects in vm heap[] array

(define (immediate->descriptor val)
   (cond
      ((and (fixnum? val) (< val 4096) (>= val 0))
         (bor bimm (<< val 2)))
      ((eq? val #true)
         itrue)
      ((eq? val #false)
         ifalse)
      ((eq? val null)
         inull)
      (else
         #false)))

(define (argto arg to)
   (bor (<< arg 4) to))

;; peephole optimizations here (e.g. mov + enter -> call)

(define (assemble-bytecode lst)
   (if (null? lst)
      null
      (lets ((op (car lst))
             (args (cdr lst)))
         (cond
            ((eq? (car op) 'ret)
               (ilist 5 (cadr op) (assemble-bytecode (cdr lst))))
            ((eq? (car op) 'arity)
               (ilist 8 (cadr op) (assemble-bytecode (cdr lst))))
            ((eq? (car op) 'ldf)
               (ilist 11 (argto (cadr op) (caddr op))
                  (assemble-bytecode (cdr lst))))
            ((eq? (car op) 'mov)
               (if (eq? (cadr op) (caddr op))
                  (assemble-bytecode (cdr lst))
                  (ilist 2 (argto (cadr op) (caddr op))
                     (assemble-bytecode (cdr lst)))))
            ((eq? (car op) 'add)
               (ilist 12 (argto (cadr op) (caddr op)) (car (cdddr op))
                  (assemble-bytecode (cdr lst))))
            ((eq? (car op) 'enter)
               (ilist 4 (cadr op)
                  ;; leave tail for debugging info
                  (cdr lst)))
            ((eq? (car op) 'sub)
               (ilist 14 (argto (cadr op) (caddr op)) (car (cdddr op))
                  (assemble-bytecode (cdr lst))))
            ((eq? (car op) 'mul)
               (ilist 13 (argto (cadr op) (caddr op)) (car (cdddr op))
                  (assemble-bytecode (cdr lst))))
            ((eq? (car op) 'div)
               (ilist 15 (argto (cadr op) (caddr op)) (car (cdddr op))
                  (assemble-bytecode (cdr lst))))
            ((eq? (car op) 'ldi) ;; ldi from offset to -> LDI (from | to) <offset>
               (ilist 3 (argto (cadr op) (car (cdddr op))) (caddr op)
                  (assemble-bytecode (cdr lst))))
            ((eq? (car op) 'lde) ;; lde (load by offset from r0)
               (ilist 6 (argto (cadr op) (caddr op))
                  (assemble-bytecode (cdr lst))))
            ((eq? (car op) 'call)
               (ilist 1 (argto (cadr op) (caddr op))
                  ;; leave reemainder as such for debugging info
                  (cdr lst)))
            ((eq? (car op) 'jeq)
               (lets ((otherwise (assemble-bytecode (cdr lst)))
                      (jump-len (+ 3 (length otherwise))))
                  (if (> jump-len 255)
                     (error "jeq: jump too large" jump-len))
                  (ilist 9 (argto (cadr op) (caddr op))
                     jump-len
                     (append otherwise
                        (assemble-bytecode (cdddr op))))))
            (else
               (error "assemble-bytecode: wat " op))))))

(define tbytecode 18) ; raw 2
(define tbytesp   17) ; raw, 1 pad
(define tbytes    16) ; raw 0
(define tproc      0)
(define tpair      3)

(define (chunk lst)
   (let loop ((lst lst) (last #f))
      (cond
         ((null? lst)
            (if last
               (loop '(0) last)
               null))
         (last
            (cons (bor (<< (car lst) 8) last)
               (loop (cdr lst) #f)))
         (else
            (loop (cdr lst) (car lst))))))

(define (burn-bytecode mem code)
   (lets ((words (chunk code))
          (words (cons (make-header (length words) tbytecode) words))
          (where (getf mem 'fp)))
      (let loop ((mem mem) (words words) (pos where))
         (if (null? words)
            (values (put mem 'fp pos) where)
            (loop (put mem pos (car words))
                  (cdr words)
                  (+ pos 1))))))

(define (assemble-fields mem fields assemble)
   (if (null? fields)
      (values mem fields)
      (lets ((mem this (assemble mem (car fields)))
             (mem rest (assemble-fields mem (cdr fields) assemble)))
         (values mem (cons this rest)))))

(define (assemble mem obj)
   (cond
      ((pair? obj)
         (cond
            ((eq? (car obj) 'bytecode)
               (lets
                  ((code (assemble-bytecode (cdr obj)))
                   (mem desc (burn-bytecode mem code)))
                  (values mem desc)))
            ((eq? (car obj) 'proc)
               (lets
                  ((mem fields (assemble-fields mem (cdr obj) assemble))
                   (pos (getf mem 'fp))
                   (hdr (make-header (length fields) tproc)))
                  (let loop ((mem mem) (fields (cons hdr fields)) (at pos))
                     (if (null? fields)
                        (values (put mem 'fp at) pos)
                        (loop
                           (put mem at (car fields))
                           (cdr fields)
                           (+ at 1))))))
            (else
               (error "assemble: wat pair " obj))))
      ((immediate->descriptor obj) =>
         (λ (val)
            (values mem val)))
      (else
         (error "assemble: wat " obj))))

(define (assembler-entry mem obj)
   (debug "Assembling " obj)
   (assemble mem obj))

;;;
;;; LL-lambda → BASIL
;;;

; Low Level lambda code has:
;  - explicit continuations
;  - explicit thread controller / error handler continuation
;  - fixed formal variables corresponding directly to runtime registers

; the code is a very small subset of the supported language, terms of which
; can still be evaluated as such within the full language

; input: code containing explicit continuations, lambdas and primitives
; output: BASIL sexp, including required linked objects


; r0 is silent
(define argument-regs
   '(a b c d e f g h i j k l m n o p))

(define regs
   (list->ff
      (zip cons
         '(a b c d e f g h i j k l m n  o p)
         (iota 1 1 15))))

(define (register? x)
   (get regs x #false))

(define (reg-num reg)
   (if (symbol? reg)
      (let ((n (get regs reg #f)))
         (if (not n)
            (error "not a register: " reg))
         n)
      reg))

(define (offset lst exp)
   (let loop ((pos 0) (lst lst))
      (cond
         ((null? lst) #false)
         ((eq? (car lst) exp) pos)
         (else (loop (+ pos 1) (cdr lst))))))

(define (binary-primop->opcode op)
   (cond
      ((eq? op '+) 'add)
      ((eq? op '-) 'sub)
      ((eq? op '*) 'mul)
      ((eq? op '/) 'div)
      ((eq? op 'cons) 'cons)
      (else #false)))

(define (unary-primop->opcode op)
   (cond
      ((eq? op 'car) 'car)
      ((eq? op 'cdr) 'cdr)
      (else #false)))
      
(define (prim-call-to target exp)
   (cond
      ((not (pair? exp))
         #false)
      ((not (all register? (cdr exp)))
         ;; values must be in registers
         #false)
      ((unary-primop->opcode (car exp)) =>
         (λ (op)
            (if (and (all register? (cdr exp)) 
                     (= (length (cdr exp)) 1))
               (cons op (append (map reg-num (cdr exp)) (list (reg-num target))))
               (error "prim-call-to: invalid args for " exp))))
      ((binary-primop->opcode (car exp)) =>
         (λ (op)
            (if (and (all register? (cdr exp))
                     (= (length (cdr exp)) 2))
               (cons op (append (map reg-num (cdr exp)) (list (reg-num target))))
               (error "prim-call-to: invalid args for " exp))))
      (else
         #false)))

(define (load-to reg exp lits)
   (cond
      ((symbol? exp)
         (let ((n (reg-num exp))
               (r (reg-num reg)))
            (if (eq? n r)
               null
               (list (list 'mov n r)))))
      ((fixnum? exp)
         (cond
            ((and (< exp #b10000) (>= exp 0))
               (list (list 'ldf exp (reg-num reg))))
            ((offset lits exp) =>
               (λ (pos)
                  (list (list 'lde (+ pos 2) (reg-num reg)))))
            (else
               (error "load-to: where is " exp))))
      ((prim-call-to reg exp) =>
         (λ (inst)
            (list inst)))
      ((offset lits exp) =>
         (λ (pos)
            (list (list 'lde (+ pos 2) (reg-num reg)))))
      (else
         (print "Could not find " exp " from literals " lits)
         (error "load-to: wat " exp))))

(define (load-args args lits)
   (let loop ((args args) (lits lits) (regs argument-regs) (out null))
      (cond
         ((null? args)
            out)
         ((load-to (car regs) (car args) lits) =>
            (λ (loads)
               (loop (cdr args) lits (cdr regs) (append loads out))))
         (else
            (error "load-args: wat " args)))))

(define (list-heading-and-len? head len)
   (λ (exp)
      (and (pair? exp)
           (eq? (car exp) head)
           (= (length exp) len))))

(define lambda?
   (list-heading-and-len? 'lambda 3))

(define if?
   (list-heading-and-len? 'if 4))

(define primops
   '(+ - * / eq? cons car cdr if %ref %close))

(define (primitive? exp)
   (has? primops exp))

(define register-list
   '(a b c d e f g h i j k l m n o p))

(define (loadable-fixnum? val)
   (or
      (and (fixnum? val) (>= val 0) (< val 16))
      ;(eq? val #true)
      ;(eq? val #false)
      ;(eq? val null)
      ))

(define (dead-register all-regs exp needed-vals)
   (let loop ((regs (cdr all-regs)) (exp (cdr exp)) (pos 1))
      (cond
         ((null? regs)
            #false)
         ((null? exp)
            #false)
         ((eq? (car regs) (car exp))
            ;; value already correct
            (loop (cdr regs) (cdr exp) (+ pos 1)))
         ((register? (car regs))
            (cond
               ((not (has? needed-vals (car regs)))
                  ;; register has a useless value
                  pos)
               ((> (length (keep (λ (x) (eq? x (car regs))) all-regs)) 1)
                  ;; we've got more where this came from, fill it!
                  pos)
               (else
                  ;; only one occurrence of the value - we mustn't lose this!
                  (loop (cdr regs) (cdr exp) (+ pos 1)))))
         (else
            ;; register has an immediate value, meaning we have put it there
            ;; intentionally here, so it's perfect
            (loop (cdr regs) (cdr exp) (+ pos 1))))))

(define (set lst)
   (cond
      ((null? lst) null)
      ((has? (cdr lst) (car lst))
         (set (cdr lst)))
      (else (cons (car lst) (set (cdr lst))))))

(define (xref lst pos)
   (cond
      ((null? lst) #false)
      ((eq? pos 0) (car lst))
      (else (xref (cdr lst) (- pos 1)))))

(define (offset lst val)
   (let loop ((lst lst) (pos 0))
      (cond
         ((null? lst)
            #false
            ;(error "offset: not found: " val)
            )
         ((eq? (car lst) val)
            pos)
         (else
            (loop (cdr lst) (+ pos 1))))))

(define (equal-prefix? a b)
   (cond
      ((null? a) #true)
      ((null? b) #true)
      ((eq? (car a) (car b))
         (equal-prefix? (cdr a) (cdr b)))
      (else
         #false)))

(define (register-dance exp lits)
   (lets
      ((rator (car exp))
       (regs (cons '_ register-list)) ;; add the silent r0, which will hold the operator after call / at enter
       (needed-regs                     ;; registers whose initial values must be available at call time
         (set (if (register? rator)
               (cons rator (keep register? exp))
               (keep register? exp))))
       (solvable?
          (λ (vals)
             (null? (diff needed-regs regs)))))
      (let loop ((regs   regs)
                 (rinsts null))
         ;(print (list 'loop regs '-> exp 'insts rinsts))
         (cond
            ((not (solvable? regs))
               (print regs " is not solvable, because it does not have " needed-regs)
               #false)
            ((equal-prefix? (cdr regs) (cdr exp))
               (if (eq? (car regs) (car exp))
                  (reverse (cons (list 'enter 0 (length (cdr exp))) rinsts))
                  (reverse
                     (cons (list 'call (offset regs rator) (length (cdr exp)))
                        rinsts))))
            ((dead-register regs exp needed-regs) =>
               (λ (pos)
                  (let ((desired-val (xref exp pos)))
                     (cond
                        ((not desired-val)
                           ;; unused
                           (loop (lset regs pos #false) rinsts))
                        ((register? desired-val)
                           (let ((val-pos (offset regs desired-val)))
                              (loop
                                 (lset regs pos desired-val)
                                 (cons (list 'mov val-pos pos) rinsts))))
                        ((loadable-fixnum? desired-val)
                           (loop (lset regs pos desired-val) (cons (list 'ldf desired-val pos) rinsts)))
                        ((offset lits desired-val) =>
                           (λ (lpos)
                              (loop
                                 (lset regs pos desired-val)
                                 (cons (list 'lde (+ lpos 2) pos) rinsts))))
                        ((load-to pos desired-val lits) =>
                           (λ (insts)
                              (loop 
                                 (lset regs pos desired-val)
                                 (append (reverse insts) rinsts))))
                        (else
                           (error "register-dance: wat " desired-val))))))
            ((eq? (car regs) '_)
               (cond
                  ((register? rator)
                     (loop (lset regs 0 rator) (cons (list 'mov (reg-num rator) 0) rinsts)))
                  (else
                     (error "no dead values, rator is " rator))))
            ;; swaps here
            (else
               (error "no dead registers in " regs))))))

(define (ll-exp->bytecode exp lits)
   (cond
      ((eq? (car exp) 'if)
         (let ((test (cadr exp))
               (then (caddr exp))
               (else (car (cdddr exp))))
            (if (and (pair? test) (eq? (car test) 'eq?) (all symbol? (cdr test)))
               (cons
                  (ilist 'jeq (reg-num (cadr test)) (reg-num (caddr test))
                     (ll-exp->bytecode then lits))
                  (ll-exp->bytecode else lits))
               (error "odd test: " test))))
      ((lambda? (car exp))
         (lets ((rator (car exp))
                (formals (cadr rator))
                (body (caddr rator))
                (rands (cdr exp)))
            (if (= 1 (length formals) (length rands))
               ;; binding of one register
               (append
                  (load-to (car formals) (car rands) lits)
                  (ll-exp->bytecode body lits))
               (error "multiple bind in head lambda in ll" formals))))
      ((register-dance exp lits) =>
         (λ (steps)
            steps))
      (else
         (error "ll-exp->bytecode: unable to generate call: " exp))))

;; literals are accessable via r0
(define (ll-lambda->bytecode formals exp lits)
   (if (not (pair? exp))
      (error "ll-lambda->bytecode: wat " exp))
   (ilist 'bytecode
      (list 'arity (length formals))
      (ll-exp->bytecode exp lits)))

(define (maybe-drop-primop exp)
   (if (has? primops (car exp))
      (cdr exp)
      exp))

(define (find-literals seen exp)
   (cond
      ((lambda? exp)
         (find-literals (cons exp seen) (cadr exp)))
      ((number? exp)
         (if (> exp 15)
            (cons exp seen)
            seen))
      ((null? exp)
         seen)
      ((list? exp)
         (if (lambda? (car exp))
            (fold find-literals
               (find-literals seen (caddr (car exp)))
               (cdr exp))
            (fold find-literals seen
               (maybe-drop-primop exp))))
      ((reg-num exp)
         seen)
      (else
         (error "find-literals: what is " exp))))

(define (literals exp)
   (find-literals null exp))

(define (ll-value->basil exp)
   (debug "LL->BASIL " exp)
   (cond
      ((lambda? exp)
         (lets
            ((formals (cadr exp))
             (body (caddr exp))
             (lits (literals body))
             (bc
               (ll-lambda->bytecode
                   (cadr exp)
                   (caddr exp)
                   lits)))
            (if (null? lits)
               bc
               (ilist 'proc bc
                  (map ll-value->basil lits)))))
      ((and (fixnum? exp) (>= exp 0) (< exp 4096))
         exp)
      (else
         (error "ll-value->basil: wat " exp))))


;;;
;;; α-conversion
;;;

; input: CPS-converted code, in which arbitrary variables are used
; output: code in which a fixed set of variables, corresponding directly to registers, are used

(define (alpha-rename exp env)
   (cond
      ((symbol? exp)
         (let ((reg (get env exp #false)))
            (if reg
               reg
               exp)))
      ((pair? exp)
         (cond
            ((eq? (car exp) 'quote)
               exp)
            ((primitive? (car exp))
               (cons (car exp) (map (λ (exp) (alpha-rename exp env)) (cdr exp))))
            ((eq? (car exp) 'lambda)
               ;; fixme: check argument count
               (lets
                  ((formals (cadr exp))
                   (body (caddr exp))
                   (env
                      (fold (λ (env var-reg) (put env (car var-reg) (cdr var-reg)))
                         env
                         (zip cons formals argument-regs))))
                 (list 'lambda (take argument-regs (length formals))
                    (alpha-rename body env))))
            ((has? '(%proc %clos) (car exp))
               (cons (car exp)
                  (map (λ (x) (alpha-rename x env)) (cdr exp))))
            (else
               (map (λ (x) (alpha-rename x env)) exp))))
      ((number? exp)
         exp)
      (else
         (error "alpha-rename: wat (usually ok) " exp)
         exp)))


(define (alpha-convert exp)
   (alpha-rename exp #empty))

(check '(lambda (a) a) (alpha-convert '(lambda (x) x)))
(check '(lambda (a) (lambda (a) a))
        (alpha-convert '(lambda (x) (lambda (y) y))))
(check
   '(%proc (lambda (a b) (%close a 0 b))
           (lambda (a b) (%ref a 0)))
   (alpha-convert
      '(%proc
         (lambda (L x) (%close L 0 x))
         (lambda (E y) (%ref E 0)))))

;;;
;;; CLP, closure and literal passing style
;;;

; closure- and literal passing style: make references to environment variables
; and values not loadable or referable by bytecode instructions alone explicit.

; (lambda (x) 9000) → (%proc (lambda (L x) (%ref x 0)) 9000)
; (lambda (x) (lambda (y) y)) →
;    (%proc (lambda (L x) (%ref L 0))
;        (lambda (C y) y))
; (lambda (x) (lambda (y) x)) →
;   (%proc (lambda (C x) (%proc-imm 0 x)) ; <- close literal at offset 0 with x
;      (lambda (C y) (%ref C 0)))


;;;
;;; CPS conversion
;;;

; (lambda (x) x) → (lambda (c x) (c x))
; (lambda (x) (x x)) → (lambda (c x) (x c x))

; input: macro-expanded code
; operation: add continuations *and* thread continuation
; output: ll-lambda


;;;
;;; Explicit Recursion
;;;

; input: s-exp in which implicit recursion may be present (letrec)
; output: s-exp in which recursion is handled with lambdas


;;;
;;; Environment bindings
;;;

; input: sexp in which bindings are done with lambdas, env
; output: sexp in which references to global values are replaced by the corresponding values


;;;
;;; Macro expansion
;;;

; input: regular lisp, env
; output: lisp without macros








; sketching operation

'(define (map fn lst)
   (if (null? lst)
      lst
      (cons (fn (car lst))
            (map fn (cdr lst)))))

'(define map
   (lambda (fn lst)
      ((lambda (rec)
         (rec fn lst rec))
       (lambda (fn lst rec)
          (if (null? lst)
             lst
             (cons (fn (car lst))
                   (map fn (cdr lst))))))))

'(define map
   (lambda (fn lst)
      ((lambda (rec)
         (rec fn lst rec))
       (lambda (fn lst rec)
          (if (null? lst)
             lst
             (cons (fn (car lst))
                   (map fn (cdr lst))))))))

'(define map
   (lambda (m k fn lst)
      ((lambda (rec)
         (rec m k fn lst rec))
       (lambda (m k fn lst rec)
          (if (null? lst)
             (k m lst)
             (fn m
                (lambda (m hd)
                   (rec m
                      (lambda (m tl)
                         (k m (cons hd tl)))
                      fn (cdr lst) rec))
                (car lst)))))))

'(define map
   (lambda (m k fn lst)
      ((lambda (rec)
         (rec m k fn lst rec))
       (lambda (m k fn lst rec)
          (if (null? lst)
             (k m lst)
             (fn m
                (lambda (m hd)
                   (rec m
                      (lambda (m tl)
                         (k m (cons hd tl)))
                      fn (cdr lst) rec))
                (car lst)))))))

'(define map
   (%proc
      (lambda (L m k fn lst)
         ((lambda (rec)
            (rec m k fn lst rec))
           (%ref L 0)))
      (%proc
         (lambda (L m k fn lst rec)
             (if (null? lst)
                (k m lst)
                (fn m (%close-ref L 0 k hd) (car lst))))   ; <- need to know the closure info before compiling body
          (%close (L k hd)
            (lambda (m tl) (k (%ref L 0) (cons (%ref L 1) tl)))))))


;;;
;;; Temporary compiler entry
;;;

(define (maybe op arg)
   (if arg (op arg) arg))         

(import (owl args)) ;; command line argument parsing

(define (accept-platform s)
   (if (or (equal? s "unix") 
           (equal? s "arduino"))
       s
       #false))

(define command-line-rules
   (cl-rules
      `((help "-h" "--help" comment "show this thing")
        (output "-o" "--output" has-arg 
           default "-"
           comment "output file, or - for stdout")
        (runtime "-r" "--runtime" has-arg
           default "c/myy.c")
        (platform "-p" "--platform" cook ,accept-platform
           default "arduino"
           comment "choose platform to generate code for (unix or arduino)"))))
                                          
(define (render-heap mem)
   (foldr string-append ""
      (cons
         "uint16_t heap[] = {"
         (foldr
            (λ (pos out)
               (ilist (str (get mem pos 0))
                  (if (< pos last)
                     ", "
                     "")
                  out))
            (list "};\n")
            (iota 0 1 memsize)))))

(define empty-mem
   (put #empty 'fp 0))

(define (output-heap exp port)
   (lets ((mem entry (assembler-entry empty-mem exp)))
      (for-each
         (λ (thing)
            (print-to port thing))
         (list
            (str "#define INULL  0x" (number->string inull 16))
            (str "#define ITRUE  0x" (number->string itrue 16))
            (str "#define IFALSE 0x" (number->string ifalse 16))
            (str "#define IHALT  0x" (number->string ihalt 16))
            (str "#define HEAPSIZE " heapsize)
            (render-heap mem)
            (str "#define ENTRY " entry)
            (str "#define FP " (get mem 'fp 'bug))))))


(define (test-compiler exp port)
   (print-to stderr "Compiling " exp)
   (output-heap
      (ll-value->basil (alpha-convert exp))
      port))

(define (maybe op arg)
   (if arg (op arg) arg))

(define (maybe-open-output-file path)
   (if (equal? path "-")
      stdout
      (open-output-file path)))

(import (owl sexp))

(define (myy-read-file path)
   (maybe car (maybe list->sexps (file->list path))))

(define (output-platform dict section port)
   (lets
      ((path (str "c/" (getf dict 'platform) "." section))
       (data (file->list path)))
      (if data
         (write-bytes port data)
         (error "failed to read platform code from " path))))

(define (myy-entry dict args)
   (cond
      ((getf dict 'help)
         (print (format-rules command-line-rules))
         0)
      ((not (getf dict 'runtime-data)) ;; load runtime if necessary
         (let ((data (file->list (getf dict 'runtime))))
            (if data
               (myy-entry (put dict 'runtime-data data) args)
               (error "failed to read runtime " (getf dict 'runtime)))))
      ((null? args)
         (print "I need something to compile")
         1)
      (else
         (lets
            ((port (open-input-file (car args)))
             (output (maybe-open-output-file (getf dict 'output)))
             (exp  (maybe read port)))
            (cond
               ((not port) 
                  (error "Failed to open " (car args)))
               ((not output)
                  (error "Cannot write to " (getf dict 'output)))
               (else
                  ;; output heap contents
                  (print "Running test compiler")
                  (output-platform dict "prelude" output)
                  (test-compiler exp output)
                  (debug "Output written to " (getf dict 'output))
                  ;; output shared runtime
                  (print "writing runtime")
                  (write-bytes output 
                     (get dict 'runtime-data null))
                  (print "ok")
                  (output-platform dict "finale" output)
                  (close-port output)
                  0))))))

(λ (args)
   (process-arguments (cdr args) command-line-rules
      "usage: myy.scm [args] [sourcefile]"
      myy-entry))
