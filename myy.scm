;;; 
;;; System settings 
;;; 

; 256 when testing, ~8000 on trinket   
(define memsize 256)

(define last (- memsize 1))


;;;
;;; Data representation
;;;

; ptr:    [00pppppp pppppppp]
; fixnum: [10ffffff ffffff00] -> 12-bit, good for ADC / DAC
; enum:   [10vvvvvv vvvvvv10] -> (null, true, false, halt, ...)
; header: [10ssssss sRtttt11] -> 32 types (16 raw, 16 alloc), 64 words max size

(define bimm #x8000)
(define heapsize 256)

(define (make-immediate val type)
   (bor bimm (bor type (<< val 3))))

(define (make-header s t)
   (bor (bor bimm #b11) (bor (<< t 2) (<< s 7))))

(define inull  (make-immediate 0 #b10))
(define itrue  (make-immediate 1 #b10))
(define ifalse (make-immediate 2 #b10))
(define ihalt  (make-immediate 3 #b10))



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

;;;
;;; VM heap rendering
;;;

; output virtual memory as C

(define (output mem)
   (display "uint16_t heap[] = {")
   (fold
      (λ (_ pos)
         (display (get mem pos 0))
         (if (< pos last)
            (display ", ")))
      mem
      (iota 0 1 memsize))
   (print "};"))

(define empty-mem
   (put #empty 'fp 0))

(define (test exp)
   (print "ASSEMBLE " exp)
   ;(print "#define INULL  0x" (number->string inull 16))
   ;(print "#define ITRUE  0x" (number->string itrue 16))
   ;(print "#define IFALSE 0x" (number->string ifalse 16))
   ;(print "#define IHALT  0x" (number->string ihalt 16))
   (print "#define HEAPSIZE " heapsize)
   (lets ((mem entry (assemble empty-mem exp)))
      (output mem)
      (print "#define ENTRY " entry)
      (print "#define FP " (get mem 'fp 'bug))))


'(test
   `(proc
      (bytecode
         (arity 1)
         (ldf 1 ,ra1)
         (ldi ,rop 2 ,ra2)
         (ldi ,rop 3 ,ra3)
         (call ,ra3 4))
      4095
      (bytecode
         (arity 4)
         (jeq ,ra1 ,ra2
            (ret ,ra1))
         (sub ,ra1 ,ra2 ,ra2)
         (call ,ra3 4))))


;;;
;;; Low level lambda code -> BASIL
;;;

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
      (else #false)))

(define* (prim-call-to target exp)
   (cond
      ((not (pair? exp))
         #false)
      ((not (all register? (cdr exp)))
         ;; values must be in registers
         #false)
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
   '(+ - * / eq? cons car cdr if))

(define register-list
   '(a b c d e f g h i j k l m n o p))

(define (loadable? val)
   (or
      (and (fixnum? val) (>= val 0) (< val 16))
      (eq? val #true)
      (eq? val #false)
      (eq? val null)))

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
            (error "offset: not found: " val))
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
   (print "Let's do the register dance for " exp)
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
                        ((loadable? desired-val)
                           (loop (lset regs pos desired-val) (cons (list 'load desired-val pos) rinsts)))
                        ((offset lits desired-val) =>
                           (λ (pos)
                              (list (list 'lde (+ pos 2) pos))))
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

(define (dance-test exp)
   (print "DANCE " exp)
   (print (register-dance exp null))
   (print)
   (print)
   (print))

;(dance-test '(b a c 1))
;(dance-test '(a a a a))
(dance-test '(b a e))
   
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
(define* (ll-lambda->bytecode formals exp lits)
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
   (print "LL->BASIL " exp)
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

; r0=op, r1=mcp=a, r2=cont=b, r3=c, r4=d

(test
   (ll-value->basil
      '(lambda (a b c) 
         ((lambda (f)
            (f a b 1 4095 f))
            (lambda (a b c d e)
               (if (eq? c d)
                  (b a d)
                  ((lambda (c)
                     (e a b c d) )
                     (- d c))))))))


;;;
;;; CPS conversion
;;;

; input: macro-expanded code
; operation: add continuations *and* thread continuation
; output: ll-lambda



;;;
;;; Environment (only for REPL)
;;;

; input: sexp in which bindings are done with lambdas, env
; output: sexp in which references to global values are replaced by the corresponding values



;;; 
;;; Macro expansion
;;; 

; input: regular lisp, env
; output: lisp without macros


