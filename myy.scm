;;;
;;; Sexp mathing
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

(define-syntax sexp-cases
   (syntax-rules (else)
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
;;; Data Encoding
;;;


;; Allocated case
;; 
;; ,------------------------------+-----> allocated object pointer to an *even* word
;; |                              |,----> pairness if alloc (otherwise car is a header)
;; |                              ||
;;(s)______ ________ ________ tttttPIG
;; |                        | |   ||||
;; |                        | |   |||`--> GC flag
;; `------------------------+ |   ||`---> immediateness
;;                          | '---+`----> fixnumness if immediate
;;                          |     `-----> immediate object type (32 options)
;;                          `-----------> immediate payload (typically signed)
;; Immediate case

;; object types in memory
;;  - nonpair, header is alloc -> function [lambda-code . env]


(define (ptr n) (<< n 2))
(define (imm-fixval n) (>> n 3))
(define (allocated? n) (eq? 0 (band n #b10)))
(define (immediate? n) (not (allocated? n)))
(define (fixnum? n) (eq? #b110 (band n #b110)))
(define (mem-cons? n) (eq? #b100 (band n #b110)))
(define (mem-pair? n) (eq? #b000 (band n #b110)))
(define (mk-fixnum n) (band #x7fffffff (bor (<< n 3) #b110)))
(define (fixnum-val n) (>> n 3))
(define (mk-immediate type payload)
   (bor (<< payload 8) (bor (<< type 3) #b010)))

;; encoding tests
(check #true (immediate? (mk-fixnum 42)))
(check 42 (imm-fixval (mk-fixnum 42)))
(check #true (fixnum? (mk-fixnum 42)))
(check #false (fixnum? (mk-immediate 0 0)))
(check #false (allocated? (mk-immediate 0 0)))
(check #false (allocated? (mk-fixnum 42)))

(define myy-null (mk-immediate 0 0))
(define myy-true (mk-immediate 1 1))
(define myy-false (mk-immediate 1 0))

(define (myy-null? desc) (eq? desc myy-null))
 
(define (marked? word) (eq? 1 (band word 1)))

(define (check-pointer mem ptr)
   (cond
      ((not (number? ptr))
         (error "Bug: invalid pointer: " ptr))
      ((< ptr 0)
         (error "Pointer below memory: start: " ptr))
      ((not (< ptr (getf mem 'end)))
         (error "Pointer past memory end: " (list 'pointer ptr 'end (getf mem 'end))))
      ((not (eq? 0 (band ptr #b11)))
         (error "Misaligned pointer: " ptr))))

(define (read mem ptr)
   (check-pointer mem ptr)
   (let ((val (get mem ptr myy-null)))
      (if val val
         (error "Invalid memory access: " ptr))))

(define (write mem ptr val)
   (check-pointer mem ptr)
   (put mem ptr val))

;; remove pairness bit if present
(define (maybe-untag ptr) (if (eq? 0 (band ptr #b100)) ptr (bxor ptr #b100)))
(define (mem-car mem ptr) (if (immediate? ptr) (error "car on immediate " ptr) (read mem (maybe-untag ptr))))
(define (mem-cdr mem ptr) (if (immediate? ptr) (error "cdr on immediate " ptr) (read mem (+ (maybe-untag ptr) 4))))
(define (mem-car! mem ptr val) (write mem (maybe-untag ptr) val))
(define (mem-cdr! mem ptr val) (write mem (+ (maybe-untag ptr) 4) val))
   
(define (set-mark word)
   (if (marked? word)
      (error "trying to remark word " word)
      (bor word 1)))

(define (unset-mark word)
   (if (marked? word)
      (bxor word 1)
      (error "trying to unmark unmarked " word)))

;; GC - sweep 

(define (sweep mem)
   (let loop ((mem mem) (pos (- (get mem 'end 8) 8)) (free 0))
      (if (eq? pos 0)
         (put mem 'free free)
         (let ((val (mem-car mem pos)))
            (if (marked? val)
               (loop (mem-car! mem pos (unset-mark val)) (- pos 8) free)
               (loop (mem-cdr! mem pos free) (- pos 8) pos))))))

(define (make-memory limit)
   (if (even? limit)
      (sweep
         (-> #empty (put 'end (* limit 8))))
      (error "list structured memory needs an even number of words, but got" limit)))

(define (mem-pair mem a b)
   (let ((free (getf mem 'free)))
      (if (eq? free myy-null)
         (error "gc needed" free)
         (values
            (-> mem
               (put 'free (mem-cdr mem free))
               (write free a)
               (write (+ free 4) b))
            free))))

(define (mem-cons mem a b)
   (lets ((mem desc (mem-pair mem a b)))
      (values mem (bor desc #b100))))
   
;; memory tests
(check 42 (-> (make-memory 10) (write (ptr 0) 42) (read (ptr 0))))
(check 42 (-> (make-memory 10) (write (ptr 5) 42) (read (ptr 5))))
(check 42 (-> (make-memory 10) (write (ptr 3) 24) (write (ptr 3) 42) (read (ptr 3))))
(check 20 (lets ((m (make-memory 10)) (m ptr (mem-cons m 22 2))) (- (mem-car m ptr) (mem-cdr m ptr))))
(check 11 (lets ((m (make-memory 10)) (m a (mem-cons m 11 22)) (m b (mem-cons m 33 a))) (mem-car m (mem-cdr m b))))
  
;;;
;;; GC - marking 
;;;

(define (mark mem root)
   (define (process mem ptr parent)
      (if (immediate? ptr)
         (backtrack mem ptr parent)
         (let ((val (mem-car mem ptr)))
            (if (marked? val)
               (backtrack mem ptr parent)
               (let ((mem (mem-car! mem ptr (set-mark parent))))
                  (process mem val (bor ptr 2)))))))
   (define (backtrack mem ptr parent)
      (cond
         ((eq? parent myy-null)
            (values mem ptr))
         ((eq? 2 (band parent 2)) ;; car is done
            (lets 
               ((parent (bxor parent 2))
                (foo (mem-cdr mem parent))
                (mem (mem-cdr! mem parent (unset-mark (mem-car mem parent))))
                (mem (mem-car! mem parent (set-mark ptr))))
               (process mem foo parent)))
         (else ;; cdr is done
            (lets
               ((foo (mem-cdr mem parent))
                (mem (mem-cdr! mem parent ptr)))
               (backtrack mem parent foo)))))
   (process mem root myy-null))


(check (mk-fixnum 42) (lets ((mem val (mark (make-memory 10) (mk-fixnum 42)))) val))

(lets ((mem (make-memory 10))
       (mem a (mem-cons mem (mk-fixnum 22) (mk-fixnum 33)))
       (mem b (mem-cons mem (mk-fixnum 1) (mk-fixnum 2)))
       (mem c (mem-cons mem (mk-fixnum 11) a))
       (mem cp (mark mem c)))
   (check #true (marked? (mem-car mem c)))
   (check #false (marked? (mem-cdr mem c)))
   (check #false (marked? (mem-car mem b)))
   (check #false (marked? (mem-cdr mem b)))
   (check #true (marked? (mem-car mem a)))
   (check #false (marked? (mem-cdr mem a))))


;;;
;;; Instruction format (de/encoding the number payload)
;;;

(define (mk-inst-unary opcode a)
   (bor opcode (<< a 6)))

;; 6-bit opcode, 8-bit a, n>=8-bit b
(define (mk-inst opcode a b)
   (mk-inst-unary opcode (bor a (<< b 8))))

(define (decode-inst n)
   (values (band n #b111111)
           (band (>> n 6) #b11111111)
           (>> n 14)))

;; instruction checks
(check (list 11 22 33)
   (lets ((a b c (decode-inst (mk-inst 11 22 33))))
      (list a b c)))


;;;
;;; Data transfer to virtual memory, simple acyclic version for compiler output
;;;

(define (burn mem obj)
   (cond
      ((number? obj)
         (values mem (mk-fixnum obj)))
      ((eq? obj #true) (values mem myy-true))
      ((eq? obj #false) (values mem myy-false))
      ((pair? obj)
         (lets ((mem hd (burn mem (car obj)))
                (mem tl (burn mem (cdr obj))))
            (mem-cons mem hd tl)))
      ((tuple? obj)
         (lets ((mem hd (burn mem (ref obj 1)))
                (mem tl (burn mem (ref obj 2))))
            (mem-pair mem hd tl)))
      ((null? obj)
         (values mem myy-null))
      (else
         (error "burn: how should i encode " obj))))

(define (create-memory obj mem-size)
   ;; mem obj -> mem descriptor
   (burn 
      (make-memory mem-size)
      obj))

(define (read-memory-object mem ptr)
   (cond
      ((immediate? ptr)
         (cond
            ((eq? ptr myy-null) '())
            ((eq? ptr myy-true) #true)
            ((eq? ptr myy-false) #false)
            ((fixnum? ptr)
               (fixnum-val ptr))
            (else
               (str "[IMMEDIATE " ptr "]"))))
      ((mem-cons? ptr)
         (cons (read-memory-object mem (mem-car mem ptr))
               (read-memory-object mem (mem-cdr mem ptr))))
      ((mem-pair? ptr)
         (tuple (read-memory-object mem (mem-car mem ptr))
                (read-memory-object mem (mem-cdr mem ptr))))
      (else 
         (str "[ALLOC " ptr "]"))))

(check 42 (lets ((mem ptr (create-memory 42 10))) (read-memory-object mem ptr)))     
(check (cons 1 2) (lets ((mem ptr (create-memory (cons 1 2) 10))) (read-memory-object mem ptr)))
(let ((val '(1 2 3 (4 . 5) . 6)))
   (check val (lets ((mem ptr (create-memory val 10))) (read-memory-object mem ptr))))

;;;
;;; SECD VM
;;;

(define op-apply           1)
(define op-close           2)
(define op-load-pos        3)
(define op-return          4)
(define op-load-env        5)
(define op-call            6)
(define op-cons            7)
(define op-car             8)
(define op-cdr             9)
(define op-load-immediate 10)
(define op-load-value     11)
(define op-equal          12)
(define op-if             13)
(define op-add            14)
(define op-sub            15)
(define op-set            16)
(define op-pig            17)
(define op-set-pig        18)
(define op-mul            19)

(define (mk-closure mem code stack env)
   (lets 
      ((mem x (mem-cons mem stack env))
       (mem x (mem-pair mem code x))) ;; non-pair, alloc at car
      (values mem x)))

(define (closure? thing)
   (and (pair? thing) (eq? 'closure (car thing))))

(define (vm-apply s e c d rator rands)
   (cond
      ((closure? rator)
         (lets ((ccode (cadr rator)) (cenv (caddr rator)))
            (values 
               rands cenv ccode (cons (list s e c) d))))
      (else
         (error "Cannot apply " rator))))

(define (mem-list-ref mem lptr n)
   (cond
      ((eq? lptr myy-null)
         (error "myy list ref out of list: " n))
      ((eq? n 0)
         (mem-car mem lptr))
      (else
         (mem-list-ref mem (mem-cdr mem lptr) (- n 1)))))

(define (mem-arg-list mem s offs)
   (if (myy-null? offs)
      (values mem myy-null)
      (lets 
         ((mem tl (mem-arg-list mem s (mem-cdr mem offs)))
          (hd (mem-list-ref mem s (fixnum-val (mem-car mem offs)))))
         (mem-cons mem hd tl))))
      
(define (execute mem s e c d instruction)
   (lets ((op a b (decode-inst (fixnum-val instruction))))
      (case op
         (op-close 
            (lets ((mem clos (mk-closure mem (mem-car mem c) s e))
                   (mem s (mem-cons mem clos s)))
               (values mem s e (mem-cdr mem c) d)))
         (op-return 
            (lets ((rval (mem-list-ref mem s a))
                   (st (mem-car mem d))
                   (d (mem-cdr mem d))
                   (s (mem-car mem st)) (st (mem-cdr mem st))
                   (e (mem-car mem st)) (st (mem-cdr mem st))
                   (c (mem-car mem st))
                   (mem s (mem-cons mem rval s)))
               (values mem s e c d)))
         (op-load-immediate
            ;; load immediate directly after instruction opcode
            (lets ((mem s (mem-cons mem (>> instruction 6) s)))
               (values mem s e c d)))
         (op-load-pos
            (lets ((mem s (mem-cons mem (mem-list-ref mem s a) s)))
               (values mem s e c d)))
         (op-load-value
            (lets ((mem s (mem-cons mem (mem-car mem c) s)))
               (values mem s e (mem-cdr mem c) d)))
         (op-equal
            (lets ((res (if (eq? (mem-list-ref mem s a) (mem-list-ref mem s b)) myy-true myy-false))
                   (mem s (mem-cons mem res s)))
               (values mem s e c d)))
         (op-add
            (lets ((a (mem-list-ref mem s a))
                   (b (mem-list-ref mem s b))
                   (x (mk-fixnum (+ (fixnum-val a) (fixnum-val b))))
                   (mem s (mem-cons mem x s)))
               (values mem s e c d)))
         (op-sub
            (lets ((a (mem-list-ref mem s a))
                   (b (mem-list-ref mem s b))
                   (x (mk-fixnum (- (fixnum-val a) (fixnum-val b))))
                   (mem s (mem-cons mem x s)))
               (values mem s e c d)))
         (op-mul
            (lets ((a (mem-list-ref mem s a))
                   (b (mem-list-ref mem s b))
                   (x (mk-fixnum (* (fixnum-val a) (fixnum-val b))))
                   (mem s (mem-cons mem x s)))
               (values mem s e c d)))
         (op-if
            (values mem s e
               (if (eq? (mem-list-ref mem s a) myy-false) (mem-cdr mem c) (mem-car mem c))
               d))
         (op-load-env
            (lets ((val (mem-list-ref mem (mem-list-ref mem e a) b))
                   (mem s (mem-cons mem val s)))
               (values mem s e c d)))
         (op-cons
            (lets
               ((a (mem-list-ref mem s a))
                (b (mem-list-ref mem s b))
                (xx (read-memory-object mem d))
                (mem val (mem-cons mem a b))
                (yy (read-memory-object mem d))
                (mem s (mem-cons mem val s)))
               (values mem s e c d)))
         (op-car
            (lets
               ((a (mem-list-ref mem s a)))
               (if (mem-cons? a)
                  (lets ((mem s (mem-cons mem (mem-car mem a) s)))
                     (values mem s e c d))
                  (error "vm car on non-pair " a))))
         (op-pig
            (lets ((a (mem-list-ref mem s a))
                   (mem s (mem-cons mem (mk-fixnum (band a 7)) s)))
               (values mem s e c d)))
         (op-set-pig
            (lets ((a (mem-list-ref mem s a))
                   (pig (fixnum-val (mem-list-ref mem s b)))
                   (ap (bor pig (band a #xfffffff8)))
                   (mem s (mem-cons mem ap s)))
               (values mem s e c d)))
         (op-cdr
            (lets
               ((a (mem-list-ref mem s a)))
               (if (mem-cons? a)
                  (lets ((mem s (mem-cons mem (mem-cdr mem a) s)))
                     (values mem s e c d))
                  (error "vm cdr on non-pair " a))))
         (op-call 
            (lets
               ((rator (mem-list-ref mem s a))
                (arity b)
                (mem args (mem-arg-list mem s (mem-car mem c)))
                (mem x (mem-cons mem (mem-cdr mem c) myy-null)) ;; dump 1st node
                (mem x (mem-cons mem e x))
                (mem x (mem-cons mem s x))
                (mem d (mem-cons mem x d)))
               ;; check closureness and arity later
               (values mem args (mem-cdr mem rator) (mem-car mem rator) d)))
         (op-apply
            (lets
               ((rator (mem-list-ref mem s a))
                (args (mem-list-ref mem s b))
                (mem x (mem-cons mem (mem-cdr mem c) myy-null)) ;; dump 1st node
                (mem x (mem-cons mem e x))
                (mem x (mem-cons mem s x))
                (mem d (mem-cons mem x d)))
               (values mem args (mem-cdr mem rator) (mem-car mem rator) d)))
         (else
            (error "Myy unknown instruction: " op)))))

(define (mem-length mem ptr)
   (let loop ((ptr ptr) (n 0))
      (if (eq? ptr myy-null)
         n
         (loop (mem-cdr mem ptr) (+ n 1)))))
   
(define (do-gc mem s e c d)
   ;(display "GC: ")
   (lets
      ((mem s (mark mem s))
       (mem e (mark mem e))
       (mem c (mark mem c))
       (mem d (mark mem d))
       (mem (sweep mem)))
      ;(print "end gc, free list length " (mem-length mem (getf mem 'free)))
      mem))
       
(define (transition mem s e c d)
      (if (myy-null? c)
         (lets ((state (mem-car mem d))
                (d (mem-cdr mem d))
                (s1 (mem-car mem state)) (state (mem-cdr mem state))
                (e1 (mem-car mem state)) (state (mem-cdr mem state))
                (c1 (mem-car mem state))
                (rval (mem-car mem s))
                (mem s1 (mem-cons mem rval s1)))
               (values mem s1 e1 c1 d))
         (execute mem s e (mem-cdr mem c) d (mem-car mem c))))

;; VM transition checks
(define-syntax check-transition
   (syntax-rules (=>)
      ((check-transition s e c d => sp ep cp dp)
         (lets ((sx ex cx dx (transition s e c d))
                (start (list s e c d))
                (desired (list sp ep cp dp))
                (got (list sx ex cx dx)))
            (if (not (equal? desired got))
               (error "The computer says no." (str "SECD " start " => \n    " got " instead of\n    " desired)))))))

 
;(check-transition 'S 'E (list (bor (<< 42 6) op-load-immediate)) 'D => '(42 . S) 'E null 'D)
;(check-transition 'S 'E (list op-load-value 42) 'D => '(42 . S) 'E () 'D)
;(check-transition 'S 'E (list op-close 'CODE) 'D => '((closure CODE (S . E)) . S) 'E () 'D)
;(check-transition '(s0 s1 s2) 'E (list (mk-inst op-load-pos 2 0)) 'D => '(s2 s0 s1 s2) 'E () 'D)
;(check-transition '((closure CC CE) (A0 A1) . S) 'E (cons (mk-inst op-apply 0 1) 'C) 'D =>
;                   '(A0 A1) 'CE 'CC '((((closure CC CE) (A0 A1) . S) E C) . D)) 
;(check-transition '(a x a b) 'E (list (mk-inst op-equal 0 2)) 'D => '(#true a x a b) 'E null 'D)
;(check-transition '(a x a b) 'E (list (mk-inst op-equal 0 1)) 'D => '(#false a x a b) 'E null 'D)
;(check-transition '(x #false) 'E (ilist (mk-inst op-if 0 0) 'THEN 'ELSE) 'D => '(x #false) 'E 'THEN 'D)
;(check-transition '(x #false) 'E (ilist (mk-inst op-if 1 0) 'THEN 'ELSE) 'D => '(x #false) 'E 'ELSE 'D)
;(check-transition '(x x RESULT) 'XE (list (mk-inst op-return 2 0)) '((S E C) . D)  => '(RESULT . S) 'E 'C 'D)



;;;
;;; SECD REPL
;;;

(define (vm mem s e c d)
   ;(print "---------------------------")
   ;(print " S = " (read-memory-object mem s))
   ;(print " E = " (read-memory-object mem e))
   ;(print " C = " (read-memory-object mem c))
   ;(print " D = " (read-memory-object mem d))
   (let ((mem (do-gc mem s e c d)))
      (if (myy-null? d)
         (read-memory-object mem (mem-car mem s))
         (lets ((mem s e c d (transition mem s e c d)))
            (vm mem s e c d)))))


;; vm run checks
;(check 42 (run (list (mk-inst op-load-immediate 42 0) (mk-inst op-return 0 0))))
;(check 42 (run (list (mk-inst op-load-immediate 11 0) (mk-inst op-load-immediate 11 0) (mk-inst op-equal 0 1) (mk-inst op-if 0 0) (list (mk-inst op-load-immediate 42 0) (mk-inst op-return 0 0)) (mk-inst op-return 1 0))))
;(check 22 (run (list (mk-inst op-load-immediate 11 0) (mk-inst op-load-immediate 22 0) (mk-inst op-equal 0 1) (mk-inst op-if 0 0) (list (mk-inst op-load-immediate 42 0) (mk-inst op-return 0 0)) (mk-inst op-return 1 0)))) ;;; ;;; SECD Compiler ;;; ;; s = (exp-evaluated-if-known . names-for-it) 

(define (stack-find s exp)
   (let loop ((s s) (p 0))
      (cond
         ((null? s) #false)
         ;((eq? (caar s) exp) p)
         ((has? (car s) exp) p)
         (else (loop (cdr s) (+ p 1))))))

(define (first-unavailable s lst)
   (cond
      ((null? lst) #false)
      ((stack-find s (car lst)) 
         (first-unavailable s (cdr lst)))
      (else (car lst))))

(define (primitive->inst exp)
   (cond
      ((eq? exp 'eq?) op-equal)
      ((eq? exp '+) op-add)
      ((eq? exp '*) op-mul)
      ((eq? exp 'apply) op-apply)
      ((eq? exp 'cons) op-cons)
      ((eq? exp 'car) op-car)
      ((eq? exp 'cdr) op-cdr)
      ((eq? exp '-) op-sub)
      ((eq? exp 'pig) op-pig)
      ((eq? exp 'set-pig) op-set-pig)
      (else #false)))

(define (prim? exp)
   (primitive->inst exp))

(define (primitive-call rator rands s)
   (let ((rands (map (lambda (x) (stack-find s x)) rands)))
      (cond
         ((null? rands)
            (mk-inst-unary (primitive->inst rator) 0))
         ((null? (cdr rands))
            (mk-inst-unary (primitive->inst rator) (car rands)))
         (else
            (mk-inst (primitive->inst rator) (car rands) (cadr rands))))))

(define (lambda? exp)
   (and (list? exp)
        (= (length exp) 3)
        (eq? (car exp) 'lambda)))

(define (if? exp)
   (and (list? exp)
        (= (length exp) 4)
        (eq? (car exp) 'if)))
  
(define (add-name s exp name)
   (cond
      ((null? s) 
         (error "bug: add-name: not in stack: " (str exp)))
      ((has? (car s) exp)
         (cons (append (car s) (list name))
               (cdr s)))
      (else
         (cons (car s)
            (add-name (cdr s) exp name)))))

(define (name-values s exps names)
   (cond
      ((null? exps)
         (if (null? names)
            s
            (error "operator lambda wrong number or values, missing " names)))
      ((null? names)
         (error "operator lambda wrong number of values, extra args " exps))
      (else
         (name-values (add-name s (car exps) (car names)) (cdr exps) (cdr names)))))

(define nothing "[nothing]")

(define (env-find e exp)
   (let loop ((e e) (depth 0))
      (cond
         ((null? e)
            #false)
         ((stack-find (car e) exp) =>
            (lambda (pos)
               (cons depth pos)))
         (else 
            (loop (cdr e) (+ depth 1))))))

(define (quote? x) 
   (and (pair? x) (eq? (car x) 'quote)))

;; compiler exp S E -> C 
(define (compiler exp fail)
   (define (comp exp s e)
      (cond
         ((or (number? exp) (has? '(() #true #false) exp))
            (values 
               (cons (list exp) s)
               (list op-load-value exp)))
         ((quote? exp)
            (values (cons (list exp) s) (list op-load-value (cadr exp))))
         ((stack-find s exp) =>
            (lambda (pos)
               (values
                  (cons (list exp) s)
                  (list (mk-inst-unary op-load-pos pos)))))
         ((and (symbol? exp) (env-find e exp)) =>
            (lambda (place)
               (values
                  (cons (list exp) s)
                  (list (mk-inst op-load-env (car place) (cdr place))))))
         ((symbol? exp)
            (fail "myy unbound: " exp))
         ((lambda? exp)
            (lets
               ((formals (cadr exp))
                (body (caddr exp))
                (sp code (comp body (map (lambda (x) (list nothing x)) formals) (cons s e))))
               (values
                  (cons (list exp) s)
                  (list op-close code))))
         ((list? exp)
            (let ((com (first-unavailable s (if (or (prim? (car exp)) (lambda? (car exp))) (cdr exp) exp))))
               (cond
                  ((if? exp)
                     (lets
                        ((s code (comp (cadr exp)  s e))
                         (sp then (comp (caddr exp) s e))
                         (sp else (comp (cadddr exp) s e)))
                        (values sp
                           (append code
                              (ilist 
                                 (mk-inst op-if (stack-find s (cadr exp)) 0)
                                 then else)))))
                  (com
                     (lets
                        ((s code (comp com s e))
                         (s rest (comp exp s e)))
                        (values s (append code rest))))
                  ((eq? (car exp) 'apply)
                     (values s
                        (list 
                           (mk-inst op-apply (stack-find s (cadr exp))
                                             (stack-find s (caddr exp))))))
                  ((prim? (car exp))
                     (values
                        (cons (list exp) s)
                        (list (primitive-call (car exp) (cdr exp) s))))
                  ((lambda? (car exp))
                     (lets 
                        ((s code 
                           (comp (caddr (car exp))
                              (name-values s (cdr exp) (cadr (car exp))) e)))
                        (values 
                           (cons (append (car s) (list exp)) (cdr s))
                           code)))
                  (else
                     (values 
                        (cons (list exp) s)
                        (list 
                           (mk-inst op-call (stack-find s (car exp)) (length (cdr exp)))
                           (map (lambda (x) (stack-find s x)) (cdr exp))))))))
         (else
            (fail "compiler: what is " exp))))
   (comp exp null null))

(define (compile exp env)
   (lets/cc ret
      ((fail (lambda args (print (apply str (cons ";; " args))) (ret #false)))
       (env-names (car env))
       (env-values (cdr env))
       (exp (list 'apply (list 'lambda env-names exp) (list 'quote env-values))))
      ;(print "Compiling " exp)
      (lets ((s c (compiler exp fail)))
         (append c (list (mk-inst op-return 0 0))))))


;; hosted compile and run tests

;(check 42 (run (compile 42)))
;(check #false (run (compile '(eq? 11 22))))
;(check #true (run (compile '(eq? 11 11))))
;(check 42 (run (compile '((lambda (x) x) 42))))
;(check #false (run (compile '((lambda (x) ((lambda (y) (eq? x y)) 42)) 101))))
;(check #true (run (compile '((lambda (x) ((lambda (y) (eq? x y)) 42)) 42))))
;(check #true (run (compile '(eq? (eq? 1 1) (eq? 2 2)))))
;(check #true (run (compile '((lambda (x) (eq? x x)) 42))))
;(check #true (run (compile '((lambda (x y) (eq? x y)) 42 42))))
;(check #false (run (compile '((lambda (x y) (eq? x y)) 42 43))))
;(check #true (run (compile '((lambda (x) (eq? 9 (+ 3 x))) 6))))
;(check #true (run (compile '(eq? (- 100 (+ 11 22)) (- (- 100 11) 22)))))
;(check 111 (run (compile '((lambda (op) (op 111)) (lambda (x) x)))))
;(check #true (run (compile '(eq? 42 ((lambda (x) x) 42)))))
;(check #true (run (compile '(eq? ((lambda (x) 42) 43) ((lambda (x) x) 42)))))
;(check 1111 (run (compile '((lambda (x) (x 1111)) ((lambda (x) (lambda (y) y)) 2222)))))
;(check 2222 (run (compile '((lambda (x) (x 1111)) ((lambda (x) (lambda (y) x)) 2222)))))
;(check 12 (run (compile '(if (eq? 1 2) 11 12))))

'(check 44 (run (compile '
   ((lambda (dup self swap twice)
      ((lambda (quad)
         (self (swap (self 11) (self quad))))
       (lambda (x) (twice dup x))))
    (lambda (x) (+ x x))
    (lambda (x) x)
    (lambda (a b) (b a))
    (lambda (op x) (op (op x)))))))

'(check 4 (run (compile 
   '((lambda (pred)
         ((lambda (walk)
            ((lambda (work)
               ((lambda (a b c d)
                  (+ (+ a b) (+ c d)))
                 (work) (work) (work) (work)))
               (lambda () (walk walk 10))))
            (lambda (self x)
               (if (eq? x 1)
                  x
                  (self self (pred x))))))
       (lambda (x) (- x 1))))))


;;;
;;; C generation
;;;

(define rt-pre "#include <stdio.h>
#include <stdlib.h>

void *calloc(size_t nmemb, size_t size);
void exit(int status); ")

(define rt-post "
#define CELLS 10000
#define allocp(val) (0==(((int)val)&2))
#define immediatep(val) (2==(((int)val)&2))
#define imm(type,payload) (2 | (type << 3) | (payload << 8))
#define fixval(n) (((int) n) >> 3)
#define fixnum(n) ((((int) n) << 3) | 6)
#define fixnump(n) ((((int) n) & 7) == 6)
#define MNULL imm(0, 0)
#define MTRUE imm(1, 1)
#define MFALSE imm(1, 0)
#define aof(inst) ((inst >> 6) & 255)
#define bof(inst) (inst >> 14)
#define car(ptr) *((int *) (((int) ptr) & ~4))
#define cdr(ptr) *((int *) (4 | ((int) ptr)))
#define cons(a, b) (pair((int) a, (int) b)|4)
#define setmark(ptr) (ptr | 1)
#define unmark(ptr) (ptr ^ 1)
#define markp(ptr) (ptr & 1)

int S, E, C, D, T;
int *start, *fp, *end;
void mark(int this) {
   int foo;
   int parent = MNULL;
   process: 
      if(immediatep(this)) goto backtrack;
      foo = car(this);
      if (markp(foo)) goto backtrack;
      car(this) = setmark(parent);
      parent = (this|2);
      this = foo;
      goto process;
   backtrack:   
      if (parent == MNULL)
         return;
      if (parent & 2) {
         parent = parent^2;
         foo = cdr(parent);
         cdr(parent) = unmark(car(parent));
         car(parent) = setmark(this);
         this = foo;
         goto process;
      }
      foo = cdr(parent);
      cdr(parent) = this;
      this = parent;
      parent = foo;
      goto backtrack;
}

int sweep() {
   int *pos = end - 2;
   fp = (int *) MNULL;
   int nfree = 0;
   while(start < pos) {
      int val = *pos;
      if (markp(val)) {
         *pos = unmark(val);
      } else {
         pos[1] = (int) fp;
         fp = pos;
         nfree++;
      }
      pos -= 2;
   }
   return nfree;
}

void gc(int a, int b) {
   int n;
   mark(S); mark(E); mark(C); mark(D); 
   mark(T); mark(a); mark(b);
   n = sweep();
   if (n < 100) {
      printf(\"Heap is too full\\n\");
      exit(1);
   }
}

int pair(int a, int b) {
   int *this;
   if ((int)fp == MNULL)
      gc(a, b);
   this = fp;
   fp = (int *) cdr(fp);
   this[0] = a;
   this[1] = b;
   return (int) this;
}

int lref(int lst, int pos) {
   while(pos--)
      lst = cdr(lst);
   return car(lst);
}

int llen(int ptr) {
   int n = 0;
   while(ptr != MNULL) {
      n++;
      ptr = cdr(ptr);
   }
   return n;
}

void list_args(int old, int ptr) {
   if (ptr == MNULL) {
      S = MNULL;
   } else {
      list_args(old, cdr(ptr));
      S = cons(lref(old, fixval(car(ptr))), S);
   }
}

int run() {
   while (C != MNULL) {
      int inst = fixval(car(C));
      C = cdr(C);
      switch(inst&63) {
         case 11: /* op-load-value */
            S = cons(car(C), S);
            C = cdr(C);
            break;
         case 12: /* op-equal */
            S = cons(((lref(S, aof(inst)) == lref(S, bof(inst))) ? MTRUE : MFALSE), S);
            break;
         case 13: { /* op-if */
            C = ((lref(S, aof(inst)) == MFALSE) ? cdr(C) : car(C));
            break; }
         case 3: 
            S = cons(lref(S, aof(inst)), S);
            break;
         case 8:
            T = lref(S, aof(inst));
            S = cons(car(T), S);
            break;
         case 9:
            T = lref(S, aof(inst));
            S = cons(cdr(T), S);
            break;
         case 7:
            S = cons(cons(lref(S, aof(inst)), lref(S, bof(inst))), S);
            break;
         case 5: 
            S = cons(lref(lref(E, aof(inst)), bof(inst)), S);
            break;
         case 4: { /* op-return */
            int rval, st;
            secd_return:
            rval = car(S);
            st = car(D);
            D = cdr(D);
            S = car(st); st = cdr(st);
            E = car(st); st = cdr(st);
            C = car(st);
            S = cons(rval, S);
            if ((int) D == MNULL) {
               return fixval(car(S));
            }
            break; }
         case 2: { /* close */
            T = cons(S, E);
            T = pair(car(C), T); 
            C = cdr(C);
            S = cons(T, S);
            break; }
         case 6: { /* call */
            T = cons(cdr(C), MNULL);
            T = cons(E, T);
            T = cons(S, T);
            D = cons(T, D);
            T = lref(S, aof(inst)); /* operator */
            list_args(S, car(C)); /* construct to S*/
            E = cdr(T);
            C = car(T);
            break; }
         case 1: /* op-apply */
            T = cons(cdr(C), MNULL);
            T = cons(E, T);
            T = cons(S, T);
            D = cons(T, D);
            T = lref(S, aof(inst)); /* operator */
            S = lref(S, bof(inst)); /* args */
            E = cdr(T);
            C = car(T);
            break;
         case 14: { /* add */
            int a = fixval(lref(S, aof(inst)));
            int b = fixval(lref(S, bof(inst)));
            S = cons(fixnum(a + b), S);
            break; }
         case 15: { /* sub */
            int a = fixval(lref(S, aof(inst)));
            int b = fixval(lref(S, bof(inst)));
            S = cons(fixnum(a - b), S);
            break; }
         case 17: /* pig */
            S = cons(fixnum(lref(S, aof(inst)) & 7), S);
            break;
         case 18: /* set-pig */
            S = cons(fixnum(lref(S, aof(inst)) & ~7) | (fixval(lref(S, bof(inst))) & 7), S);
            break;
         case 19: { /* mul */
            int a = fixval(lref(S, aof(inst)));
            int b = fixval(lref(S, bof(inst)));
            S = cons(fixnum(a * b), S);
            break; }
         default:
            printf(\"vm: what inst is %d\\n\", inst&63);
            return 127;
      }
   }
   goto secd_return;
}

void load_heap() {
   int pos = 0;
   int *hp = start;
   int val;
   while (1) {
      int val = heap[pos];
      if (val == 3) { break; }
      *hp = (immediatep(val) ? val : ((int) start) + val);
      hp++;
      pos++;     
   }
   hp -= 2;
   S = MNULL;
   E = MNULL;
   C = hp[0];
   D = hp[1];
   T = MNULL;
   fp = hp;
   while(hp < end) { /* first sweep */
      cdr(hp) = ((int)hp)+8;
      hp += 2;
   }
   hp -= 2;
   cdr(hp) = MNULL;
}

int main(int nargs, char **args) {
   int rval;
   start  = (int *) calloc((sizeof (int *)), 2 * CELLS);
   end = start + 2*CELLS;
   load_heap();   
   rval = run();
   return rval;
}

")

(define (heap-array port mem)
   (display-to port "int heap[] = {")
   (for-each
      (lambda (pos)
         (print-to port (str (read mem pos) ", ")))
      (iota 0 4 (getf mem 'free)))
   (display-to port "3};"))

(define (dump-heap mem entry-pair)
   ;(print "Dumping " (read-memory-object mem entry-pair))
   (print ";; writing out.c")
   (lets
       ((port (open-output-file "out.c")))
      (print-to port rt-pre)
      (heap-array port mem)
      (print-to port rt-post)
      (close-port port)))

(define (run c)
   (if c
      (lets ((mem cp (create-memory c 4096))
             (mem ret (burn mem `((,null ,null ,null))))
             (mem entry (mem-cons mem cp ret)))
         (dump-heap mem entry)
         ;(print ";; Running SECD with C = " c)
         (print ";; Running virtual SECD")
         (vm mem myy-null myy-null cp ret))))

(import (owl terminal)
        (owl sexp)
        (owl parse))

(define (myy-eval exp env)
   (sexp-case exp
      ((define ? ?) (name value)
         (let ((res (run (compile value env))))
            (print ";; " name " → " res)
            (values 'ok
               (cons
                  (cons name (car env))
                  (cons res (cdr env))))))
      (else
         (values
            (run (compile exp env))
            env))))

(define (myy-repl env)
   (let loop ((es (lambda () (fd->exp-stream stdin #f sexp-parser #f #f))) (env env))
      (display "* ")
      (lets ((exp es (uncons es 'quit)))
         (if (eq? exp 'quit)
            (begin
               (print "Bye bye o/~")
               0)
            (lets ((res env (myy-eval exp env)))
               (print res)
               (loop es env))))))

(define empty-env (cons null null))

(define standard-library   
   (fold
      (lambda (env exp) (lets ((res env (myy-eval exp env))) env))
      empty-env 
      '(
         (define pair? (lambda (x) (eq? 4 (pig x))))
         
         (define number? (lambda (x) (eq? 6 (pig x))))
         
         (define function? (lambda (x) (if (eq? 0 (pig x)) 
            (pair? (car (set-pig x 4))) #false)))
         
         (define fakt ((lambda (f) (lambda (s) (f f s))) 
             (lambda (f s) (if (eq? s 0) 1 (* s (f f (- s 1)))))))

         (define range ((lambda (r) (lambda (f t) (r f t r))) 
              (lambda (f t r) (if (eq? f t) '() (cons f (r (+ f 1) t r)) )))))))


(print "MYY LISP")
(myy-repl standard-library)



