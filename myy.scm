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
      ((sexp-cases var (pat formals . then) . others)
         (let ((m (match var (quasiquote pat))))
            (if m
               (apply (lambda formals . then) m)
               (sexp-cases var . others))))))

(define-syntax sexp-case
   (syntax-rules ()
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

;;;
;;; Virtual memory
;;;

;; Allocated case
;; 
;; ,------------------------------+----->  allocated object pointer to an *even* word
;; |                              |
;;(s)______ ________ ________ ttttFiGG
;; |                        | |  |||||
;; `------------------------+ |  |||`+--> GC bits
;;                          | '--+|`----> immediateness
;;                          |    |'-----> fixnum bit, leaves sign + 27 bits for fixnums
;;                          |    `------> immediate object type (16 options)
;;                          `-----------> immediate payload (typically signed)
;; Immediate case

(define (ptr n) (<< n 2))

(define (make-memory limit)
   (if (even? limit)
      (-> #empty
         (put 'end (* limit 8))
         (put 'free (ptr 0)))
      (error "list structured memory needs an even number of words, but got" limit)))

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
   (let ((val (getf mem ptr)))
      (if val val
         (error "Invalid memory access: " ptr))))

(define (write mem ptr val)
   (check-pointer mem ptr)
   (put mem ptr val))

(define (mem-cons mem a b)
   (let ((free (getf mem 'free)))
      (values
         (-> mem
            (put 'free (+ free 8))
            (write free a)
            (write (+ free 4) b))
         free)))

(define (mem-car mem ptr) (read mem ptr))
(define (mem-cdr mem ptr) (read mem (+ ptr 4)))
   
;; memory tests
(check 42 (-> (make-memory 10) (write (ptr 0) 42) (read (ptr 0))))
(check 42 (-> (make-memory 10) (write (ptr 5) 42) (read (ptr 5))))
(check 42 (-> (make-memory 10) (write (ptr 3) 24) (write (ptr 3) 42) (read (ptr 3))))
(check 20
   (lets ((m (make-memory 10))
          (m ptr (mem-cons m 22 2)))
      (- (mem-car m ptr) (mem-cdr m ptr))))
(check 11
   (lets ((m (make-memory 10))
          (m a (mem-cons m 11 22))
          (m b (mem-cons m 33 a)))
      (mem-car m (mem-cdr m b))))
   


;;;
;;; Data Encoding
;;;

(define (imm-payload n) (>> n 3))
(define (imm-fixval n) (>> n 4))
(define (allocated? n) (eq? 0 (band n #b100)))
(define (immediate? n) (not (allocated? n)))
(define (fixnum? n) (eq? #b1100 (band n #b1111)))
(define (mk-fixnum n) (bor (<< n 4) #b1100))
(define (mk-immediate type payload)
   (bor (<< payload 8) (bor (<< type 4) #b0100)))

;; encoding tests
(check #true (immediate? (mk-fixnum 42)))
(check 42 (imm-fixval (mk-fixnum 42)))
(check #true (fixnum? (mk-fixnum 42)))
(check #false (fixnum? (mk-immediate 0 0)))
(check #false (allocated? (mk-immediate 0 0)))
(check #false (allocated? (mk-fixnum 42)))

   
;;;
;;; GC
;;;


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
;;; SECD VM, direct, transitions
;;;

(define op-apply           1)
(define op-close           2)
(define op-load-pos        3)
(define op-return          4)
(define op-load-immediate 10)
(define op-load-value     11)
(define op-equal          12)
(define op-if             13)
(define op-add            14)
(define op-sub            15)

(define (mk-closure code env)
   (list 'closure code env))

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
      
(define* (execute s e c d instruction)
   (lets ((op a b (decode-inst instruction)))
      (case op
         (op-apply
            (lets
               ((rator (list-ref s a))
                (rands (list-ref s b)))
               (vm-apply s e c d rator rands)))
         (op-close 
            (values
               (cons (mk-closure (car c) e) s) e (cdr c) d))
         (op-return 
            (let ((rval (list-ref s a)))
               (apply (lambda (s e c) (values (cons rval s) e c (cdr d))) (car d))))
         (op-load-immediate
            ;; load immediate directly after instruction opcode
            (values (cons (>> instruction 6) s) e c d))
         (op-load-pos
            (values (cons (list-ref s a) s) e c d))
         (op-load-value
            ;; load subsequent (likely allocated) value
            (values (cons (car c) s) e (cdr c) d))
         (op-equal
            (values (cons (eq? (list-ref s a) (list-ref s b)) s) e c d))
         (op-add
            (lets ((fa (list-ref s a))
                   (fb (list-ref s b)))
               (values (cons (+ fa fb) s) e c d)))
         (op-sub
            (lets ((fa (list-ref s a))
                   (fb (list-ref s b)))
               (values (cons (- fa fb) s) e c d)))
         (op-if
            (values s e (if (list-ref s a) (car c) (cdr c)) d))
         (else
            (error "Unknown instruction: " op)))))

(define* (transition s e c d)
   (if (null? c)
      (lets ((state d d)
             (s1 e1 c1 d1 state))
         (values (cons (car s) s1) e1 c1 d1))
      (execute s e (cdr c) d (car c))))

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

 
(check-transition 'S 'E (list (bor (<< 42 6) op-load-immediate)) 'D => '(42 . S) 'E null 'D)
(check-transition 'S 'E (list op-load-value 42) 'D => '(42 . S) 'E () 'D)
(check-transition 'S 'E (list op-close 'CODE) 'D => '((closure CODE E) . S) 'E () 'D)
(check-transition '(s0 s1 s2) 'E (list (mk-inst op-load-pos 2 0)) 'D => '(s2 s0 s1 s2) 'E () 'D)
(check-transition '((closure CC CE) (A0 A1) . S) 'E (cons (mk-inst op-apply 0 1) 'C) 'D =>
                   '(A0 A1) 'CE 'CC '((((closure CC CE) (A0 A1) . S) E C) . D)) 
(check-transition '(a x a b) 'E (list (mk-inst op-equal 0 2)) 'D => '(#true a x a b) 'E null 'D)
(check-transition '(a x a b) 'E (list (mk-inst op-equal 0 1)) 'D => '(#false a x a b) 'E null 'D)
(check-transition '(x #false) 'E (ilist (mk-inst op-if 0 0) 'THEN 'ELSE) 'D => '(x #false) 'E 'THEN 'D)
(check-transition '(x #false) 'E (ilist (mk-inst op-if 1 0) 'THEN 'ELSE) 'D => '(x #false) 'E 'ELSE 'D)
(check-transition '(x x RESULT) 'XE (list (mk-inst op-return 2 0)) '((S E C) . D)  => '(RESULT . S) 'E 'C 'D)


;;;
;;; SECD VM
;;;

(define* (vm s e c d)
   (if (null? d)
      (car s)
      (lets ((s e c d (transition s e c d)))
         (vm s e c d))))

(define* (run c)
   (vm null null c '((null null (list (mk-inst op-return 0 0))))))

;; vm run checks
(check 42
   (run 
      (list 
         (mk-inst op-load-immediate 42 0)
         (mk-inst op-return 0 0))))
(check 42
   (run
      (list
         (mk-inst op-load-immediate 11 0)
         (mk-inst op-load-immediate 11 0)
         (mk-inst op-equal 0 1)
         (mk-inst op-if 0 0) 
         (list 
            (mk-inst op-load-immediate 42 0)
            (mk-inst op-return 0 0))
         (mk-inst op-return 1 0))))
 (check 22
   (run
      (list
         (mk-inst op-load-immediate 11 0)
         (mk-inst op-load-immediate 22 0)
         (mk-inst op-equal 0 1)
         (mk-inst op-if 0 0) 
         (list 
            (mk-inst op-load-immediate 42 0)
            (mk-inst op-return 0 0))
         (mk-inst op-return 1 0))))


;;;
;;; SECD Compiler
;;;

;; value that need not be evaluated
(define (simple? val)
   (or (symbol? val)
       (number? val)
       (and (pair? val) (eq? (car val) 'quote))))

(define (complex? x) 
   (not (simple? x)))

;; s = (exp-evaluated-if-known . names-for-it)

(define (first-nonsimple lst)
   (cond
      ((null? lst) #false)
      ((simple? (car lst)) (first-nonsimple (cdr lst)))
      (else (car lst))))

(define* (stack-find s exp)
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

(define (prim? exp)
   (has? '(eq? + - * =) exp))

(define (primitive->inst exp)
   (cond
      ((eq? exp 'eq?) op-equal)
      ((eq? exp '+) op-add)
      ((eq? exp '-) op-sub)
      (else
         (error "primitive->inst: what is " exp))))

(define* (primitive-call rator rands s)
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

(define (add-name s exp name)
   (cond
      ((null? s) 
         (error "bug: add-name: not in stack: " exp))
      ((eq? (caar s) exp)
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

(define nothing "foo")

;; compiler exp S E -> C 
(define* (compiler exp s e)
   (cond
      ((number? exp)
         (values
            (cons (list exp) s)
            (list (mk-inst-unary op-load-immediate exp))))
      ((stack-find s exp) =>
         (lambda (pos)
            (values
               (cons nothing s) ;; don't refer here later to avoid shadowing issues, use the real value
               (list (mk-inst-unary op-load-pos pos)))))
      ((symbol? exp)
         (error "myy unbound: " exp))
      ((list? exp)
         (let ((comp (first-unavailable s (if (or (prim? (car exp)) (lambda? (car exp))) (cdr exp) exp))))
            (cond
               (comp
                  (lets
                     ((s code (compiler comp s e))
                      (s rest (compiler exp s e)))
                     (values s (append code rest))))
               ((prim? (car exp))
                  (values
                     (cons (list exp) s)
                     (list (primitive-call (car exp) (cdr exp) s))))
               ((lambda? (car exp))
                  (compiler (caddr (car exp))
                     (name-values s (cdr exp) (cadr (car exp))) e))
               (else
                  (error "could compile call " (str "call: " exp "\nstack: " s))))))
      (else
         (error "compiler: what is " exp))))

(define* (compile exp)
   (lets ((s c (compiler exp null null)))
      (append c (list (mk-inst op-return 0 0)))))


;; compiler&vm tests

(check 42 (run (compile 42)))
(check #false (run (compile '(eq? 11 22))))
(check #true (run (compile '(eq? 11 11))))
(check 42 (run (compile '((lambda (x) x) 42))))
(check #false (run (compile '((lambda (x) ((lambda (y) (eq? x y)) 42)) 101))))
(check #true (run (compile '((lambda (x) ((lambda (y) (eq? x y)) 42)) 42))))
(check #true (run (compile '(eq? (eq? 1 1) (eq? 2 2)))))
(check #true (run (compile '((lambda (x) (eq? x x)) 42))))
(check #true (run (compile '((lambda (x y) (eq? x y)) 42 42))))
(check #false (run (compile '((lambda (x y) (eq? x y)) 42 43))))
(check #true (run (compile '((lambda (x) (eq? 9 (+ 3 x))) 6))))
(check #true (run (compile '(eq? (- 100 (+ 11 22)) (- (- 100 11) 22)))))


;; ------------------------------------------ 8< ------------------------------------------

;; varying output for $ watch -n 0.2 "ol myy.scm | tail -n 20"
(lets 
   ((t (time-ms))
    (cs '(#\space #\▐  #\▌ #\█))
    (rcs (reverse cs)))
   (print 
      (list->string 
         (ilist #\newline #\space #\space #\░ 
            (reverse
               (cons #\░ 
                  (map (lambda (x) (list-ref cs (band (>> t (<< x 1)) #b11)))
                       (iota 0 1 30))))))))
   

