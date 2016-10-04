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


(define (ptr n) (<< n 2))
(define (imm-fixval n) (>> n 3))
(define (allocated? n) (eq? 0 (band n #b10)))
(define (immediate? n) (not (allocated? n)))
(define (fixnum? n) (eq? #b110 (band n #b110)))
(define (mk-fixnum n) (bor (<< n 3) #b110))
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

(define (mem-car mem ptr) (read mem ptr))
(define (mem-cdr mem ptr) (read mem (+ ptr 4)))
(define (mem-car! mem ptr val) (write mem ptr val))
(define (mem-cdr! mem ptr val) (write mem (+ ptr 4) val))
   
(define (set-mark word)
   (if (marked? word)
      (error "trying to remark word " word)
      (bor word 1)))

(define (unset-mark word)
   (if (marked? word)
      (bxor word 1)
      (error "trying to unmark unmarked " word)))

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

(define (mem-cons mem a b)
   (let ((free (getf mem 'free)))
      (if (eq? free myy-null)
         (error "gc needed")
         (values
            (-> mem
               (put 'free (mem-cdr mem free))
               (write free a)
               (write (+ free 4) b))
            free))))

;; memory tests
(check 42 (-> (make-memory 10) (write (ptr 0) 42) (read (ptr 0))))
(check 42 (-> (make-memory 10) (write (ptr 5) 42) (read (ptr 5))))
(check 42 (-> (make-memory 10) (write (ptr 3) 24) (write (ptr 3) 42) (read (ptr 3))))
(check 20 (lets ((m (make-memory 10)) (m ptr (mem-cons m 22 2))) (- (mem-car m ptr) (mem-cdr m ptr))))
(check 11 (lets ((m (make-memory 10)) (m a (mem-cons m 11 22)) (m b (mem-cons m 33 a))) (mem-car m (mem-cdr m b))))
   


  
;;;
;;; GC
;;;

(define (mark mem root)
   (define (process mem ptr parent)
      (if (immediate? ptr)
         (backtrack mem ptr parent)
         (let ((val (mem-car mem ptr)))
            (if (marked? val)
               (backtrack mem ptr parent)
               (let ((mem (mem-car! mem ptr (set-mark parent))))
                  (process mem val (set-mark ptr)))))))
   (define (backtrack mem ptr parent)
      (cond
         ((eq? parent myy-null)
            (values mem ptr))
         ((marked? parent)
            (lets 
               ((parent (unset-mark parent))
                (foo (mem-cdr mem parent))
                (mem (mem-cdr! mem parent (unset-mark (mem-car mem parent))))
                (mem (mem-car! mem parent (set-mark ptr))))
               (process mem foo parent)))
         (else
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
;;; SECD VM, direct, transitions
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
(define op-list1          16)

(define (mk-closure code stack env)
   (list 'closure code (cons stack env)))

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
      
(define (execute s e c d instruction)
   (print "EXEC " instruction)
   (lets ((op a b (decode-inst instruction)))
      (case op
         (op-apply
            (lets
               ((rator (list-ref s a))
                (rands (list-ref s b)))
               (vm-apply s e c d rator rands)))
         (op-close 
            (values
               (cons (mk-closure (car c) s e) s) e (cdr c) d))
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
            (print "if on stack " s)
            (values s e (if (list-ref s a) (car c) (cdr c)) d))
         (op-list1
            (values (cons (list (list-ref s a)) s) e c d))
         (op-load-env
            (values (cons (list-ref (list-ref e a) b) s) e c d))
         (op-call 
            (lets
               ((rator (list-ref s a))
                (arity b)
                (args (map (lambda (x) (list-ref s x)) (car c))))
               (if (closure? rator)
                  (values
                     args
                     (caddr rator)
                     (cadr rator)
                     (cons (list s e (cdr c)) d))
                  (error "bad rator: " rator))))
         (else
            (error "Myy unknown instruction: " op)))))

(define (transition s e c d)
   (if (null? c)
      (lets ((state d d))
         (apply
            (lambda (s1 e1 c1)
               (values (cons (car s) s1) e1 c1 d))
            state))
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
(check-transition 'S 'E (list op-close 'CODE) 'D => '((closure CODE (S . E)) . S) 'E () 'D)
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

(define (vm s e c d)
   (if (null? d)
      (car s)
      (lets ((s e c d (transition s e c d)))
         (vm s e c d))))

(define (run c)
   (vm null null c `((null null ,(list (mk-inst op-return 0 0))))))

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

(define (stack-find s exp)
   ;(for-each (lambda (node) (print "  - '" (car node) "' is known as " (cdr node))) s)
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

;; compiler exp S E -> C 
(define (compiler exp s e)
   (print "================= COMPILER " exp)
   (cond
      ((number? exp)
         (values
            (cons (list exp) s)
            (list (mk-inst-unary op-load-immediate exp))))
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
         (error "myy unbound: " exp))
      ((lambda? exp)
         (lets
            ((formals (cadr exp))
             (body (caddr exp))
             (sp code (compiler body (map (lambda (x) (list nothing x)) formals) (cons s e))))
            (values
               (cons (list exp) s)
               (list op-close code))))
      ((list? exp)
         (let ((comp (first-unavailable s (if (or (prim? (car exp)) (lambda? (car exp))) (cdr exp) exp))))
            ;(print " - first unavailable is " comp)
            (cond
               ((if? exp)
                  (lets
                     ((s code (compiler (cadr exp)  s e))
                      (sp then (compiler (caddr exp) s e))
                      (sp else (compiler (cadddr exp) s e)))
                     (values sp
                        (append code
                           (ilist 
                              (mk-inst op-if (stack-find s (cadr exp)) 0)
                              then else)))))
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
                  (lets 
                     ((s code 
                        (compiler (caddr (car exp))
                           (name-values s (cdr exp) (cadr (car exp))) e)))
                     (values 
                        (cons (append (car s) (list exp)) (cdr s))
                        code)))
               ;((= (length exp) 2)
               ;   (lets
               ;      ((pos (stack-find s (cadr exp)))
               ;       (s (cons (list exp) s))
               ;       (rp (stack-find s (car exp))))
               ;      (values
               ;         s
               ;         (list (mk-inst op-list1 pos 0) (mk-inst op-apply rp 0)))))
               (else
                  (values 
                     (cons (list exp) s)
                     (list 
                        (mk-inst op-call (stack-find s (car exp)) (length (cdr exp)))
                        (map (lambda (x) (stack-find s x)) (cdr exp))))))))
      (else
         (error "compiler: what is " exp))))

(define (compile exp)
   (lets ((s c (compiler exp null null)))
      (append c (list (mk-inst op-return 0 0)))))


;; hosted compile and run tests

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
(check 111 (run (compile '((lambda (op) (op 111)) (lambda (x) x)))))
(check #true (run (compile '(eq? 42 ((lambda (x) x) 42)))))
(check #true (run (compile '(eq? ((lambda (x) 42) 43) ((lambda (x) x) 42)))))
(check 1111 (run (compile '((lambda (x) (x 1111)) ((lambda (x) (lambda (y) y)) 2222)))))
(check 2222 (run (compile '((lambda (x) (x 1111)) ((lambda (x) (lambda (y) x)) 2222)))))
(check 12 (run (compile '(if (eq? 1 2) 11 12))))

(check 44 (run (compile '
   ((lambda (dup self swap twice)
      ((lambda (quad)
         (self (swap (self 11) (self quad))))
       (lambda (x) (twice dup x))))
    (lambda (x) (+ x x))
    (lambda (x) x)
    (lambda (a b) (b a))
    (lambda (op x) (op (op x)))))))

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
   

