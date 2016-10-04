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

;; object types in memory
;;  - nonpair, header is alloc -> function [lambda-code . env]


(define (ptr n) (<< n 2))
(define (imm-fixval n) (>> n 3))
(define (allocated? n) (eq? 0 (band n #b10)))
(define (immediate? n) (not (allocated? n)))
(define (fixnum? n) (eq? #b110 (band n #b110)))
(define (mem-pair? n) (eq? #b100 (band n #b110)))
(define (mk-fixnum n) (bor (<< n 3) #b110))
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
         (error "gc needed")
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
;;; Data transfer to virtual memory, simple acyclic version for compiler output
;;;

(define (burn mem obj)
   (cond
      ((number? obj)
         (values mem (mk-fixnum obj)))
      ((pair? obj)
         (lets ((mem hd (burn mem (car obj)))
                (mem tl (burn mem (cdr obj))))
            (mem-cons mem hd tl)))
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
      ((mem-pair? ptr)
         (cons (read-memory-object mem (mem-car mem ptr))
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
   ;(print "SECD VM")
   ;(print "  - s " (read-memory-object mem s))
   ;(print "  - e " (read-memory-object mem e))
   ;(print "  - c " (read-memory-object mem c))
   ;(print "  - d " (read-memory-object mem d))
   (lets ((op a b (decode-inst (fixnum-val instruction))))
      (print "  - exec " op ": " a ", " b)
      (case op
         (op-apply
            (lets
               ((rator (list-ref s a))
                (rands (list-ref s b)))
               (error "unhandled " op)
               (vm-apply s e c d rator rands)))
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
               (print "returning")
               (values mem s e c d)))
         (op-load-immediate
            ;; load immediate directly after instruction opcode
            (lets ((mem s (mem-cons mem (>> instruction 6) s)))
               (print "Loading immediate " (>> instruction 6))
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
         (op-if
            (values mem s e
               (if (eq? (mem-list-ref mem s a) myy-false) (mem-cdr mem c) (mem-car mem c))
               d))
         (op-load-env
            (lets ((val (mem-list-ref mem (mem-list-ref mem e a) b))
                   (mem s (mem-cons mem val s)))
               (values mem s e c d)))
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
         (else
            (error "Myy unknown instruction: " op)))))

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
;;; SECD VM
;;;

(define (vm mem s e c d)
   (print "vm " s ", " e ", " c ", " d)
   (if (myy-null? d)
      (read-memory-object mem (mem-car mem s))
      (lets ((mem s e c d (transition mem s e c d)))
         (vm mem s e c d))))

(define (run c)
   (lets ((mem cp (create-memory c 1024))
          (mem ret (burn mem `((,null ,null ,(list (mk-inst op-return 0 0)))))))
      (vm mem myy-null myy-null cp ret)))

;; vm run checks
;(check 42 (run (list (mk-inst op-load-immediate 42 0) (mk-inst op-return 0 0))))
;(check 42 (run (list (mk-inst op-load-immediate 11 0) (mk-inst op-load-immediate 11 0) (mk-inst op-equal 0 1) (mk-inst op-if 0 0) (list (mk-inst op-load-immediate 42 0) (mk-inst op-return 0 0)) (mk-inst op-return 1 0))))
;(check 22 (run (list (mk-inst op-load-immediate 11 0) (mk-inst op-load-immediate 22 0) (mk-inst op-equal 0 1) (mk-inst op-if 0 0) (list (mk-inst op-load-immediate 42 0) (mk-inst op-return 0 0)) (mk-inst op-return 1 0)))) ;;; ;;; SECD Compiler ;;; ;; s = (exp-evaluated-if-known . names-for-it) 

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
      ;((number? exp)
      ;   (values
      ;      (cons (list exp) s)
      ;      (list (mk-inst-unary op-load-immediate (mk-fixnum exp)))))
      ((number? exp)
         (values 
            (cons (list exp) s)
            (list op-load-value exp)))
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

;; varying output for $ watch -n 0.1 "ol myy.scm | tail -n 20"

(lets 
   ((t (time-ms))
    (cs '(#\space #\▐  #\▌ #\█))
    (rcs (reverse cs))
    (out
       (list->string 
         (ilist #\newline #\space #\space #\░ 
            (reverse
               (cons #\░ 
                  (map (lambda (x) (list-ref cs (band (>> t (<< x 1)) #b11)))
                       (iota 0 1 25))))))))
   (print out))
   
   

