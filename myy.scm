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
;;; Instruction format
;;;

;; 6-bit opcode, 8-bit a, n>=8-bit b
(define (mk-inst opcode a b)
   (bor opcode (bor (<< a 6) (<< b 14))))

(define (decode-inst n)
   (values (band n #b111111)
           (band (>> n 6) #b11111111)
           (>> n 14)))

;; instruction checks
(check (list 11 22 33)
   (lets ((a b c (decode-inst (mk-inst 11 22 33))))
      (list a b c)))

;;;
;;; SECD Compiler
;;;



;;;
;;; SECD VM, direct
;;;

(define op-close           2)
(define op-load-pos        3)
(define op-load-immediate 10)
(define op-load-value     11)

(define (mk-closure code env)
   (list 'closure code env))

(define (execute s e c d instruction)
   (lets ((op a b (decode-inst instruction)))
      (case op
         (op-close 
            (values
               (cons (mk-closure (car c) e) s) e (cdr c) d))
         (op-load-immediate
            ;; load immediate directly after instruction opcode
            (values (cons (>> instruction 6) s) e c d))
         (op-load-pos
            (values (cons (list-ref s a) s) e c d))
         (op-load-value
            ;; load subsequent (likely allocated) value
            (values (cons (car c) s) e (cdr c) d))
         (else
            (error "Unknown instruction: " op)))))

(define (transition s e c d)
   (if (null? c)
      (lets ((state d d)
             (s1 e1 c1 d1 state))
         (values (cons (car s) s1) e1 c1 d1))
      (execute s e (cdr c) d (car c))))


;; VM checks

(define-syntax check-transition
   (syntax-rules (=>)
      ((check-transition s e c d => sp ep cp dp)
         (lets ((sx ex cx dx (transition s e c d))
                (start (list s e c d))
                (desired (list sp ep cp dp))
                (got (list sx ex cx dx)))
            (if (not (equal? desired got))
               (error "The computer says no." (str "SECD " start " => " got " instead of " desired)))))))
 
(check-transition 'S 'E (list (bor (<< 42 6) op-load-immediate)) 'D => '(42 . S) 'E null 'D)
(check-transition 'S 'E (list op-load-value 42) 'D => '(42 . S) 'E () 'D)
(check-transition 'S 'E (list op-close 'CODE) 'D => '((closure CODE E) . S) 'E () 'D)
(check-transition '(s0 s1 s2) 'E (list (mk-inst op-load-pos 2 0)) 'D => '(s2 s0 s1 s2) 'E () 'D)


;; varying output for watch
(lets 
   ((t (>> (time-ms) 9))
    (cs '(#\space #\▐  #\▌ #\█))
    (rcs (reverse cs)))
   (print 
      (list->string 
         (cons #\░ 
            (reverse
               (cons #\░ 
                  (map (lambda (x) (list-ref cs (band (>> t (<< x 1)) #b11)))
                       (iota 0 1 10))))))))
   

