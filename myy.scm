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

(define (make-memory limit)
   (if (even? limit)
      (put #empty 'limit (* limit 8))
      (error "list structured memory needs an even number of words, but got" limit)))

(define (read mem ptr)
   (cond
      ((not (number? ptr))
         (error "Bug: trying to read " ptr))
      ((not (eq? (band ptr #b11) 0))
         (error "Invalid pointer alignment in read: " ptr))
      ((or (< ptr 0) (> ptr (getf mem 'limit)))
         (error "Segmentation violation: reading " ptr))
      (else
         (let ((val (getf mem (>> ptr 2))))
            (if val val
               (error "Invalid memory access: " ptr))))))

(define (write mem ptr val)
   (cond
      ((not (number? ptr))
         (error "Bug: trying to write " ptr))
      ((not (eq? (band ptr #b11) 0))
         (error "Invalid pointer alignment in write: " ptr))
      ((or (< ptr 0) (> ptr (getf mem 'limit)))
         (error "Segmentation violation: writing " ptr))
      (else
         (put mem ptr val))))

;; memory tests
(check 42 
   (-> (make-memory 10)
       (write (<< 0 2) 42)
       (read (<< 0 2))))

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

;; Allocated case
;; 
;; ,------------------------------+----->  allocated object pointer to an *even* word
;; |                              |
;;(s)______ ________ ________ ttttFiGG
;; |                        | |  |||||
;; `------------------------+ |  |||`+--> GC bits
;;                          | '--+|`----> immediateness
;;                          |    |'-----> fixnum bit  
;;                          |    `------> immediate object type (16 options)
;;                          `-----------> immediate payload (typically signed)
;; Immediate case


;;;
;;; SECD Compiler
;;;



;;;
;;; SECD VM
;;;

(define (execute s e c d op)
   (cond
      (else 42)))
        

(define (transition s e c d)
   (if (null? c)
      (lets ((state d d)
             (s1 e1 c1 d1 state))
         (values (cons (car s) s1) e1 c1 d1))
      (execute s e (cdr c) d (car c))))

         
(print "foo")

