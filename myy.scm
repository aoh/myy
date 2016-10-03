
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
;;; Virtual memory
;;;


;;;
;;; Data Encoding
;;;

;;;
;;; GC
;;;

;; Allocated case
;; 
;; ,------------------------------+----->  allocated object pointer to an *even* word
;; |                              |
;; sxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx
;; |                        | |  |||||
;; `------------------------+ |  |||`+--> GC bits
;;                          | '--+|`----> immediateness
;;                          |    |'-----> fixnum bit  
;;                          |    `------> immediate object type (16 options)
;;                          `-----------> immediate payload (typically signed)
;;
;; Immediate case


;;;
;;; SECD Compiler
;;;



;;;
;;; SECD VM
;;;

(define (transition s e c d)
   (if (null? c)
      (lets ((state d d)
             (s1 e1 c1 d1 state))
         (values (cons (car s) s1) e1 c1 d1))
      (lets ((op c c))

(print "foo")
