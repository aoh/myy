;;

(lambda (args)
   (let* 
      ((rec 
         (lambda (p rec)
            (if (eq? (car p) (cdr p))
               (car p)
               (rec (cons (car p) (- (cdr p) 1)) rec)))))
      (rec (cons 42 4095) rec)))

