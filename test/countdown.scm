;;

(lambda (args)
   (let* 
      ((rec 
         (lambda (a b rec)
            (if (eq? a b)
               a
               (rec a (- b 1) rec)))))
      (rec 42 4095 rec)))

