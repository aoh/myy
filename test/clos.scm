(lambda (x)
   (let*
      ((k (lambda (x) (lambda (y) x))))
      ((k 12) 13)))
