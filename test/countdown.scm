;;

(lambda (mcp cont vminfo)
  ((lambda (rec)
     (rec a b 42 4095 rec))
     (lambda (mcp cont a b self)
         (if (eq? a b)
            (cont mcp a)
            ((lambda (one)
               ((lambda (pred)
                  (self mcp cont a pred self))
                  (- b one)))
              1)))))

