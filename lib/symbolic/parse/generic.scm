(define (parse expr . notation-opt)
  (let* ((notation (if (null? notation-opt) 'infix (car notation-opt)))
         (parser-proc (get 'parser notation)))
    (if parser-proc
      (parser-proc expr)
      (error "Unknown parsing notation: " notation))))

(define (unparse expr . notation-opt)
  (let* ((notation (if (null? notation-opt) 'infix (car notation-opt)))
         (unparser-proc (get 'unparser notation)))
    (if unparser-proc
      (unparser-proc expr)
      (error "Unknown unparsing notation: " notation))))
