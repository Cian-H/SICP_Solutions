(define (accumulate op acc s)
  (if (null? s)
    acc
    (op (car s) (accumulate op acc (cdr s)))))
