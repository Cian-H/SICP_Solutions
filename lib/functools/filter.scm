(define (filter predicate s)
  (cond ((null? s) nil)
    ((predicate (car s)) (cons (car s) (filter predicate (cdr s))))
    (else (filter predicate (cdr s)))))
