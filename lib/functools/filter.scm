(define (filter predicate s)
  (cond ((null? s) nil)
    ((predicate (car s)) (cons (car s) (filter predicate (cdr s))))
    (else (filter predicate (cdr s)))))

(define (unique lst)
  (fold-right
    (lambda (x acc) (cons x (filter (lambda (y) (not (equal? x y))) acc)))
    '()
    lst))
