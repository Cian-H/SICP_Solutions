(define (rational-equal? x y)
  ((get 'equal? 'rational 'rational) x y))
