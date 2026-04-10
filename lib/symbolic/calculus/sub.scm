(define (install-symbolic/calculus/sub-package)
  (define (minuend operands) (car operands))
  (define (subtrahend operands) (cadr operands))

  (define (make-sub x y)
    (cond
      ((and (number? x) (number? y)) (- x y))
      ((eq? x 0) (if (number? y) (- y) (type-wrap '- (list 0 y))))
      ((eq? y 0) x)
      (else (type-wrap '- (list x y)))))

  (define (deriv-sub operands var)
    (make-sub (deriv (minuend operands) var)
      (deriv (subtrahend operands) var)))

  (define (integrate-sub operands var)
    (make-sub (integrate (minuend operands) var)
      (integrate (subtrahend operands) var)))

  (put 'deriv '- deriv-sub)
  (put 'integrate '- integrate-sub)
  (put 'sub '- make-sub)
  'ok)
