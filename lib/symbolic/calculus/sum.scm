(define (install-sum-package)
  (define (addend operands) (car operands))
  (define (augend operands) (cadr operands))

  (define (make-sum x y)
    (cond
      ((and (number? x) (number? y)) (+ x y))
      ((eq? x 0) y)
      ((eq? y 0) x)
      (else (type-wrap '+ (list x y)))))

  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
      (deriv (augend operands) var)))

  (put 'deriv '+ deriv-sum)
  (put 'add '+ make-sum)

  'ok)
