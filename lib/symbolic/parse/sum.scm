(define (install-sum-package)
  (put 'precedence '+ 10)
  (put 'associativity '+ 'right)
  'ok)
