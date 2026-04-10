(define (install-symbolic/parse/sum-package)
  (put 'precedence '+ 10)
  (put 'associativity '+ 'right)
  'ok)
