(define (install-symbolic/parse/infix/sum-package)
  (put 'precedence '+ 10)
  (put 'associativity '+ 'right)
  'ok)
