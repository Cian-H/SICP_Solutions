(define (install-symbolic/parse/exp-package)
  (put 'precedence '** 30)
  (put 'associativity '** 'right)
  'ok)
