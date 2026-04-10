(define (install-symbolic/parse/infix/exp-package)
  (put 'precedence '** 30)
  (put 'associativity '** 'right)
  'ok)
