(define (install-symbolic/parse/infix/mul-package)
  (put 'precedence '* 20)
  (put 'associativity '* 'right)
  'ok)
