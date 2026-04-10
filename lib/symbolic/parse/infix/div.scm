(define (install-symbolic/parse/infix/div-package)
  (put 'precedence '/ 20)
  (put 'associativity '/ 'left)
  'ok)
