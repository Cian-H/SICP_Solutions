(define (install-symbolic/parse/div-package)
  (put 'precedence '/ 20)
  (put 'associativity '/ 'left)
  'ok)
