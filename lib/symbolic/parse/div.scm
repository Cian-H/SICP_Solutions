(define (install-div-package)
  (put 'precedence '/ 20)
  (put 'associativity '/ 'left)
  'ok)
