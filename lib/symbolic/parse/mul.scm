(define (install-mul-package)
  (put 'precedence '* 20)
  (put 'associativity '* 'right)
  'ok)
