(define (complex-from-real-imag x y)
  ((get 'from-real-imag 'complex) x y))

(define (complex-from-mag-ang r a)
  ((get 'from-mag-ang 'complex) r a))
