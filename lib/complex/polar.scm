(define (complex-install-polar-package)
  (define (from-mag-ang r a) (cons r a))
  (define (from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))

  (define (magnitude z) (car z))
  (define (angle z) (cdr z))

  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))

  (define (wrap x) (type-wrap 'polar x))

  (put 'from-mag-ang '(polar) (lambda (x y) (wrap (from-mag-ang x y))))
  (put 'from-real-imag '(polar) (lambda (x y) (wrap (from-real-imag x y))))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  'ok)
