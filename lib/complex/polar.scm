(define (complex-install-polar-package)
  (register-type 'polar)

  (define (from-mag-ang r a) (cons r a))
  (define (from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))

  (define (magnitude z) (car z))
  (define (angle z) (cdr z))

  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))

  (define (to-rectangular z)
    ((get 'from-real-imag 'rectangular) (real-part z) (imag-part z)))

  (define (complex-zero? z)
    (zero? (magnitude z)))

  (define (equal? z1 z2)
    (and
      (= (magnitude z1) (magnitude z2))
      (= (angle z1) (angle z2))))

  (define (wrap x) (type-wrap 'polar x))

  (put 'from-mag-ang 'polar (lambda (x y) (wrap (from-mag-ang x y))))
  (put 'from-real-imag 'polar (lambda (x y) (wrap (from-real-imag x y))))
  (put 'real-part 'polar real-part)
  (put 'imag-part 'polar imag-part)
  (put 'magnitude 'polar magnitude)
  (put 'angle 'polar angle)
  (put 'to-rectangular 'polar to-rectangular)
  (put 'to-polar 'polar wrap)
  (put 'zero? 'polar complex-zero?)
  (put 'equal? 'polar 'polar equal?)
  'ok)
