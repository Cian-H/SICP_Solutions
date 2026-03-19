(define (complex-install-rectangular-package)
  (register-type 'rectangular)

  (define (from-real-imag x y) (cons x y))
  (define (from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))

  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (to-polar z)
    ((get 'from-mag-ang 'polar) (magnitude z) (angle z)))

  (define (complex-zero? z)
    (and
      (zero? (real-part z))
      (zero? (imag-part z))))

  (define (equal? z1 z2)
    (and
      (= (real-part z1) (real-part z2))
      (= (imag-part z1) (imag-part z2))))

  (define (wrap x) (type-wrap 'rectangular x))

  (put 'from-real-imag 'rectangular (lambda (x y) (wrap (from-real-imag x y))))
  (put 'from-mag-ang 'rectangular (lambda (x y) (wrap (from-mag-ang x y))))
  (put 'real-part 'rectangular real-part)
  (put 'imag-part 'rectangular imag-part)
  (put 'magnitude 'rectangular magnitude)
  (put 'angle 'rectangular angle)
  (put 'to-rectangular 'rectangular wrap)
  (put 'to-polar 'rectangular to-polar)
  (put 'zero? 'rectangular complex-zero?)
  (put 'equal? 'rectangular 'rectangular equal?)
  'ok)
