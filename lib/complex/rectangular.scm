(define (complex-install-rectangular-package)
  (register-type 'rectangular)

  ;;; Define type methods
  (define (from-real-imag x y) (cons x y))
  (define (from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))

  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (add z1 z2)
    (from-real-imag
      (apply-generic 'add (real-part z1) (real-part z2))
      (apply-generic 'add (imag-part z1) (imag-part z2))))

  (define (sub z1 z2)
    (from-real-imag
      (apply-generic 'sub (real-part z1) (real-part z2))
      (apply-generic 'sub (imag-part z1) (imag-part z2))))

  (define (mul z1 z2)
    (let ((r1 (real-part z1)) (i1 (imag-part z1))
          (r2 (real-part z2))
          (i2 (imag-part z2)))
      (from-real-imag
        (apply-generic 'sub (apply-generic 'mul r1 r2) (apply-generic 'mul i1 i2))
        (apply-generic 'add (apply-generic 'mul r1 i2) (apply-generic 'mul i1 r2)))))

  (define (div z1 z2)
    (let* ((r1 (real-part z1)) (i1 (imag-part z1))
           (r2 (real-part z2))
           (i2 (imag-part z2))
           (denom (apply-generic 'add
                   (apply-generic 'mul r2 r2)
                   (apply-generic 'mul i2 i2))))
      (from-real-imag
        (apply-generic 'div
          (apply-generic 'add (apply-generic 'mul r1 r2) (apply-generic 'mul i1 i2))
          denom)
        (apply-generic 'div
          (apply-generic 'sub (apply-generic 'mul i1 r2) (apply-generic 'mul r1 i2))
          denom))))

  (define (complex-zero? z)
    (and
      (zero? (real-part z))
      (zero? (imag-part z))))

  (define (equal? z1 z2)
    (and
      (= (real-part z1) (real-part z2))
      (= (imag-part z1) (imag-part z2))))

  (define (wrap x) (type-wrap 'rectangular x))

  ;;; Register type methods
  (put 'from-real-imag 'rectangular (lambda (x y) (wrap (from-real-imag x y))))
  (put 'from-mag-ang 'rectangular (lambda (x y) (wrap (from-mag-ang x y))))
  (put 'real-part 'rectangular real-part)
  (put 'imag-part 'rectangular imag-part)
  (put 'magnitude 'rectangular magnitude)
  (put 'angle 'rectangular angle)
  (put 'add 'rectangular 'rectangular (lambda (z1 z2) (wrap (add z1 z2))))
  (put 'sub 'rectangular 'rectangular (lambda (z1 z2) (wrap (sub z1 z2))))
  (put 'mul 'rectangular 'rectangular (lambda (z1 z2) (wrap (mul z1 z2))))
  (put 'div 'rectangular 'rectangular (lambda (z1 z2) (wrap (div z1 z2))))
  (put 'zero? 'rectangular complex-zero?)
  (put 'equal? 'rectangular 'rectangular equal?)

  ;;; Add type coercions
  (define (rectangular->polar z)
    (let ((raw-z (type-unwrap z)))
      ((get 'from-mag-ang 'polar) (magnitude raw-z) (angle raw-z))))

  (put-coercion 'rectangular 'polar rectangular->polar)

  'ok)
