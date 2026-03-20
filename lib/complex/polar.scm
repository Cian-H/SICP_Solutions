(define (complex-install-polar-package)
  (register-type 'polar)

  ;;; Define type methods
  (define (from-mag-ang r a) (cons r a))
  (define (from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))

  (define (magnitude z) (car z))
  (define (angle z) (cdr z))

  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))

  (define (add z1 z2)
    (from-real-imag
      (apply-generic 'add (real-part z1) (real-part z2))
      (apply-generic 'add (imag-part z1) (imag-part z2))))

  (define (sub z1 z2)
    (from-real-imag
      (apply-generic 'sub (real-part z1) (real-part z2))
      (apply-generic 'sub (imag-part z1) (imag-part z2))))

  (define (mul z1 z2)
    (from-mag-ang
      (apply-generic 'mul (magnitude z1) (magnitude z2))
      (apply-generic 'add (angle z1) (angle z2))))

  (define (div z1 z2)
    (from-mag-ang
      (apply-generic 'div (magnitude z1) (magnitude z2))
      (apply-generic 'sub (angle z1) (angle z2))))

  (define (complex-zero? z)
    (zero? (magnitude z)))

  (define (equal? z1 z2)
    (and
      (= (magnitude z1) (magnitude z2))
      (= (angle z1) (angle z2))))

  (define (wrap x) (type-wrap 'polar x))

  ;;; Register type methods
  (put 'from-mag-ang 'polar (lambda (x y) (wrap (from-mag-ang x y))))
  (put 'from-real-imag 'polar (lambda (x y) (wrap (from-real-imag x y))))
  (put 'real-part 'polar real-part)
  (put 'imag-part 'polar imag-part)
  (put 'magnitude 'polar magnitude)
  (put 'angle 'polar angle)
  (put 'add 'polar 'polar (lambda (z1 z2) (wrap (add z1 z2))))
  (put 'sub 'polar 'polar (lambda (z1 z2) (wrap (sub z1 z2))))
  (put 'mul 'polar 'polar (lambda (z1 z2) (wrap (mul z1 z2))))
  (put 'div 'polar 'polar (lambda (z1 z2) (wrap (div z1 z2))))
  (put 'zero? 'polar complex-zero?)
  (put 'equal? 'polar 'polar equal?)

  ;;; Add type coercions
  (define (polar->rectangular z)
    (let ((raw-z (type-unwrap z)))
      ((get 'from-real-imag 'rectangular) (real-part raw-z) (imag-part raw-z))))

  (put-coercion 'polar 'rectangular polar->rectangular)

  'ok)
