(complex-install-rectangular-package)
(complex-install-polar-package)

(define (install-complex-package)
  (register-type 'complex)

  ;;; Define type methods
  (define (from-real-imag x y)
    ((get 'from-real-imag 'rectangular) x y))
  (define (from-mag-ang r a)
    ((get 'from-mag-ang 'polar) r a))

  (define (add z1 z2)
    (from-real-imag
      (+ (complex-real-part z1) (complex-real-part z2))
      (+ (complex-imag-part z1) (complex-imag-part z2))))

  (define (sub z1 z2)
    (from-real-imag
      (- (complex-real-part z1) (complex-real-part z2))
      (- (complex-imag-part z1) (complex-imag-part z2))))

  (define (mul z1 z2)
    (from-mag-ang
      (* (complex-magnitude z1) (complex-magnitude z2))
      (+ (complex-angle z1) (complex-angle z2))))

  (define (div z1 z2)
    (from-mag-ang
      (/ (complex-magnitude z1) (complex-magnitude z2))
      (- (complex-angle z1) (complex-angle z2))))

  (define (complex-zero? z) (apply-generic 'zero? z))
  (define (equal? z1 z2) (apply-generic 'equal? z1 z2))

  (define (wrap z) (type-wrap 'complex z))

  ;;; Register type methods
  (put 'add 'complex 'complex
    (lambda (z1 z2) (wrap (add z1 z2))))
  (put 'sub 'complex 'complex
    (lambda (z1 z2) (wrap (sub z1 z2))))
  (put 'mul 'complex 'complex
    (lambda (z1 z2) (wrap (mul z1 z2))))
  (put 'div 'complex 'complex
    (lambda (z1 z2) (wrap (div z1 z2))))
  (put 'from-real-imag 'complex
    (lambda (x y) (wrap (from-real-imag x y))))
  (put 'from-mag-ang 'complex
    (lambda (r a) (wrap (from-mag-ang r a))))
  (put 'to-rectangular 'complex
    (lambda (z) (wrap (apply-generic 'to-rectangular z))))
  (put 'to-polar 'complex
    (lambda (z) (wrap (apply-generic 'to-polar z))))
  (put 'zero? 'complex
    (lambda (z) (complex-zero? z)))
  (put 'equal? 'complex 'complex
    (lambda (z1 z2) (equal? z1 z2)))

  ;;; Add type coercions
  (define (number->complex n)
    (complex-from-real-imag (type-unwrap n) 0))

  (put-coercion 'number 'complex number->complex)

  (put-coercion 'polar 'rectangular
    (lambda (z) (apply-generic 'to-rectangular z)))
  (put-coercion 'rectangular 'polar
    (lambda (z) (apply-generic 'to-polar z)))

  'ok)
