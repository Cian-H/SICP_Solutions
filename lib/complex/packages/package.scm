(complex-install-rectangular-package)
(complex-install-polar-package)

(define (install-complex-package)
  (define (from-real-imag x y)
    ((get 'from-real-imag 'rectangular) x y))
  (define (from-mag-ang r a)
    ((get 'from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (from-real-imag
      (+ (complex-real-part z1) (complex-real-part z2))
      (+ (complex-imag-part z1) (complex-imag-part z2))))

  (define (sub-complex z1 z2)
    (from-real-imag
      (- (complex-real-part z1) (complex-real-part z2))
      (- (complex-imag-part z1) (complex-imag-part z2))))

  (define (mul-complex z1 z2)
    (from-mag-ang
      (* (complex-magnitude z1) (complex-magnitude z2))
      (+ (complex-angle z1) (complex-angle z2))))

  (define (div-complex z1 z2)
    (from-mag-ang
      (/ (complex-magnitude z1) (complex-magnitude z2))
      (- (complex-angle z1) (complex-angle z2))))

  (define (wrap z) (type-wrap 'complex z))
  (put 'add 'complex 'complex
    (lambda (z1 z2) (wrap (add-complex z1 z2))))
  (put 'sub 'complex 'complex
    (lambda (z1 z2) (wrap (sub-complex z1 z2))))
  (put 'mul 'complex 'complex
    (lambda (z1 z2) (wrap (mul-complex z1 z2))))
  (put 'div 'complex 'complex
    (lambda (z1 z2) (wrap (div-complex z1 z2))))
  (put 'from-real-imag 'complex
    (lambda (x y) (wrap (from-real-imag x y))))
  (put 'from-mag-ang 'complex
    (lambda (r a) (wrap (from-mag-ang r a))))
  'ok)
