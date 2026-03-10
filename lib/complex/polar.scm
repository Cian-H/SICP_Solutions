(define (complex-polar-from-mag-ang r a) (attach-tag 'polar (cons r a)))

(define (complex-polar? z)
  (eq? (type-tag z) 'polar))

(define (complex-polar-unwrap z)
  (if (complex-polar? z)
    (contents z)
    (error "`complex-polar-*` procedures only accept polar complex numbers!")))

(define (complex-polar-magnitude z) (car (complex-polar-unwrap z)))
(define (complex-polar-angle z) (cdr (complex-polar-unwrap z)))

(define (complex-polar-real-part z)
  (* (complex-polar-magnitude z) (cos (complex-polar-angle z))))

(define (complex-polar-imag-part z)
  (* (complex-polar-magnitude z) (sin (complex-polar-angle z))))

(define (complex-polar-from-real-imag x y)
  (attach-tag
    'polar
    (cons
      (sqrt (+ (square x) (square y)))
      (atan y x))))
