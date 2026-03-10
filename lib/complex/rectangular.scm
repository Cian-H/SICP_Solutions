(define (complex-rectangular-from-real-imag x y) (attach-tag 'rectangular (cons x y)))

(define (complex-rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (complex-rectangular-unwrap z)
  (if (complex-rectangular? z)
    (contents z)
    (error "`complex-rectangular-*` procedures only accept rectangular complex numbers!")))

(define (complex-rectangular-real-part z) (car (complex-rectangular-unwrap z)))
(define (complex-rectangular-imag-part z) (cdr (complex-rectangular-unwrap z)))

(define (complex-rectangular-magnitude z)
  (sqrt (+ (square (complex-rectangular-real-part z)) (square (complex-rectangular-imag-part z)))))

(define (complex-rectangular-angle z)
  (atan (complex-rectangular-imag-part z) (complex-rectangular-real-part z)))

(define (complex-rectangular-from-mag-ang r a)
  (attach-tag
    'rectangular
    (cons
      (* r (cos a))
      (* r (sin a)))))
