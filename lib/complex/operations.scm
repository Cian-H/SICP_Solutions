(define (complex-add z1 z2)
  (complex-from-real-imag
    (+ (complex-real-part z1) (complex-real-part z2))
    (+ (complex-imag-part z1) (complex-imag-part z2))))

(define (complex-sub z1 z2)
  (complex-from-real-imag
    (- (complex-real-part z1) (complex-real-part z2))
    (- (complex-imag-part z1) (complex-imag-part z2))))

(define (complex-mul z1 z2)
  (complex-from-mag-ang
    (* (complex-magnitude z1) (complex-magnitude z2))
    (+ (complex-angle z1) (complex-angle z2))))

(define (complex-div z1 z2)
  (complex-from-mag-ang
    (/ (complex-magnitude z1) (complex-magnitude z2))
    (- (complex-angle z1) (complex-angle z2))))
