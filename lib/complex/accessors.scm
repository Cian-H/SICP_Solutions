;;; SICP handles this a bit differently, unwrapping here. However: this prematurely breaks the
;;; type guarantees promised by the monadic structure here. To preserve the monad, i've placed
;;; unwrap functions in the `rectangular` and `polar` modules that allow us to unwrap at the
;;; *correct* time.

(define (complex-real-part z)
  (cond
    ((complex-rectangular? z) (complex-rectangular-real-part z))
    ((complex-polar? z) (complex-polar-real-part z))
    (else (error "complex-real-part: Unknown type" z))))

(define (complex-imag-part z)
  (cond
    ((complex-rectangular? z) (complex-rectangular-imag-part z))
    ((complex-polar? z) (complex-polar-imag-part z))
    (else (error "complex-imag-part: Unknown type" z))))

(define (complex-magnitude z)
  (cond
    ((complex-rectangular? z) (complex-rectangular-magnitude z))
    ((complex-polar? z) (complex-polar-magnitude z))
    (else (error "complex-magnitude: Unknown type" z))))

(define (complex-angle z)
  (cond
    ((complex-rectangular? z) (complex-rectangular-angle z))
    ((complex-polar? z) (complex-polar-angle z))
    (else (error "complex-angle: Unknown type" z))))
