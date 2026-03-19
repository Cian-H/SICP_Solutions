(define (complex-real-part z) (apply-generic 'real-part z))
(define (complex-imag-part z) (apply-generic 'imag-part z))
(define (complex-magnitude z) (apply-generic 'magnitude z))
(define (complex-angle z) (apply-generic 'angle z))
