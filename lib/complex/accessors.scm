(define (complex-real-part z) (complex-apply-generic 'real-part z))
(define (complex-imag-part z) (complex-apply-generic 'imag-part z))
(define (complex-magnitude z) (complex-apply-generic 'magnitude z))
(define (complex-angle z) (complex-apply-generic 'angle z))
