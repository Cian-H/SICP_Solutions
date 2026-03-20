;;; Constructors
(define (complex-from-real-imag x y) ((get 'from-real-imag 'complex) x y))
(define (complex-from-mag-ang r a) ((get 'from-mag-ang 'complex) r a))
;;; Accessors
(define (complex-real-part z) (apply-generic 'real-part z))
(define (complex-imag-part z) (apply-generic 'imag-part z))
(define (complex-magnitude z) (apply-generic 'magnitude z))
(define (complex-angle z) (apply-generic 'angle z))
;;; Operations
(define (complex-add x y) (apply-generic 'add x y))
(define (complex-sub x y) (apply-generic 'sub x y))
(define (complex-mul x y) (apply-generic 'mul x y))
(define (complex-div x y) (apply-generic 'div x y))
