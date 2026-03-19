(define (rational-numer x)
  (apply-generic 'numer x))

(define (rational-denom x)
  (apply-generic 'denom x))
