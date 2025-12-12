(load "lib/math/arithmetic.scm")

(define (sqrt x)
  (if (< x 0)
    (error "Cannot calculate square root of negative number" x))

  (define (iter guess)
    (define next_guess (improve guess))
    (if (>= next_guess guess)
      guess
      (iter next_guess)))

  (define (improve guess)
    (average guess (/ x guess)))

  (iter (improve 1.0)))
