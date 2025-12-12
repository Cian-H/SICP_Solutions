(load "lib/math/arithmetic.scm")

(define (cubert x)

  (define (iter guess)
    (define next_guess (improve guess))
    (if (>= (abs next_guess) (abs guess))
      guess
      (iter next_guess)))

  (define (improve guess)
    (/ (+ (/ x (square guess)) guess guess) 3))

  (iter (improve 1.0)))
