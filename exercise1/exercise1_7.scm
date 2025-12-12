(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (define next_guess (improve guess x))
  (if (good-enough? guess next_guess)
    guess
    (sqrt-iter next_guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? prev_guess guess)
  (< (/ (abs (- prev_guess guess)) guess) 0.001))
