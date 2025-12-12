(define (sqrt x)
  (if (< x 0)
    (error "Cannot calculate square root of negative number" x)
    (sqrt-iter (improve 1.0 x) x)))

(define (sqrt-iter guess x)
  (define next_guess (improve guess x))
  (if (>= next_guess guess)
    guess
    (sqrt-iter next_guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))
