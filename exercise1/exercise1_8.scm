(define (cubert x)
  (cubert-iter (improve 1.0 x) x))

(define (cubert-iter guess x)
  (define next_guess (improve guess x))
  (if (>= (abs next_guess) (abs guess))
    guess
    (cubert-iter next_guess x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) guess guess) 3))

(define (square x)
  (* x x))
