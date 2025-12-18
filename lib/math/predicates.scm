(define (divides? a b)
  (zero? (remainder b a)))

(define (even? n)
  (divides? 2 n))

(define (prime?) n
  (= n (smallest-divisor n)))
