(define (divides? a b)
  (zero? (remainder b a)))

(define (even? n)
  (divides? 2 n))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (fermat-prime? n times)

  (define (expmod base exp m)
    (cond
      ((= exp 0) 1)
      ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
      (else (remainder (* base (expmod base (- exp 1) m)) m))))

  (define (try-it a)
    (= (expmod a n n) a))

  (define (fermat-test n)
    (try-it (+ 1 (random (- n 1)))))

  (cond
    ((zero? times) true)
    ((fermat-test n) (fermat-prime? n (- times 1)))
    (else false)))
