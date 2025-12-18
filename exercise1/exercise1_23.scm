(load "lib/math.scm")

(define (smallest-divisor n) (find-divisor n 2))

(define (next p)
  (if (= p 2)
    3
    (+ p 2)))

(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))))
