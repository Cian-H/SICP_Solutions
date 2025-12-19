(load "lib/math.scm")

(define (filtered-accumulate acc f term a next b filter-pred)

  (define (iter a result)
    (cond
      ((> a b) result)
      ((filter-pred a) (iter (next a) (f result (term a))))
      (else (iter (next a) result))))

  (iter a acc))

(define (sum-sq-primes a b)
  (filtered-accumulate 0 + square a inc b prime?))

(define (product-relative-primes n)
  (define (rel-prime? x)
    (= (gcd x n) 1))

  (filtered-accumulate 1 * identity 1 inc (- n 1) rel-prime?))

(display (sum-sq-primes 2 100)) ; 65796
(newline)
(display (product-relative-primes 10)) ; 189
(newline)
