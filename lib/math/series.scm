(define (filtered-accumulate acc f term a next b filter-pred)

  (define (iter a result)
    (cond
      ((> a b) result)
      ((filter-pred a) (iter (next a) (f result (term a))))
      (else (iter (next a) result))))

  (iter a acc))

(define (accumulate acc f term a next b)
  (filtered-accumulate acc f term a next b (lambda (x) true)))

(define (sum term a next b)
  (accumulate 0 + term a next b))

(define (product term a next b)
  (accumulate 1 * term a next b))
