(define (accumulate acc f term a next b)

  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (f result (term a)))))

  (iter a acc))

(define (sum term a next b)
  (accumulate 0 + term a next b))

(define (product term a next b)
  (accumulate 1 * term a next b))
