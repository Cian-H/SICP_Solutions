(define (even? n)
  (= (remainder n 2) 0))

(define (fast-mult x n)

  (define (iter b i a)
    (cond
      ((= i 0) a)
      ((even? i) (iter (+ b b) (/ i 2) a))
      (else (iter b (- i 1) (+ a b)))))

  (iter x n 0))
